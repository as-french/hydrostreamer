## code to prepare `miera_rivers_split` dataset
library(dplyr)
library(sf)
library(arrow)
library(geoarrow)

# load mayo river atts and spatial geoms
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/miera_rivers/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# To do download link remote URL to attributes [TBC on Zenodo]
# To do download link remote URL to geometries [TBC on Zenodo]

miera_rivers <- arrow::open_dataset(paste0(dirName,"amber_rivers_03_03_2023.parquet")) %>%
    dplyr::filter(river_group == 1951) %>%
    dplyr::collect() %>%
    dplyr::tibble() %>%
    dplyr::mutate("geom" = sf::st_as_sfc(.data$geom)) %>%
    sf::st_as_sf() %>%
    sf::st_set_crs("EPSG:3035") %>%
    dplyr::select("riverID","river_group","region_id","lake_id","elev_gmean_m","grad","UCA_km2","geom")

# check classes
class(miera_rivers)
apply(miera_rivers,MARGIN = 2,function(x){class(x[[1]])})
class(miera_rivers$geom)

st_crs(miera_rivers)$wkt <- gsub("ü","\\u00fc", sf::st_crs(miera_rivers)$wkt)
# check https://github.com/r-spatial/sf/issues/1341
# st_crs(miera_rivers_bad) == st_crs(miera_rivers)
# miera_rivers_badtrasn = miera_rivers_bad %>%
#     sf::st_transform("EPSG:3035")
# sf::st_crs(miera_rivers_badtrasn)
# export

# ------------------------------------------------------------------------- #
# NOW IDENTIFY REACHES TO SPLIT USING QGIS
# ------------------------------------------------------------------------- #
miera_rivers <- hydrostreamer::miera_rivers
# ---------------------------------------------------------------------------- #
# VELOCITY
# ---------------------------------------------------------------------------- #
#V = Vm * (s^bA^c) / ([s^bA^c]m)  from Maidment et al., 1996
# where [...]m denotes the catchment average 

# Vm = average velocity of entire catchment
# s = slope
# A is upstream area
# b slope power coefficient 
# c area power coefficient

# Vm * ("reach specific slope-area term" / [catchment mean slope-area term])
#
# This velocity will matter more when working with hourly time steps

b = 0.5
c = 0.5
Vm = 0.1

# note we set parameters here to reflect realistic flows will range between 0.02
# and 2 m/s and are based on the original Maidment 1996 paper
# https://learn.arcgis.com/en/projects/predict-floods-with-unit-hydrographs/

shallowest_grad = min(miera_rivers$grad[miera_rivers$grad > 0])

miera_rivers_velocity = miera_rivers %>%
    dplyr::mutate("grad" = dplyr::case_when(
        grad <= 0 ~ shallowest_grad, .default = .data$grad)) %>%
    dplyr::mutate(
        "Maidment_velocity_ms" = 
            (Vm * ((.data$grad^b) * (.data$UCA_km2^c)) /
             mean((.data$grad^b) * (.data$UCA_km2^c), na.rm = TRUE)))

# ---------------------------------------------------------------------------- #
# IDENTIFY REACHES THAT REQUIRE SPLITS

riv_net_to_split_prelim = miera_rivers_velocity  %>%
    dplyr::mutate("reach_len_m" = sf::st_length(.data$geom) %>% units::drop_units()) # largest river

# To not unnecessarily increase computational demand, identify reaches that
# require splitting, split them and then bind back with any unsplit original data
# A reach that requires splitting is one that requires more than one time step
# (i.e., 3600 seconds if hourly time step) for a "unit" to pass through a
# network edge/reach, where the velocity of units are reach specific Maidment
# velocity.

riv_net_to_split <- riv_net_to_split_prelim %>%
    dplyr::mutate(
        "hydo_unit_duration" = .data$reach_len_m / .data$Maidment_velocity_ms,
        "requires_split" = .data$hydo_unit_duration > 3600,
        "required_split_length" = round((3600 * .data$Maidment_velocity_ms) -
                                            1)) %>%
    dplyr::select(
        "riverID",
        "reach_len_m",
        "river_group",
        "requires_split",
        "required_split_length",
        "Maidment_velocity_ms",
        "geom"
    ) %>%
    dplyr::filter(.data$requires_split == TRUE)


# https://github.com/r-spatial/qgisprocess/issues/26
library(qgisprocess)

# Lars Dalby's function
# https://github.com/r-spatial/sf/issues/2111#issuecomment-1458718747

split_transects <- function(transect_lines, length) {
    result <- qgisprocess::qgis_run_algorithm(
        "native:splitlinesbylength",
        INPUT = transect_lines,
        LENGTH = length,
        OUTPUT = tempfile(fileext = ".fgb"),
        .quiet = TRUE
    )
    segments <- sf::read_sf(qgisprocess::qgis_extract_output(result, "OUTPUT"))
    return(segments)
}

# split based on reach specific information and assign new riverID and bind back
# to any unsplit reaches

reaches_not_split <- riv_net_to_split_prelim %>%
    dplyr::filter(!c(.data$riverID %in% unique(riv_net_to_split$riverID))) %>%
    dplyr::rename("old_riverID" = "riverID") %>%
    dplyr::select(dplyr::any_of(c(names(riv_net_to_split),"old_riverID")))

riv_net_to_split.split <- split_transects(
    transect_lines = riv_net_to_split  %>%
    dplyr::select(-c("reach_len_m")),
    length = 'expression: "required_split_length"*1') %>%
    dplyr::rename("old_riverID" = "riverID",
                  "geom" = "geometry") %>%
    dplyr::select(-c("fid"))

miera_rivers_split <- riv_net_to_split.split %>%
    dplyr::bind_rows(., reaches_not_split
    ) %>%
    dplyr::mutate("riverID" = dplyr::row_number()) %>%
    dplyr::select(-c("reach_len_m")) %>%
    dplyr::select(
        "riverID",
        "old_riverID",
        "river_group",
        "Maidment_velocity_ms",
        "geom"
    )

# ---------------------------------------------------------------------------- #
# RECOMPUTE ATTRIBUTES

# ---------------------------------------------------------------------------- #
st_crs(miera_rivers_split)$wkt <- gsub("ü","\\u00fc", sf::st_crs(miera_rivers_split)$wkt)

usethis::use_data(miera_rivers_split,overwrite = TRUE)
