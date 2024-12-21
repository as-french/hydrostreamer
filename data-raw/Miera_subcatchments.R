## code to prepare `miera_subcatchments` dataset
library(dplyr)
library(sf)
library(arrow)
library(geoarrow)

# load spatial geoms
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/miera_subcatchments/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# To do download link remote URL to sfs (TBC on Zenodo)

miera_subcatchments <- arrow::open_dataset(paste0(dirName,"amber_rivers_03_03_2023.parquet")) %>%
    dplyr::filter(river_group == 1951) %>%
    dplyr::collect() %>%
    dplyr::tibble() %>%
    dplyr::mutate("geom" = sf::st_as_sfc(.data$geom)) %>%
    sf::st_as_sf() %>%
    sf::st_set_crs("EPSG:3035") %>%
    dplyr::select("riverID","manning_n","region_id","river_group","elev_m","geom")

# check classes
class(miera_subcatchments)
apply(miera_subcatchments,MARGIN = 2,function(x){class(x[[1]])})
class(miera_subcatchments$geom)

st_crs(miera_subcatchments)$wkt <- gsub("ü","\\u00fc", sf::st_crs(miera_subcatchments)$wkt)

# export
usethis::use_data(miera_subcatchments,overwrite = TRUE)
