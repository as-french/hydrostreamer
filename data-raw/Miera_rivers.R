## code to prepare `miera_rivers` dataset
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
usethis::use_data(miera_rivers,overwrite = TRUE)
