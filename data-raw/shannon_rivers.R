## code to prepare `shannon_rivers` dataset
library(dplyr)
library(sf)
library(arrow)
library(geoarrow)

# load mayo river atts and spatial geoms
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/shannon_rivers/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# To do download link remote URL to attributes
# To do download link remote URL to geometries

# load vars to join
sf_riv_net = geoarrow::read_geoparquet_sf(paste0(dirName,"amber_simplified_rivers_03_03_2023.parquet"))
sf_riv_net_vars = arrow::read_parquet(paste0(dirName,"amber_river_atts_2023_03_02_names.parquet"))

# join
sf_riv_net.att_join = dplyr::left_join(sf_riv_net,
                                       sf_riv_net_vars,
                                       by = "riverID")


# filter to Shannon
shannon_rivers = sf_riv_net.att_join %>%
    dplyr::filter(river_group == 1528) %>%
    dplyr::select(riverID,river_group,region_id,lake_id,gmean_m,grad,geom)

# check classes
class(shannon_rivers)
apply(shannon_rivers,MARGIN = 2,function(x){class(x[[1]])})
class(shannon_rivers$geom)

# export
usethis::use_data(shannon_rivers,overwrite = TRUE)
