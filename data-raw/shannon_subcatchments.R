## code to prepare `shannon_subcatchments` dataset
library(dplyr)
library(sf)
library(arrow)
library(geoarrow)

# load spatial geoms
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/shannon_subcatchments/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# To do download link remote URL to sfs

# load vars to join
ireland_subcatch = geoarrow::read_geoparquet_sf(paste0(dirName,"amber_subcatchments_07_03_2023.parquet"))

# extract Shannon
shannon_subcatchments = ireland_subcatch %>%
    dplyr::filter(river_group == 1528)

# check classes
class(shannon_subcatchments)
apply(shannon_subcatchments,MARGIN = 2,function(x){class(x[[1]])})
class(shannon_subcatchments$geom)

# export
usethis::use_data(shannon_subcatchments,overwrite = TRUE)
