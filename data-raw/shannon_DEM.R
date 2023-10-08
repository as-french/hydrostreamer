## code to prepare `shannon_DEM` dataset
library(dplyr)
library(terra)
library(sf)
library(raster)

# load DEM
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/shannon_DEM/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# download DEM
shannon_subcatchments = hydrostreamer::shannon_subcatchments

ex_i = sf::st_union(shannon_subcatchments) %>%
    nngeo::st_remove_holes() %>%
    sf::st_cast("POLYGON")# %>%
# remove holes if needed
    
elevation_i <- elevatr::get_elev_raster(locations = ex_i, z = 10)

# prepare to crop
r <- terra::rast(elevation_i)

shannon_region = ex_i %>%
    sf::st_transform(crs = terra::crs(r)) %>%
    terra::vect()

# crop
shannon_dem <- terra::crop(r, shannon_region, mask = TRUE) %>%
    terra::project("EPSG:3035")

# export to file
terra::writeRaster(shannon_dem,paste0(dirName, "shannon_dem.tif"),overwrite = TRUE)

shannon_DEM = raster::raster(paste0(dirName, "shannon_dem.tif"))

shannon_DEM - shannon_DEM * 1

usethis::use_data(shannon_DEM, overwrite = TRUE)
