## code to prepare `miera_DEM` dataset
library(dplyr)
library(terra)
library(sf)
library(raster)

# load DEM
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/miera_DEM/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# download DEM
miera_subcatchments = hydrostreamer::miera_subcatchments

ex_i = sf::st_union(miera_subcatchments) %>%
    nngeo::st_remove_holes() %>%
    sf::st_cast("POLYGON") %>%
    sf::st_buffer(2000) %>%
    nngeo::st_remove_holes() %>%
sf::st_as_sf() %>%
    dplyr::rename("geom" = "x")# %>%
# remove holes if needed
    
elevation_i <- elevatr::get_elev_raster(locations = ex_i, z = 10)

# prepare to crop
r <- terra::rast(elevation_i)
terra::crs(r) <- "EPSG:3035"

miera_region = ex_i %>%
    sf::st_transform(crs = terra::crs(r)) %>%
    terra::vect()

# crop
miera_dem <- terra::crop(r, miera_region, mask = TRUE) %>%
    terra::project("EPSG:3035")

# export to file
terra::writeRaster(miera_dem,paste0(dirName, "miera_dem.tif"),overwrite = TRUE)

miera_DEM = raster::raster(paste0(dirName, "miera_dem.tif"))

values(miera_DEM)[values(miera_DEM) < 0] = 1

miera_DEM = miera_DEM * 1

usethis::use_data(miera_DEM, overwrite = TRUE)
