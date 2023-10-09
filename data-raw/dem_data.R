## code to prepare `dem_data` dataset
library(sf)
library(raster)

# load DEM
dem_data <- raster::brick(system.file("extdata", "dem.tif", package = "hydrostreamer")) 

# read basins
data(example_basins)
# ---------------------------------------------------------------------------- #

# check spatial overlap
plot(dem_data[[1]])
plot(basins$geom, add = TRUE)

# export to file
dem_data = dem_data * 1

usethis::use_data(dem_data, overwrite = TRUE)
