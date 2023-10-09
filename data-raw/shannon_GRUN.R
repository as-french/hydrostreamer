## code to prepare `shannon_GRUN` dataset
library(terra)
library(sf)
library(raster)

# load GRUN
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/GRUN/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

to_download = list("https://s3-eu-west-1.amazonaws.com/pfigshare-u-files/16807844/GRUN_v1_GSWP3_WGS84_05_1902_2014.nc")

for (url in (1:length(to_download))) {
  downloader::download(
    to_download[[url]],
    destfile = paste0(
      dirName,
      "GRUN_v1_GSWP3_WGS84_05_1902_2014.nc"
    ),
    mode = "wb"
  )
}

# read as raster
GRUNNc_runoff13 <- list(c(paste0(dirName, "GRUN_v1_GSWP3_WGS84_05_1902_2014.nc")))

# read file
r <- terra::rast(GRUNNc_runoff13[[1]])

#terra::time(r) # time here

# ---------------------------------------------------------------------------- #
# load subbasins to extract extent of GRUN in GRUN projection
subcatchments_region_i <- hydrostreamer::shannon_subcatchments %>%
    sf::st_cast("POLYGON") %>%
    sf::st_transform(crs = terra::crs(r)) %>%
    terra::vect()

# Define the geographical domain to be loaded add .5 degree buffer all around
i_ext = sf::st_bbox(subcatchments_region_i)
lonLim <- c(i_ext[[1]]-0.5,i_ext[[3]]+0.5)
latLim <- c(i_ext[[2]]-0.5,i_ext[[4]]+0.5)

# subset dataset to variable of interest
lcc <- r["Runoff_1"]

# define extent
e <- c(lonLim, latLim) %>%
    terra::ext() 

shannon_grun <- terra::crop(x = lcc,y = e)
# terra::time(shannon_grun) # time here

# check spatial overlap
plot(shannon_grun[[1]])
plot(subcatchments_region_i, add = TRUE)

#shannon_grun = shannon_grun * 1

# export to file
terra::writeRaster(shannon_grun,paste0(dirName, "shannon_grun.tif"),overwrite = TRUE)

shannon_GRUN = raster::brick(paste0(dirName, "shannon_grun.tif"))

shannon_GRUN = shannon_GRUN * 1

# get time
times<- data.frame("Date" = as.Date(terra::time(shannon_grun)))

shannon_GRUN <- raster::setZ(shannon_GRUN, times[,1], "Date")

usethis::use_data(shannon_GRUN, overwrite = TRUE)
