## code to prepare `runoff_data` dataset
library(sf)
library(raster)

# load LORA
r <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))

# read file
r <- raster::setMinMax(r)

# read basins
data(example_basins)
# ---------------------------------------------------------------------------- #

# check spatial overlap
plot(r[[1]])
plot(basins$geom, add = TRUE)

# export to file

runoff_data = r * 1

# set time dimension values
times <-
    data.frame("Date" = seq(
        from = as.Date("1980-01-01"),
        by = "1 month",
        length = raster::nlayers(r)
    ))

runoff_data <- raster::setZ(runoff_data, times[,1], "Date")

usethis::use_data(runoff_data, overwrite = TRUE)
