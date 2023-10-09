## code to prepare `basins_data` dataset
library(dplyr)
library(sf)

# load spatial geoms
data(example_basins)
st_crs(basins) <- st_crs(basins) # see https://github.com/r-spatial/sf/issues/1811
basins_data <- basins

# check classes
class(basins_data)
apply(basins_data,MARGIN = 2,function(x){class(x[[1]])})
class(basins_data$geom)

# export
usethis::use_data(basins_data,overwrite = TRUE)
