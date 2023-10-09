## code to prepare `river_data` dataset
library(dplyr)
library(sf)

# load spatial geoms
data(example_rivers)
st_crs(river) <- st_crs(river) # see https://github.com/r-spatial/sf/issues/1811
river_data <- river

# check classes
class(river_data)
apply(river_data,MARGIN = 2,function(x){class(x[[1]])})
class(river_data$geom)

# export
usethis::use_data(river_data,overwrite = TRUE)
