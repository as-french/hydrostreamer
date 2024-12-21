## code to prepare `shannon_ERA5_land` dataset

# Run this line in case of a clash of proj versions with postgis installed separately,
# https://github.com/rspatial/terra/issues/1378#issuecomment-1864893650
plib <- Sys.getenv("PROJ_LIB")
prj <- system.file("proj", package = "terra")[1]
Sys.setenv("PROJ_LIB" = prj)

library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(tidyterra)


# ---------------------------------------------------------------------------- #
# DOWNLOAD ERA5-LAND data for the SHANNON CATCHMENT, IRELAND
dirName <- paste0(system.file("extdata", package = "hydrostreamer"),
                  "/ERA5/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

river_net_catchments <- hydrostreamer::shannon_rivers

subcatchments <- hydrostreamer::shannon_subcatchments

# Add a buffer to ensure region of interest is covered completely.
catchment_bbox = subcatchments %>%
    sf::st_buffer(5000) %>%
    sf::st_transform("EPSG:4326") %>%
    sf::st_bbox() %>%
    .[c(4,1,2,3)] %>%
    unname()

catch_box_sf = subcatchments %>%
    sf::st_buffer(5000) %>%
    sf::st_transform("EPSG:4326") %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf()

# check extents overlap
ggplot()+
    geom_sf(data = catch_box_sf, aes(geometry = x))+
    geom_sf(data = subcatchments %>%
                sf::st_transform("EPSG:4326"), aes(geometry = geom))+
    theme_bw()

library(ecmwfr) # install this package and follow instructions at https://github.com/bluegreen-labs/ecmwfr
ecmwfr::wf_set_key(user = "xxxxxxxxxxxxxxxxxx email",
                   key = "xxxxxxxxxxxxxxxxxxxx access token on CDS site")

catchment_name = "shannon"

list_years <- c(2023:2023)

#  variable = "runoff",
#   variable = "surface_runoff",
#   variable = "mean_surface_runoff_rate",
#   variable = "mean_runoff_rate",
#   variable = "2m_temperature",
#   variable = "total_precipitation",

month_list <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

request_urls <- pbapply::pblapply(1:length(list_years), function(i){
    
    request_urls_year_i <- pbapply::pblapply(1:12, function(j){
        
        request <- list(
            dataset_short_name = "reanalysis-era5-land",
            variable = "surface_runoff",
            year =  as.character(list_years[[i]]),
            month = month_list[[j]],
            day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
            time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
            data_format = "grib",
            download_format = "unarchived",
            area = catchment_bbox,
            target = paste0("download_",list_years[[i]],".grib")
        )
        
        req_url_j <- ecmwfr::wf_request(
            user = "*************** email",
            request = request,
            transfer = FALSE,
            path = dirName,
            verbose = TRUE,
            time_out = 20 * 3600
        )
        
        return(req_url_j)
    })
    
    return(request_urls_year_i)
    
})

# save urls
urls_of_completed_data <- lapply(1:length(request_urls), function(j){
    
    lapply(1:length(request_urls[[j]]), function(k){
        request_urls[[j]][[k]]$get_url()
    }) %>%
        unlist(recursive = TRUE)
    
}) %>%
    unlist(recursive = TRUE)

saveRDS(urls_of_completed_data,
        paste0(dirName,"",
               "surface_runoff_ERA5_land.rds"))

# ---------------------------------------------------------------------------- #
# get the data (perhaps the next day as requests can take several hours)

urls_of_completed_data <- readRDS(paste0(dirName,"",
                                         "surface_runoff_ERA5_land.rds"))

list_months <- 1:(12*length(list_years))

list_months <- split(list_months, ceiling(seq_along(list_months)/12))

# if needed
for(i in 1:length(list_years)) {
    for (j in 1:length(list_months[[i]])) {
        ecmwfr::wf_transfer(
            url = urls_of_completed_data[list_months[[i]][[j]]],
            user = "*************** email",
            path = dirName,
            filename = paste0(
                'ERA5_land_',
                catchment_name,
                "_",
                list_years[[i]],
                "_month_",
                j,
                '.grib'
            ),
            verbose = TRUE
        )
    }
    
}


# ---------------------------------------------------------------------------- #
# PRE-PROCESS DATA (DE-ACCUMULATE RUNOFF)
# Many climate data products are provided in accumulated format (e.g., daily
# accumulated precipitation). This is also the case for ERA5-Land runoff, which
# is accumulated across each 24 hour period. To work with hydrostreamer, this
# must be de-accumulated.


grib_files = list.files(
    path = dirName,
    pattern = ".grib",
    recursive = TRUE,
    full.names = TRUE
)

# read and fill gaps before aggregating to daily sum

runoff_prelim <- terra::rast(grib_files) %>%
    terra::time() %>%
    order()
# 
runoff_prelim.sort = terra::rast(grib_files) %>%
    .[[runoff_prelim]]
# 
runoff_catchment_name <- runoff_prelim.sort
rm(runoff_prelim.sort)

terra::time(runoff_catchment_name) = terra::time(runoff_catchment_name) - lubridate::minutes(1)

# remove first value
runoff_catchment_name = runoff_catchment_name[[-1]]

all_years <- lubridate::year(terra::time(runoff_catchment_name)) %>%
    unique()

# check order is correct
all(seq(from = min(terra::time(runoff_catchment_name)), 
        to = max(terra::time(runoff_catchment_name)),
        by = "hour") ==
        terra::time(runoff_catchment_name))

# subset to a year at a time
#https://gis.stackexchange.com/a/395434
ERA5time <- terra::time(runoff_catchment_name)

year_boolean_subset_list <- lapply(1:length(all_years), function(yr) {
    boolean_yr_i <- ERA5time >= as.POSIXct(paste0(all_years[[yr]], "-01-01 00:00:00")) &
        ERA5time <= as.POSIXct(paste0(all_years[[yr]], "-12-31 23:59:59"))
    return(boolean_yr_i)
})

# create subsets
runoff_subset_list_yrs <- lapply(1:length(year_boolean_subset_list), function(j){
    runoff_subset_j <- runoff_catchment_name[[year_boolean_subset_list[[j]]]]
    return(runoff_subset_j)
})

system.time({
    processed_rasters <- pbapply::pblapply(1:length(runoff_subset_list_yrs), function(k){
        
        # ----------------------------------------------------------------------- #
        #  DEACCUMLATE RASTER
        # ----------------------------------------------------------------------- #
        # read the raster into tidy format
        
        names(runoff_subset_list_yrs[[k]]) = as.character(terra::time(runoff_subset_list_yrs[[k]]))

        # note here that the first value at 00:00:00 on 1st January is the
        # cumulative runoff from the whole of the previous day 31th December.
        # Because we cannot know the value before that value until all the
        # datasets are appended, we assign it NA. However, we can either, at the
        # end of this processing interpolate the NA across raster layers, or
        # before processing, we can ensure this value is assigned to the previous
        # year by adjusting data times back 1 minute.
        
        # tidyterra MUST BE LOADED FOR THE AS_TIBBLE METHOD TO WORK
        
        runoff_subset.tibble = runoff_subset_list_yrs[[k]] %>%
            as_tibble(xy = TRUE, cell = TRUE,.name_repair = "check_unique") %>%
            setNames(., nm = c("cell","x","y",as.character(terra::time(runoff_subset_list_yrs[[k]])))) %>%
            tidyr::pivot_longer(cols = !1:3,
                                names_to = "time",values_to = "accum_runoff") %>%
            dplyr::mutate("time" = lubridate::as_datetime(.data$time),
                          "day24h" = lubridate::date(.data$time)) %>%
            dplyr::group_by(.data$day24h,.data$cell) %>%
            dplyr::mutate("deaccum_runoff" = .data$accum_runoff - dplyr::lag(.data$accum_runoff)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate("deaccum_runoff" = dplyr::case_when(
                is.na(.data$deaccum_runoff) ~ .data$accum_runoff,
                .default =  .data$deaccum_runoff
            )) %>%
            dplyr::select("x","y","time","deaccum_runoff")
        
        # convert back to raster xylz
        system.time({
            new_raster = terra::rast(runoff_subset.tibble, 
                                     type = "xylz", 
                                     crs = "EPSG:4326")
        })
        # 2 minutes per year of data
        
        names(new_raster) <- runoff_subset.tibble %>%
            dplyr::group_by(x,y) %>%
            dplyr::filter(dplyr::cur_group_id() == 1) %>%
            dplyr::pull("time") %>%
            as.character()
        
        terra::time(new_raster) <- runoff_subset.tibble %>%
            dplyr::group_by(x,y) %>%
            dplyr::filter(dplyr::cur_group_id() == 1) %>%
            dplyr::pull("time")
        
        # ------------------------------------------------------------------------#
        # FILL NAS AND CROP TO STUDY EXTENT
        
        runoff_subset_NA_filled <- terra::focal(
            new_raster,
            w = 3,
            fun = mean,
            na.policy = "only",
            na.rm = TRUE
        )
        
        
        # crop to spatial extent of subcatchments
        
        new_raster_crop <- runoff_subset_NA_filled %>%
            terra::project("EPSG:3035") %>%
            terra::crop(., subcatchments %>%
                            sf::st_bbox() %>%
                            sf::st_as_sfc(), snap = "out") %>%
            terra::crop(., subcatchments, mask = TRUE, snap = "out")
        
        # ----------------------------------------------------------------------- #
        # PLOT RESULTS
        # ----------------------------------------------------------------------- #
        # time series for a cell
        deaccumulated_raster.tibble = new_raster %>%
            as_tibble(xy = TRUE, cell = TRUE) %>%
            tidyr::pivot_longer(cols = 4:(ncol(.)),
                                names_to = "time",
                                values_to = "deaccum_runoff") %>%
            dplyr::mutate("time" = lubridate::as_datetime(.data$time))
        
        # # plot deacc data
        # deaccumulated_raster.tibble %>%
        #   dplyr::filter(.data$cell == 1) %>%
        #   ggplot() +
        #   geom_line(aes(x = time, y = deaccum_runoff))
        
        # ----------------------------------------------------------------------- #
        # time series for a cell
        original_raster.tibble = runoff_subset_list_yrs[[k]] %>%
            as_tibble(xy = TRUE, cell = TRUE) %>%
            tidyr::pivot_longer(cols = 4:(ncol(.)),
                                names_to = "time",
                                values_to = "accum_runoff") %>%
            dplyr::mutate("time" = lubridate::as_datetime(.data$time))
        
        # # plot original accu data
        # original_raster.tibble %>%
        #   dplyr::filter(.data$cell == 1) %>%
        #   ggplot() +
        #   geom_line(aes(x = time, y = accum_runoff))
        
        # ----------------------------------------------------------------------- #
        
        # export new raster as netcdf ready for routing
        new_raster_signif <- round(new_raster_crop,8)
        
        names(new_raster_signif) <- terra::time(new_raster_signif) + lubridate::minutes(1)
        terra::time(new_raster_signif) <- terra::time(new_raster_signif) + lubridate::minutes(1)
        
        year_range = range(terra::time(new_raster_signif)) %>%
            lubridate::year() %>%
            paste0(., collapse = "_")
        
        # write deaccumulated hourly data
        terra::writeCDF(
            x = new_raster_signif,
            filename = paste0(dirName, "surface_runoff_hourly_8dp_",
                              catchment_name, "_", year_range, ".nc"),
            overwrite = TRUE
        )
        
        # write original data but collated to full years as nc
        terra::writeCDF(
            x = runoff_subset_list_yrs[[k]],
            filename = paste0(dirName, "accum_surface_runoff_hourly_original_",
                              catchment_name, "_", year_range, ".nc"),
            overwrite = TRUE
        )
        
        # aggregate to daily sum
        runoff_catchment_name_daily = new_raster_signif %>%
            terra::tapp(.,index = "days", fun = \(y){
                sum(y, na.rm = TRUE)
            })
        
        terra::writeCDF(
            runoff_catchment_name_daily,
            paste0(dirName, "surface_runoff_daily_8dp_",
                   catchment_name, "_", year_range, ".nc"),
            overwrite = TRUE
        )
        
        return(new_raster_signif)
    })
    
    
    # # check catchment boundary lines up with grid
    # ggplot()+
    #     geom_spatraster(data = new_raster_crop[[3]])+
    #     geom_sf(data = subcatchments, aes(geometry = geom))+
    #     theme_bw()
    
})

# # collate multiple years into single ncdf if required
# collated_rasters <- terra::rast(processed_rasters)
# 
# year_range = range(terra::time(collated_rasters)) %>%
#     lubridate::year() %>%
#     paste0(., collapse = "_")
# 
# system.time({
#     terra::writeCDF(
#         collated_rasters,
#         paste0(dirName, "surface_runoff_hourly_8dp_",
#           catchment_name, "_", year_range, ".nc"),
#         overwrite = FALSE
#     )
# })



# ---------------------------------------------------------------------------- #


# read as raster
ERA5_Land_NC_runoff13 <- list(c(paste0(dirName,"surface_runoff_hourly_8dp_",
                                       catchment_name, "_", year_range, ".nc")))

# read file
r <- terra::rast(ERA5_Land_NC_runoff13[[1]])

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
runoff <- r[terra::varnames(r)]

times <- data.frame("Date" = lubridate::as_datetime(terra::time(r)))

# define extent
e <- c(lonLim, latLim) %>%
    terra::ext() 

shannon_ERA5 <- terra::crop(x = runoff,y = e, snap = "out")
# terra::time(shannon_ERA5) # time here

# check spatial overlap
plot(shannon_ERA5[[1]])
plot(subcatchments_region_i, add = TRUE)

#shannon_ERA5 = shannon_ERA5 * 1

# export to file
terra::writeRaster(shannon_ERA5, paste0(dirName, "shannon_ERA5.tif"),
                   overwrite = TRUE)

shannon_ERA5 = raster::brick(paste0(dirName, "shannon_ERA5.tif"))

shannon_ERA5 = shannon_ERA5 * 1

# get time
#times<- data.frame("Date" = lubridate::as_datetime(terra::time(shannon_ERA5)))

shannon_ERA5_land <- raster::setZ(shannon_ERA5, times[,1], "Date")
names(shannon_ERA5_land) <- times[,1]

usethis::use_data(shannon_ERA5_land, overwrite = TRUE)
