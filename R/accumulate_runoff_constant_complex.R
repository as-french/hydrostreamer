#' Apply iterative constant velocity routing to a pre-processed complex river
#' network
#' 
#' A method for constant velocity routing that takes a pre-processed HS river
#' network that contains reach specific interpolated rainfall runoff and routes
#' runoff though the network according to topologically ordered grouping.
#'
#' @param sf_river_network A LINESTRING simple feature collection containing a
#'   topologically derived grouping column that ensures each group contains no
#'   bifurcating channels; i.e., all group networks could be presented as simple
#'   directed acyclic graphs.
#' @param riverID unique edge identifier.
#' @param runoff_var_name Character string of column name for runoff time.
#' @param catchment_subgroup_var_name Character string of column name for reach velocity.
#' @param velocity_var_name Character string of column name for topologically
#'   ordered groups.
#'   series.
#' @param verbose Boolean. Print messages.
#'
#' @details With processing order established using
#'   hydrostreamer::split_network_at_bifurcations, we now route discharge. Each
#'   iteration of the loop generates routed discharge for all edges in the
#'   current group, and updates the two or more downstream reaches in the
#'   relevant group(s) into which the furthest downstream edge of the current
#'   group flows. That is, we add the reach specific runoff times series for the
#'   bifurcated channels to a fraction (usually 1/2) of the accumulated
#'   discharge at the edge immediately upstream of the bifurcation. These steps
#'   occur iteratively until all groups have been processed in topological
#'   order. See vignette for example with Miera river, which contains a single
#'   bifurcation at the spatial resolution of EU-Hydro.
#'   
#' Note that the iterative nature of this method (and the preceding step of
#' topological group identification) ensures that if there is more than one
#' upstream reach (group) flowing into a bifurcation of the next downstream
#' group, the runoff_ts of those bifurcated reaches will also be updated
#' iteratively, so the method is robust to, for example, three edges flowing
#' into two at a complex confluence (as might be expected in deltas or braids).
#' Parallelisation of this method is theoretically possible for independent
#' subgroups at the upper reaches of catchments, but is not implemented.
#'   
#' @return Returns a HS river network with discharge_ts list column
#' @export
accumulate_runoff_constant_complex <- function(sf_river_network,
                                          riverID = "riverID",
                                          runoff_var_name = "ERA5_land",
                                          velocity_var_name = "Maidment_velocity_ms",
                                          catchment_subgroup_var_name = "river_group_new",
                                          verbose = FALSE) {
    
    if (verbose == TRUE) {
        message("Split data into list in order of processing...",
                paste0("[", Sys.time(), "]"))
    }
    
    # set up list in order of processing
    bifurcation_sub_groups.processing.list <- sf_river_network %>%
        split(sf_river_network[[catchment_subgroup_var_name]])
    
    if (verbose == TRUE) {
        message("Iteratively routing...",
                paste0("[", Sys.time(), "]"))
    }
    
    n_to_process = length(bifurcation_sub_groups.processing.list)
     # system.time({
        for(i in (1:length(bifurcation_sub_groups.processing.list))){
            cat("\r","Routing group ",i," of ",n_to_process)
            
            
            # run cv routing
            bfur_subcat_group_i <- hydrostreamer::accumulate_runoff(HS = 
                bifurcation_sub_groups.processing.list[[i]],
                routing_method = "constant",
                velocity = bifurcation_sub_groups.processing.list[[i]][[velocity_var_name]],
                verbose = TRUE
            ) %>%
                dplyr::rename("NEXT_old" = "NEXT") %>%
                dplyr::rename("NEXT" = "NEXT_stored")
            
            # restore HS attributes
            bfur_subcat_group_i <-
                mod_HS_attributes(bfur_subcat_group_i,
                                                  next_col = TRUE,
                                                  col = "NEXT")

            # update original list element
            bifurcation_sub_groups.processing.list[[i]] = bfur_subcat_group_i
            
            # if not the last subgroup in the list update relevant data
            if(i < n_to_process){
                
                # extract runoff immediately upstream of bifurcation
                runoff_upstream_of_bifurcation = bfur_subcat_group_i %>%
                    dplyr::filter(.data$NEXT_old == -9999) %>%
                    # dplyr::slice_tail(n=1) %>%
                    dplyr::select(-c("runoff_ts")) %>%
                    dplyr::rename("runoff_ts"= "discharge_ts")
                
                # divide runoff and assign to each downstream bifurcation
                divide_by_n <- length(bfur_subcat_group_i %>%
                    dplyr::filter(.data$NEXT_old == -9999) %>%
                    dplyr::pull("NEXT") %>% unlist())
                
                runoff_ts <- runoff_upstream_of_bifurcation %>%
                    dplyr::pull("runoff_ts")
                
                runoff_ts_halved = lapply(1:length(runoff_ts), function(y){
                    res = runoff_ts[[y]] %>%
                        dplyr::mutate(!!runoff_var_name := (1/divide_by_n)*(.data[[runoff_var_name]])) %>%
                        dplyr::rename("runoff_upsteam_bifur" = !!runoff_var_name)
                    return(res)
                })
                
                runoff_upstream_of_bifurcation <- runoff_upstream_of_bifurcation %>%
                    dplyr::mutate("runoff_ts" = runoff_ts_halved)
                
                # get riverIDs into which runoff time series should be substituted
                riverIDs_of_bifurcated_channels_to_assign_runoff <- 
                    runoff_upstream_of_bifurcation %>%
                    dplyr::pull("NEXT") %>%
                    unlist() %>%
                    unname()
                
                # get new_river_group into which runoff time series should be substituted
                river_group_of_bifurcated_channels_to_assign_runoff <- 
                    sf_river_network %>%
                    dplyr::filter(.data$riverID %fin% riverIDs_of_bifurcated_channels_to_assign_runoff) %>%
                    dplyr::pull(!!catchment_subgroup_var_name) %>%
                    unique()
                
                # occasionally need to flow from one group into two downstream groups, so use a list
                river_group_of_bifurcated_channels_to_assign_runoff.list <- 
                    as.list(river_group_of_bifurcated_channels_to_assign_runoff)
                
                # update runoff time series in relevant downstream subgroup by
                # summing runoff_upstream_of_bifurcation with instantaneous
                # runoff at each bifurcated channel
                
                for(k in 1:length(river_group_of_bifurcated_channels_to_assign_runoff.list)){
                
                relevant_downstream_channel_data <- 
                    bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]] %>%
                    dplyr::filter(.data$riverID %fin% riverIDs_of_bifurcated_channels_to_assign_runoff)
                
                relevant_downstream_channel_data.updated <- relevant_downstream_channel_data %>%
                    split(relevant_downstream_channel_data[["riverID"]]) %>%
                    lapply(.,function(j){
                        updated_runoff <- j[["runoff_ts"]][[1]] %>%
                            dplyr::left_join(., runoff_ts_halved[[1]], by = "Date") %>%
                            dplyr::mutate(!!runoff_var_name := .data[[runoff_var_name]] + .data$runoff_upsteam_bifur) %>%
                            dplyr::select(-c("runoff_upsteam_bifur"))
                        
                        j[["runoff_ts"]] = list(updated_runoff)
                        j[["PREVIOUS"]] = list(-9999)
                        j[["UP_SEGMENTS"]] = 0
                        return(j)
                    }) %>%
                    do.call(dplyr::bind_rows,.)
                
                # bind updated runoff time series for bifurcated forks back to relevant group(s)
                # and reassign HS attributes
                
                bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]] <- 
                    bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]] %>%
                    dplyr::filter(!c(.data$riverID %fin% riverIDs_of_bifurcated_channels_to_assign_runoff)) %>%
                    dplyr::bind_rows(relevant_downstream_channel_data.updated,.)
                
                bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]] <-
                    assign_class(bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]], 
                                                 c("HS"))
                
                bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]] <-
                    mod_HS_attributes(bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]],
                                                      next_col = TRUE,
                                                      col = "NEXT")
                bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]] <-
                    mod_HS_attributes(bifurcation_sub_groups.processing.list[[river_group_of_bifurcated_channels_to_assign_runoff.list[[k]]]],
                                                      prev_col = TRUE,
                                                      col = "PREVIOUS")
                }
                }
                
        }
     # })
    # 51 minutes for 1 year of hourly data for 24751 Shannon edges split into 43
    # groups
    
    # bind together updated subgroup list to check discharge estimates
    bifurcation_sub_groups.processing.list.bind <- 
        bifurcation_sub_groups.processing.list %>%
        do.call(dplyr::bind_rows,.) %>%
        sf::st_as_sf()
    
    # restore original NEXT and PREVIOUS columns
    bifurcation_sub_groups.processing.list.bind.restored_lookups <- 
        bifurcation_sub_groups.processing.list.bind %>%
        dplyr::select(-c("NEXT","PREVIOUS")) %>%
        dplyr::left_join(., (sf_river_network %>%
                                 sf::st_drop_geometry() %>%
                                 dplyr::select("riverID","NEXT_stored", "PREVIOUS") %>%
                                 dplyr::rename("NEXT"= "NEXT_stored")), by = "riverID")
    
    return(bifurcation_sub_groups.processing.list.bind.restored_lookups)
    
}


