#' Apply constant velocity river routing
#' 
#' Implements a simple constant velocity routing algorithm which can be used 
#' with arbitrary timesteps.
#'
#' @param velocity Flow velocity. Can be a constant, or a vector of flow 
#'   velocity at each unique river segments. Defaults to 1 meter per second.
#' @inheritParams accumulate_runoff_instant
#'
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#' 
#' @export 
accumulate_runoff_constant <- function(HS, 
                                       velocity = 1,
                                       verbose=FALSE) {
    
    riverID <- NULL
    NEXT <- NULL
    PREVIOUS <- NULL
    UP_SEGMENTS <- NULL
    
    route <- "forward" # the only option currently available
    
    test <- inherits(HS, "HS")
    if(!test) stop("HS must be of class HS")
    
    ###########################
    # PREPROCESSING
    
    if(verbose) message("Preparing...")
    
    # routing does not work for river networks of 1 segment: duplicate and
    # remove duplicate at the end
    ind <- find_attribute(HS, "next_col", TRUE)
    test <- nrow(HS) == 1
    if(test) {
        single_segment <- TRUE
        HS <- dplyr::bind_rows(HS, HS)
        HS$riverID[2] <- -9999
    } else {
        single_segment <- FALSE
    }
    
    
    
    #prepare required variables
    lengths <- sf::st_length(HS) %>% units::drop_units()
    IDs <- dplyr::pull(HS, riverID)
    
    order <- HS %>%
        dplyr::select("riverID", "UP_SEGMENTS") %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::arrange(.data$UP_SEGMENTS) %>%
        dplyr::select("riverID") %>%
        unlist() %>%
        match(IDs)
    
    ## find next river
    #ind <- find_attribute(HS, "next_col", TRUE)
    nextriver <- dplyr::pull(HS, ind) %>%
        match(IDs)
    
    duration <- lengths/velocity
    
    # timeseries
    flow <- HS$runoff_ts
    names(flow) <- HS$riverID
    unit <- units::deparse_unit(dplyr::pull(flow[[1]], 2))
    nseg <- length(order)
    preds <- ncol(flow[[1]])-1
    
    # get runoff series name to pass to routed discharge output
    runoff_series_name <- names(HS$runoff_ts[[1]])[2]

    # dates and time intervals between dates
    # This can be slow for long time series
    # lubridate::as_date() does not work for POSIXct dates so changed to as_datetime
    dates <- lapply(flow, function(x) x$Date) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        lubridate::as_datetime()
    
    
    intervals <- vector("numeric", length(dates))
    for (i in seq_along(intervals)) {
        if (i == length(intervals)) {
            intervals[i] <- lubridate::interval(dates[i],
                                                lubridate::ceiling_date(dates[i],
                                                                        unit="month")) / 
                lubridate::seconds(1)
        } else {
            intervals[i] <- lubridate::interval(dates[i],
                                                dates[i+1]) / lubridate::seconds(1)
        }
    }
    
    # test intervals
    test <- max(duration) > min(intervals)
        maxlen <- min(intervals)*max(velocity)
    if(test) stop("Constant routing does not currently support river ",
                  "segments longer than through which water passes ",
                  "during a single timestep. Consider breaking the ",
                  "longest segments into smaller pieces. With the ",
                  "current velocity, maximum segment length is ", 
                  maxlen, " meters.")
    
    # ------------------------------------------------------------------------ #
    # inspect downstream for each segment noting this is not computationally
    # efficient to define a list that is likely to be too long, but it is
    # probably more efficient than appending new list items with each iteration.
    # ------------------------------------------------------------------------ #
    
    downstream <- vector(mode = "list", length = 99999)
    
    temp <- dplyr::select(HS, riverID, NEXT, PREVIOUS) %>%
        tibble::add_column("duration" = duration) %>%
        sf::st_drop_geometry()
    
    # ------------------------------------------------------------------------ #
    # the original code using the downstream() function only works if there is
    # no pseudonode in the network. For now, we assume pseudonodes are
    # omnipresent, because it is not yet clear how to change the
    # hydrostreamer:::downstream function to cope with this.
    # ------------------------------------------------------------------------ #
    
    contains_pseudo_nodes = TRUE 
    
    if(all(lengths(temp$NEXT) == 1) & contains_pseudo_nodes == FALSE){
    
    for(i in order) {
        downstream[[i]] <- downstream(temp, temp$riverID[i]) %>%
            dplyr::mutate("cumulative" = cumsum(duration))
    }
        downstream.list <- list(downstream[lengths(downstream) != 0])
    
    # ------------------------------------------------------------------------ #
    # Added method for anabranching rivers
    } else if(any(lengths(temp$NEXT) > 1) | contains_pseudo_nodes == TRUE){
        temp_with_geom <- HS %>%
            dplyr::select("riverID", "NEXT", "PREVIOUS") %>%
            tibble::add_column("duration" = duration)
        
        # or with pblapply to check progress
        
        temp_with_geom_sfnetworks = temp_with_geom %>%
            dplyr::rowwise() %>%
            dplyr::mutate("NEXT" = paste0(.data$NEXT,collapse = " "),
                          "PREVIOUS" = paste0(.data$PREVIOUS,collapse = " ")) %>%
            dplyr::ungroup() %>%
            dplyr::mutate("NEXT" = dplyr::case_when(
                .data$NEXT == -9999 ~ "",
                .default = .data$NEXT
            ), "PREVIOUS" = dplyr::case_when(
                .data$PREVIOUS == -9999 ~ "",
                .default = .data$PREVIOUS
            )) %>%
            sfnetworks::as_sfnetwork()
        
        
        # ----------------------------------------- #
        # order topologically first
        
        vertices_topo_order <- temp_with_geom_sfnetworks %>%
            igraph::topo_sort(.,mode = "out") %>%
            as.vector()
        
        vertices_topo_order_to_from =
            matrix(
                ncol = 2,
                byrow = TRUE,
                data = c(
                    vertices_topo_order[1],
                    rep(vertices_topo_order[2:(length(vertices_topo_order) - 1)], each = 2),
                    vertices_topo_order[length(vertices_topo_order)]
                )
            ) %>%
            .[, 1]
        
        # now reorder result table in topological order
        res_rivID_order = temp_with_geom_sfnetworks %>%
            sfnetworks::activate("edges") %>%
            dplyr::arrange(match(.data$from,vertices_topo_order_to_from)) %>%
            dplyr::pull("riverID")
        
        temp_with_geom_sfnetworks = temp_with_geom_sfnetworks %>%
            sfnetworks::activate("edges") %>%
            dplyr::arrange(match(.data$riverID,res_rivID_order))
        
        # ----------------------------------------- #
        
        temp_with_geom <- temp_with_geom_sfnetworks %>%
            sfnetworks::activate("edges") %>%
            sf::st_as_sf() %>%
            sf::st_drop_geometry() %>%
            data.table::as.data.table()
        
        # HSnetwork_sfnetworks = temp_with_geom_sfnetworks
        # HSnetwork = temp_with_geom
        # ID =  temp_with_geom$riverID[i]
        # verbose = FALSE
        
        downstream <- lapply(order, function(i) {
       #      cat("\r",i)
            downstream[[i]] <- all_downstream_routes_to_sea(HSnetwork_sfnetworks = temp_with_geom_sfnetworks,
                                                            HSnetwork = temp_with_geom,
                                                            ID =  temp_with_geom$riverID[i],
                                                            verbose = FALSE) %>%
                purrr::map(~dplyr::mutate(.,"cumulative" = cumsum(duration)))
            })
        
        # remove any empty elements
        downstream <- downstream[lengths(downstream) != 0]
    
    # ------------------------------------------------------------------------ #
    # To ensure the original hydrostreamer routing algorithm is compatible with
    # the anabranching network, we need to feed a single route to sea from each
    # reach. But, to avoid losing information (because multiple routes may exist
    # in the case of anabranching channels), we must define alternative "simple"
    # dentritic networks and loop the algorithm over all of them. When this loop
    # is completed, we can merge the estimates of each list (e.g., averaging
    # predictions at the riverID and date level) to obtain estimates for all
    # reaches in the anabranching network. This is not necessarily a
    # computationally efficient or accurate solution, but nevertheless
    # facilitates estimation of routed discharge through a complex network using
    # the constant flow velocity algorithm.
    # ------------------------------------------------------------------------ #
        
    max_number_of_alternate_routes_to_sea <- max(lengths(downstream))
    
    # ------------------------------------------------------------------------ #
    # Create table/list of several (but not necessarily all) permutations noting
    # that not all routes can coexist, e.g., because some routes must flow to
    # one outlet and other must flow to another. Ideally, we would have a method
    # to identify unique possible networks based on the unique flow paths of
    # individual reaches to sea entry, but for now, each table/list fed into the
    # loop will contain the same set of reaches, but with routes to sea varying
    # between tables/list elements.
    #
    # For example, if the reach within the network with the most possible routes
    # to sea has 4 potential routes to sea, we run the algorithm 4 times, with
    # that reach's route to sea different each time, but routes from some
    # reaches (i.e., those with only one path to sea) identical between
    # tables/list elements. As said, this is probably computationally
    # inefficient, but is a constructive step that facilitates use of the
    # algorithm which would otherwise be unavailable for complex rivers.
    # ------------------------------------------------------------------------ #

    # all available routes but not all permutations
    all_available_routes_at_least_once = rep(1, times = length(downstream)) %>%
        cbind(lengths(downstream)) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        dplyr::rename("first" = 1,
                     "tot_n_routes" = 2) %>%
        dplyr::rowwise() %>%
        dplyr::mutate("seq_unique" = list(seq(from = .data$first,
                                         to = .data$tot_n_routes,
                                         by = 1))) %>%
        tidyr::unnest_wider("seq_unique",
                            names_sep = "_") %>%
        dplyr::mutate(
            dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 1))
        ) %>%
        dplyr::select(3:ncol(.))
    

    downstream.list = lapply(1:max_number_of_alternate_routes_to_sea,
                             function(nmax){
                                 lapply(1:length(downstream),function(n_reaches){
                                     downstream[[n_reaches]][[all_available_routes_at_least_once[[n_reaches,nmax]]]]
                                 })
                             })
    
    }
    
    output.list <- lapply(downstream.list, function(downstream){

    # ------------------------------------------------------------------------ #
    
    downstreamind <- lapply(downstream, function(x) match(x$riverID, 
                                                          names(flow)))
    
    # determine size of padding needed
    longest_duration <- max(unlist(sapply(downstream, 
                                   function(x) x$cumulative)))
    pad_n <- ceiling(longest_duration / min(intervals)) +1
    
    # dates with padding
    dates_padded <- c(rep(NA, pad_n), dates, rep(NA, pad_n))
    

    
    # process control timeseries?
    test <- hasName(HS, "control_ts")
    if(test) {
        boundary_runoff <- unname(which(sapply(HS$control_type, function(x) {
            if(is.null(x)) {
                return(FALSE)
            } else {
                return(x[2] == "runoff")
            }
        })))
        if(length(boundary_runoff) == 0) {
            rboundary <- FALSE
        } else rboundary <- TRUE
        boundary_discharge <- unname(which(sapply(HS$control_type, function(x) {
            if(is.null(x)) {
                return(FALSE)
            } else {
                return(x[2] == "discharge")
            }
        })))
        if(length(boundary_discharge) == 0) {
            dboundary <- FALSE
        } else dboundary <- TRUE
    } else {
        rboundary <- FALSE
        dboundary <- FALSE
    }
    
    
    
    ################################
    # ROUTE
    # create array, and pad and populate it
    nts <- nrow(flow[[1]])+pad_n
    inflowmat <- outflowmat <- array(0,dim=c(nts+pad_n,nseg,preds))
    
    for(seg in 1:nseg) {
        Qin <- as.matrix(flow[[seg]][,-1])
        Qin <- rbind(matrix(rep(Qin[1,],pad_n), ncol=preds, byrow=TRUE,
                            dimnames = list(NULL,colnames(Qin))),
                     Qin,
                     matrix(rep(Qin[nrow(Qin),], pad_n), ncol=preds, byrow=TRUE,
                            dimnames = list(NULL,colnames(Qin))))
        
        # apply boundary for runoff
        if(rboundary) {
            test <- seg %in% boundary_runoff
            if(test) {
                type <- HS$control_type[[seg]][1]
            
                inds <- dates_padded %in% HS$control_ts[[seg]]$Date
                cts <- units::drop_units(dplyr::pull(HS$control_ts[[seg]], 2))
                
                if(type == "add") {
                    Qin[inds,] <- Qin[inds,] + cts
                } else if(type == "subtract") {
                    Qin[inds,] <- Qin[inds,] - cts
                } else if(type == "multiply") {
                    Qin[inds,] <- Qin[inds,] * cts
                } else if(type == "set") {
                    Qin[inds,] <- 0
                    Qin[inds,] <- Qin[inds,] + cts
                } 
            }
        }
            
        inflowmat[,seg,] <- Qin
    }
    inflownas <- is.na(inflowmat)
    inflowmat[inflownas] <- 0
    
    #pad also intervals
    interval <- c(rep(intervals[[1]], pad_n), intervals)
    
    # --------------------------------------------------------------------------
    # ROUTE FORWARD
    if(verbose) message("Routing...")
    max <- preds
    if (verbose) pb <- txtProgressBar(min = 0, max = max, style = 3)
    prog <- 0
    
    
    if(route == "forward") {
        
        # ----------------------------------------------------------------------
        # record contribution
       
        record <- record_forward(downstream, 
                                 downstreamind, 
                                 interval)
        
        # ----------------------------------------------------------------------
        # route
        # tsinds <- (pad_n+1):(nrow(inflowmat)-pad_n)
        for(pred in 1:preds) {
            
            outflowmat[,,pred] <- constantroute(inflowmat[,,pred], record, 
                                                pad_n, nseg)

            if(verbose) setTxtProgressBar(pb, pred)
        }
    }
    outflowmat[inflownas] <- NA
    
    # apply discharge boundary
    if(dboundary) {
        for(seg in boundary_discharge) {
            type <- HS$control_type[[seg]][1]
            
            inds <- which(dates_padded %in% HS$control_ts[[seg]]$Date)-pad_n
            cts <- units::drop_units(dplyr::pull(HS$control_ts[[seg]], 2))
            
            if(type == "add") {
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] + cts
            } else if(type == "subtract") {
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] - cts
            } else if(type == "multiply") {
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] * cts
            } else if(type == "set") {
                outflowmat[inds,seg,] <- 0
                outflowmat[inds,seg,] <- outflowmat[inds,seg,] + cts
            } else {
                stop("Error in boundary operation type.")
            }
        }
    }
    
    if(verbose) close(pb)
    
    # --------------------------------------------------------------------------
    # ROUTE REVERSE
    # NOT IMPLEMENTED YET
    if(route == "reverse") {

    }
    
    
    # --------------------------------------------------------------------------
    # prepare output
    
    outflowmat <- units::as_units(outflowmat, "m3 s-1")
    for(seg in 1:nseg) {
        out <- flow[[seg]]
        nas <- is.na(out)
        #out[,-1] <- outflowmat[(pad_n+1):(tsteps),seg,]
        for(i in 1:preds) {
            out[,i+1] <- outflowmat[1:nrow(out), seg, i]
        }
        out[nas] <- NA
        out <- dplyr::as_tibble(out, .name_repair = "minimal")
        flow[[seg]] <- out
    }
    
    output <- HS 
    output$discharge_ts <- flow
    
    if(single_segment) output <- output[1,]
    
    output <- reorder_cols(output)
    output <- assign_class(output, "HS")
    
    ##
    return(output)
    })
      
    # need to calculate averages from each alternative route set
    # extract all time series
    # Note - if using accumulate_runoff_constant_complex(), length(output.list)
    # will be 1, so the code below generates no new information. As such, the
    # following code is only strictly necessary for the computationally
    # inefficient method, whereby a river system has not been grouped into
    # topologically distinct subgroups. That is, the inefficient method routes
    # runoff for all possible edge paths to all outlets from all reaches, before
    # calculating reach average discharge using the code below.
      output.list.timeseries = lapply(1:length(output.list), function(listid){
          output.list[[listid]] %>%
              sf::st_drop_geometry() %>%
              dplyr::select("riverID","discharge_ts") %>%
              tidyr::unnest(cols = "discharge_ts")
      }) %>%
          purrr::reduce(.,
                        dplyr::left_join,
                        by = c("riverID", "Date")) %>%
          dplyr::mutate("mean_discharge_m3_s" = dplyr::select(.,-c(1:2)) %>%
                            rowMeans() %>% units::set_units("m3 s-1")) %>%
          dplyr::select("riverID","Date","mean_discharge_m3_s") %>%
          tibble::as_tibble(.name_repair = "minimal") %>%
          dplyr::rename("{runoff_series_name}" := "mean_discharge_m3_s") %>%
          tidyr::nest(.by = "riverID",.key = "discharge_ts") %>%
          dplyr::left_join(output.list[[1]] %>%
                               dplyr::select(-c("discharge_ts")), by = "riverID") %>%
          dplyr::relocate("discharge_ts",.after = "runoff_ts")
      
      
      
    ##
      return(output.list.timeseries)
    
   # return(output)
}










record_forward <- function(downstream, downstreamind, interval) {
    
    n <- length(downstream)
    record <- lapply(1:n, function(x) {
        list(shares = NULL,
             tsteps = NULL,
             segment = NULL)
    })
    interval <- mean(interval)
    for(seg in 1:n) {
        
        
        # get indices of downstream segments
        ds <- downstreamind[[seg]]
        
        # inflowing runoff at segment seg, timestep ts
        q <- 1
        
        # cumulative time through all downstream segments standardized by
        # the interval between timesteps
        cumulative <- downstream[[seg]]$cumulative / interval
        tf <- floor(cumulative)
        tfuni <- unique(tf)
        
        # Identify last segment water flows through in each segment
        last <- c(0, cumsum(rle(tf)$lengths))
        
        outflow <- matrix(0,
                          nrow = length(tfuni)+1,
                          ncol = length(cumulative))
        
        for(i in 2:length(last)) {
            # first and last segment
            f <- last[i-1]+1
            l <- last[i]
            
            # outflow = q * how long it takes to pass through during
            # the timestep
            outflow[i-1,f:l] <- q*(1-(cumulative[f:l]-tf[f:l]))
            
            # flow that did not flow through the segments during,
            # the timestep, flows through on the next tstep (?)
            outflow[i,f:l] <- q-outflow[i-1,f:l]
        }
        
        for(i in 1:ncol(outflow)) {
            shares <- outflow[,i]
            tsteps <- which(shares != 0)
            shares <- shares[tsteps]
            segment <- ds[i]
            rec <- record[[segment]]
            rec$shares <- c(rec$shares, rev(shares))
            rec$tsteps <- c(rec$tsteps, as.integer(rev(tsteps-1)))
            rec$segment <- c(rec$segment, rep(seg, length(shares)))
            record[[segment]] <- rec
        }
    }
    return(record)
}

record_reverse <- function(downstream, downstreamind, interval, inflowmat) {
    
    nseg <- NULL
    
    record <- lapply(1:nseg, function(x) {
        list(shares = NULL,
             tsteps = NULL,
             segment = NULL)
    })
    interval <- mean(interval)
    for(seg in 1:nseg) {
        
        
        # get indices of downstream segments
        ds <- downstreamind[[seg]]
        
        # inflowing runoff at segment seg, timestep ts
        q <- 1
        
        # cumulative time through all downstream segments standardized by
        # the interval between timesteps
        cumulative <- downstream[[seg]]$cumulative / interval
        tf <- floor(cumulative)
        tfuni <- unique(tf)
        
        # Identify last segment water flows through in each segment
        last <- c(0, cumsum(rle(tf)$lengths))
        
        outflow <- matrix(0,
                          nrow = length(tfuni)+1,
                          ncol = length(cumulative))
        
        for(i in 2:length(last)) {
            # first and last segment
            f <- last[i-1]+1
            l <- last[i]
            
            # outflow = q * how long it takes to pass through during
            # the timestep
            outflow[i-1,f:l] <- q*(1-(cumulative[f:l]-tf[f:l]))
            
            # flow that did not flow through the segments during,
            # the timestep, flows through on the next tstep (?)
            outflow[i,f:l] <- q-outflow[i-1,f:l]
        }
        
        for(i in 1:ncol(outflow)) {
            shares <- outflow[,i]
            tsteps <- which(shares != 0)
            shares <- shares[tsteps]
            segment <- ds[i]
            rec <- record[[segment]]
            rec$shares <- c(rec$shares, rev(shares))
            rec$tsteps <- c(rec$tsteps, as.integer(rev(tsteps-1)))
            rec$segment <- c(rec$segment, rep(seg, length(shares)))
            record[[segment]] <- rec
        }
    }
    return(record)
}





# apply_boundary <- function(control_ts, discharge, type) {
#     # check and apply controls condition
#     
#     dateind <- discharge$Date %in% control_ts$Date
#     
#     # Set, of modify input runoff of the segment
#     if (type == "set") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- control_ts[,2]
#         }
#         
#         # if no downstream segments, go to next seg
#         if(!is.na(nextriver)) {
#             new_dis <- discharge[[nextriver[seg] ]][,-1] + 
#                 discharge[,-1]
#             
#             discharge[[ nextriver[seg] ]][,-1] <- new_dis
#         }
#         
#     } else if (type == "add") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- 
#                 discharge[dateind,pred] + control_ts[,2]
#         }
#         
#     } else if (type == "subtract") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- 
#                 discharge[dateind,pred] - control_ts[,2]
#         }
#         
#     } else if (type == "multiply") {
#         for(pred in 2:ncol(discharge)) {
#             discharge[dateind,pred] <- 
#                 discharge[dateind,pred] * control_ts[,2]
#         }
#     }
#     return(discharge)
# }

