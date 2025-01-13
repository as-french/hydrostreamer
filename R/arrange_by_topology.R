#' Arrange network edges in order of topology upstream to downstream
#' 
#' A sorting method for arranging tables of sfc LINESTRINGs in topological
#' order.
#'
#' @param sf_river_network A LINESTRING simple feature collection.
#' @param riverID unique edge identifier.
#' @param verbose Boolean. Print messages.
#'
#' @return Returns the input sfc arranged by topology.
#' @examples
#' # Miera example
#' library(dplyr)
#'
#'  miera = hydrostreamer::miera_rivers %>%
#'      dplyr::mutate("default_order" = dplyr::row_number())
#' 
#'  # miera %>%
#'  # ggplot()+
#'  #    geom_sf(aes(geometry = geom, col = default_order))
#' 
#'  miera_ordered_by_topology = arrange_by_topology(miera,
#'                                                    riverID = "riverID",
#'                                                    verbose = TRUE)
#'  
#'  #miera_ordered_by_topology %>%
#'  #    ggplot()+
#'  #    geom_sf(aes(geometry = geom, col = topo_order))
#'
#' @export 
arrange_by_topology <- function(sf_river_network,
                                riverID = "riverID",
                                verbose = FALSE) {
    
    sf_river_network[["riverID"]] = sf_river_network[[riverID]]
    
    
    if(verbose == TRUE){
        message("Converting to sfnetwork...",
                paste0("[",Sys.time(),"]"))
    }
    
    sf_river_network_sfnetworks = sf_river_network %>%
        dplyr::select("riverID","geom") %>%
        sfnetworks::as_sfnetwork()
    
    if(verbose == TRUE){
        message("Extracting topological order using igraph...",
                paste0("[",Sys.time(),"]"))
    }
    
    vertices_topo_order <- sf_river_network_sfnetworks %>%
        igraph::topo_sort(.,mode = "out") %>%
        as.vector()
    
    vertices_topo_order_to_from <-
        matrix(
            ncol = 2,
            byrow = TRUE,
            data = c(
                vertices_topo_order[1],
                rep(vertices_topo_order[2:(length(vertices_topo_order) - 1)],
                    each = 2),
                vertices_topo_order[length(vertices_topo_order)]
            )
        ) %>%
        .[, 1]
    
    if(verbose == TRUE){
        message("Sorting original network table in topological order...",
                paste0("[",Sys.time(),"]"))
    }
    
    res_rivID_order = sf_river_network_sfnetworks %>%
        sfnetworks::activate("edges") %>%
        dplyr::arrange(match(.data$from,vertices_topo_order_to_from)) %>%
        dplyr::pull("riverID")
    
    sf_river_network_sfnetworks = sf_river_network %>%
        dplyr::arrange(match(.data$riverID,res_rivID_order)) %>%
        dplyr::mutate("topo_order" = dplyr::row_number()) %>%
        sf::st_as_sf()
    
    if(verbose == TRUE){
        message("Done!",
                paste0("[",Sys.time(),"]"))
    }
    
    return(sf_river_network_sfnetworks)
}

