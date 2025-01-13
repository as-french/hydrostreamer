#' @title Assign grouping factor id to each set of interconnected reaches
#'
#' @description This function receives a directed sf LINESTRING that describes a
#'   river network and returns an additional column with unique grouping id.
#'
#' @param sf_riv_net An sf LINESTRING object.
#' @param verbose Boolean. If TRUE, returns messages during processing.
#'
#' @details This function is based on igraph solution proposed by Barry
#'   Rowlingson (Follow URL below for details). The original solution used
#'   \emph{sf}, whereas the implemented solution uses \emph{geos}.
#'
#' @source \url{https://gis.stackexchange.com/a/310465}
#'
#' @return An sf LINESTRING object with one additional column.
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' set.seed(1234)
#' sf_riv_net = hydrostreamer::miera_rivers %>%
#'     dplyr::filter(
#'         !c(.data$riverID %in% sample(hydrostreamer::miera_rivers$riverID, 
#'                                                size = 1)))
#' sf_riv_net.grouped <- group_rivers(sf_riv_net,
#'                                             verbose = TRUE)
#'
#' \dontrun{
#' library(ggplot2)
#' sf_riv_net.grouped %>%
#'  ggplot()+
#'  geom_sf(aes(geometry = geom, col = factor(river_group)),show.legend = FALSE) +
#'  theme_bw()
#'}
#' @export
group_rivers <- function(sf_riv_net, verbose = FALSE){
    
    if (verbose == TRUE) {
        message("Identifying all reach interconnections...",
                paste0("[", Sys.time(), "]"))
    }
    #line.touches <- sf::st_touches(sf_riv_net)
    
    # try geos solution (faster, but not necessarily fair comparison as sf uses
    # spherical geometry by default), which is more complex than planar geometry
    # assumption of geos
    line.touches.geos = geos::geos_touches_matrix(
        geom = geos::as_geos_geometry(sf_riv_net),
        tree = geos::geos_strtree(geos::as_geos_geometry(sf_riv_net))
    )
    
    
    if (verbose == TRUE) {
        message("Building adjacency list...",
                paste0("[", Sys.time(), "]"))
    }
    
    g_geos = igraph::graph_from_adj_list(line.touches.geos)
    
    if (verbose == TRUE) {
        message("Extracting groups...",
                paste0("[", Sys.time(), "]"))
    }
    
    c_geos = igraph::components(g_geos)
    
    sf_riv_net$river_group = c_geos$membership
    
    if (verbose == TRUE) {
        message("Done!",
                paste0("[", Sys.time(), "]"))
    }
    
    return(sf_riv_net)
}
