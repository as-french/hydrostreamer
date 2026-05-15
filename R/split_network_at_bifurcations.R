#' Split a HS network into spatially distinct chunks for iterative runoff
#' routing of complex anabranching digital river networks
#' 
#' A method for splitting vector river networks into spatially distinct chunks
#' for more efficient runoff routing of anabranching networks than simply
#' running hydrostreamer::accumulate_runoff().
#'
#' @param sf_river_network A LINESTRING simple feature collection.
#' @param riverID unique edge identifier.
#' @param verbose Boolean. Print messages.
#'
#' @details This function is designed to topologically order a digital
#'   river network (LINESTRING sfc), identify each downstream bifurcation point
#'   in the network and split the network at these points using a small buffer
#'   (1cm). Then the function identifies the resulting spatially distinct
#'   groups maintaining topological order.
#'   
#' @return Returns the river network with an additional three columns denoting
#'   the the spatially distinct subgroups "river_group_new", reach specific
#'   topological order "topo_order", a new NEXT column that replaces values for
#'   the most downstream segment in each new topologically ordered subgroup with
#'   -9999 for compatibility with _complex routing methods. The original NEXT
#'   column is renamed to "NEXT_stored".
#'   
#' @examples
#' # Miera example
#' library(dplyr)
#' 
#'  miera = hydrostreamer::miera_rivers %>%
#'      dplyr::mutate("default_order" = dplyr::row_number()) %>%
#'      hydrostreamer::river_network(riverID = "riverID",
#'                                   verbose = TRUE)
#' 
#'  # miera %>%
#'  # ggplot()+
#'  #    geom_sf(aes(geometry = geom, col = default_order))
#' 
#'  miera_split_at_bifurcations <-
#'      split_network_at_bifurcations(miera,
#'    riverID = "riverID",
#'    verbose = TRUE)
#'    
#' \dontrun{
#' library(ggplot2)
#' 
#' # individual edges
#' miera_split_at_bifurcations %>%
#'     ggplot()+
#'     geom_sf(aes(geometry = geom, col = topo_order)) +
#'     scale_color_gradientn(colours = c("blue","white","red"))
#' 
#' # groups
#' miera_split_at_bifurcations %>%
#'     ggplot()+
#'     geom_sf(aes(geometry = geom, col = river_group_new)) +
#'     scale_color_gradientn(colours = c("blue","white","red"))
#'}
#' @export
split_network_at_bifurcations <- function(sf_river_network,
                                riverID = "riverID",
                                verbose = FALSE) {
    
    if (verbose == TRUE) {
        message("Identifying bifurcation points...",
                paste0("[", Sys.time(), "]"))
    }
    
    # find bifurcation points based on lookups (splits into at least 2 downstream channels)
    reaches_just_upstream_of_bifurcation <- sf_river_network %>%
        dplyr::filter(lengths(.data$NEXT) > 1)
    
    # ------------------------------------------------------------------------ #
    # find additional edge case bifurcation points where two headwater segments
    # are touching at their upstream nodes. These would be missed based on
    # lookup information alone. Ideally, these would not be present in a
    # network, but perhaps make sense in the case of headwater lakes with two
    # outflows and no apparent surface inflows.
    # Filter for headwater reaches (no upstream elements)
    headwaters <- sf_river_network %>%
        dplyr::filter(lengths(.data$PREVIOUS) == 1 & sapply(.data$PREVIOUS, function(x) -9999 %in% x))
    
    # Extract the exact start points (upstream ends) of these headwater reaches
    upstream_nodes <- headwaters %>%
        sf::st_line_sample(sample = 0) %>%
        sf::st_cast("POINT")
    
    # Find which upstream nodes touch ANY other reach in the full network
    # st_intersects returns a list of indices; we look for lengths > 1 
    # (1 because it will always intersect itself)
    touching_indices <- which(lengths(sf::st_intersects(upstream_nodes, sf_river_network)) > 1)
    
    # Filter the headwater features that are touching, and create the buffers
    reaches_touching_at_upstream_end <- headwaters[touching_indices, ]
    
    # Extract the unique start points of the touching headwaters
    upstream_points <- reaches_touching_at_upstream_end %>%
        sf::st_line_sample(sample = 0) %>%
        sf::st_cast("POINT")
    
    # Find identical coordinates (each point matches itself and its touching pair)
    equal_matrix <- sf::st_equals(upstream_points, upstream_points)
    
    # Keep only the first index from each pair of matching coordinates
    unique_indices <- vapply(equal_matrix, function(x) x[1], FUN.VALUE = integer(1)) %>% 
        unique()
    
    # Generate the final single buffer per pair
    buffers_for_touching_splits <- upstream_points[unique_indices] %>%
        sf::st_buffer(0.01) %>%
        sf::st_as_sf() %>%
        dplyr::rename("geom" = "x")
    # ------------------------------------------------------------------------ #
    
    
    # split_points <- reaches_just_upstream_of_bifurcation %>%
    #     sf::st_line_sample(sample = 1) %>% sf::st_as_sf() %>%
    #     dplyr::rename("geom" = "x") %>%
    #     sf::st_cast("POINT")
    
    # sf_river_network %>%
    #     ggplot()+
    #     geom_sf(aes(geometry = geom))+
    #     geom_sf(data = (reaches_just_upstream_of_bifurcation %>%
    #                         sf::st_line_sample(sample = 1) %>% sf::st_as_sf() %>%
    #                         dplyr::rename("geom" = "x")),
    #             aes(geometry = geom), col = "red")
    
    # identify order bifurcations encountered by removing reach upstream of each
    # bifurcation and grouping
    
    if (verbose == TRUE) {
        message("Splitting network at bifurcation points...",
                paste0("[", Sys.time(), "]"))
    }
    
    # split with small buffers at edge sinks immediately upstream of bifurcation
    buffers_to_split_by <- reaches_just_upstream_of_bifurcation %>%
        sf::st_line_sample(., sample = 1) %>%
        sf::st_cast("POINT") %>%
        sf::st_buffer(0.01) %>%
        sf::st_as_sf() %>%
        dplyr::rename("geom" = "x") |>
        dplyr::bind_rows(buffers_for_touching_splits)
    
    # split lines using difference
    
    # intersecting lines
    river_lines_intersecting_buffers = sf_river_network[sapply(
        X = geos::geos_intersects_matrix(
            geos::as_geos_geometry(sf_river_network),
            geos::geos_strtree(geos::as_geos_geometry(buffers_to_split_by))
        ),
        FUN = function(x) {
            length(x) > 0
        }
    ), ]
    
    # difference lines
    river_lines_intersecting_buffers_shortened <- 
        river_lines_intersecting_buffers %>%
        sf::st_set_agr("constant") %>%
        sf::st_difference(.,buffers_to_split_by %>%
                              sf::st_union())
    
    # append back to river network and identify groups
    river_IDs_upstream_of_bifurcations <- 
        river_lines_intersecting_buffers_shortened %>%
        dplyr::pull("riverID")
    
    if (verbose == TRUE) {
        message("Identifying spatially distinct groups based on splits...",
                paste0("[", Sys.time(), "]"))
    }
    
    bifurcation_sub_groups <- sf_river_network %>%
        dplyr::filter(!c(.data$riverID %fin% river_IDs_upstream_of_bifurcations)) %>%
        dplyr::bind_rows(river_lines_intersecting_buffers_shortened) %>%
        # dplyr::filter(!c(lengths(.data$NEXT) > 1)) %>%
        dplyr::rename("subcatch_river_group" = "river_group") %>%
        group_rivers(verbose = verbose)
    
    # join new subgroups to original geometries and spit
    bifurcation_sub_groups.list.upd <- sf_river_network %>%
        dplyr::rename("subcatch_river_group" = "river_group") %>%
        dplyr::left_join(.,bifurcation_sub_groups %>%
                             sf::st_drop_geometry() %>%
                             dplyr::select("riverID","river_group"),
                         by = "riverID")
    
    if (verbose == TRUE) {
        message("Reordering reaches by topology and arranging by group...",
                paste0("[", Sys.time(), "]"))
    }
    
    # get order routing should be performed
    bifurcation_sub_groups.list.upd.ord_topo <- bifurcation_sub_groups.list.upd %>%
        arrange_by_topology(riverID = "riverID",
                                          verbose = verbose)
    
    # set up group order for processing
    
    # ------------------------------------------------------------------------ #
    # reorder groups based on highest topo order of any single reach in each group
    order_of_processing_new <- bifurcation_sub_groups.list.upd.ord_topo |>
        sf::st_drop_geometry() |>
        dplyr::select("topo_order","river_group") |>
        dplyr::group_by(.data$river_group) |>
        dplyr::reframe("max_topo_order" = max(.data$topo_order)) |>
        dplyr::arrange(.data$max_topo_order) |>
        dplyr::mutate("river_group_recode_info" = as.character(dplyr::row_number()))
    
    names(order_of_processing_new$river_group_recode_info) =
        as.character(order_of_processing_new$river_group)
    
    named_vector_of_orders <- order_of_processing_new$river_group_recode_info
    # ------------------------------------------------------------------------ #
    
    reaches_just_upstream_of_bifurcation_IDs <-
        reaches_just_upstream_of_bifurcation %>%
        dplyr::pull("riverID")
    
    # ------------------------------------------------------------------------ #
    
    bifurcation_sub_groups.list.upd.ord_topo_upd <-
        bifurcation_sub_groups.list.upd.ord_topo %>%
        dplyr::mutate(
            "river_group_new" = 
            as.numeric(named_vector_of_orders[as.character(.data$river_group)])) %>%
        dplyr::mutate("river_group_new" = dplyr::case_when(
            is.na(.data$river_group_new) ~ (max(.data$river_group_new, na.rm = TRUE) + 1),
            .default = .data$river_group_new
        )) %>%
        dplyr::select(-c("river_group")) %>%
        dplyr::mutate("NEXT_stored" = .data$NEXT) %>%
        dplyr::mutate(
            "NEXT" = dplyr::case_when(
                .data$riverID %fin% reaches_just_upstream_of_bifurcation_IDs ~ list(-9999),
                .default = .data$NEXT
            )
        ) %>%
        dplyr::arrange(.data$river_group_new)
    
    # assign correct topo ordered groups
    
    
    if (verbose == TRUE) {
        message("Restoring HS class...",
                paste0("[", Sys.time(), "]"))
    }
    
    # add HS class back to object
    bifurcation_sub_groups.list.upd.ord_topo_upd <-
        assign_class(bifurcation_sub_groups.list.upd.ord_topo_upd, 
                                     c("HS"))
    
    bifurcation_sub_groups.list.upd.ord_topo_upd <-
        mod_HS_attributes(bifurcation_sub_groups.list.upd.ord_topo_upd,
                                          next_col = TRUE, col = "NEXT")
    
    bifurcation_sub_groups.list.upd.ord_topo_upd <-
        mod_HS_attributes(bifurcation_sub_groups.list.upd.ord_topo_upd,
                                          next_col = FALSE, col = "NEXT_stored")
    
    bifurcation_sub_groups.list.upd.ord_topo_upd <-
        mod_HS_attributes(bifurcation_sub_groups.list.upd.ord_topo_upd,
                                          prev_col = TRUE, col = "PREVIOUS")
    
    # With processing order established, we now run each in order and update the
    # list. Each iteration of the loop generates routed discharge for all edges
    # in the group, and updates the two or more downstream reaches in the
    # relevant group into which the furthest downstream edge of the current
    # group flow. That is, we add the reach specific runoff times series for the
    # bifurcated channels to a fraction (usually 1/2) of the accumulated
    # discharge at the edge immediately upstream of the bifurcation. These steps
    # then occur iteratively until all groups have been processed.
    
    if (verbose == TRUE) {
        message("Done!",
                paste0("[", Sys.time(), "]"))
    }
    
    return(bifurcation_sub_groups.list.upd.ord_topo_upd)
}

