#' @title Find all node and edge paths between two nodes in an sfnetwork
#'
#' @description This function receives a sfnetwork directed LINESTRING that
#'   describes a river network with lookup list within an attribute tibble and
#'   corresponding directed acyclic graph (DAG) igraph component of nodes and
#'   edges. Follow examples to prepare an appropriate sfnetwork object. The
#'   function returns a list of paths. Note param descriptions below are copied
#'   from igraph::all_simple_paths.
#'
#' @param graph An input graph.
#' @param sf_net_graph_equiv An input sf LINESTRING equivalent to the graph.
#' @param from The source vertex.
#' @param to The target vertex of vertices. Defaults to all vertices.
#' @param mode Character constant, gives whether the shortest paths to or from
#'   the given vertices should be calculated for directed graphs. If out then
#'   the shortest paths from the vertex, if ⁠in⁠ then to it will be considered.
#'   If all, the default, then the corresponding undirected graph will be used,
#'   i.e. not directed paths are searched. This argument is ignored for
#'   undirected graphs.
#' @param verbose Boolean. Print commentary of algorithm (useful for debugging).
#' @param parallel Boolean. Use parallel processing through
#'   future.apply::future_lapply
#' @param ... Any parameters relevant to igraph::all_simple_paths.
#'
#' @details The primary motivation for this function is to provide a packaged
#'   method for finding all possible "edge" paths between nodes on a directed
#'   acyclic graph that contains multiple edges (but not loops). Thus, this
#'   function fills the gap left by igraph::all_simple_paths, which only returns
#'   node paths for complex graphs. This function is intended to facilitate
#'   river network analyses for river systems that contain braids/islands and/or
#'   multiple outlets, not "simple" dendritic (e.g., in relation to in-stream
#'   barriers). See other packages (e.g., riverconn), for analysis of simple
#'   dendritic graphs/river networks (e.g., CCM2).
#'
#'   Note - Take care with direction of LINESTRINGs. Reverse using
#'   sf::st_reverse if necessary (see example).
#'
#' @return A list of unique edge paths between to and from nodes.
#' 
#' @export
all_simple_paths_for_complex_graphs <- function(graph,
                                                sf_net_graph_equiv,
                                                from,
                                                to,
                                                mode = "in",
                                                verbose = FALSE,
                                                parallel = FALSE,
                                                ...) {
    from_index = from
    
    if (verbose == TRUE) {
        message(
            "Running igraph::all_simple_paths to obtain node paths...",
            paste0("[", Sys.time(), "]")
        )
    }
    
    # extract all simple node paths between to and from nodes
    barrier_i_node_paths <- igraph::all_simple_paths(
        graph = graph,
        from = from,
        to = to,
        mode = mode,
        ...
    )
    
    if(length(barrier_i_node_paths) == 0){
        if (verbose == TRUE) {
            message(
                "No possible paths between nodes...",
                paste0("[", Sys.time(), "]")
            )
        }
        return(list("node_paths" = list(NA),
                    "edge_paths"= list(NA)))
    }
    
    if (verbose == TRUE) {
        message("igraph::all_simple_paths complete!",
                paste0("[", Sys.time(), "]"))
    }
    
    if (verbose == TRUE) {
        message("Extracting edge riverID of 'from' node...",
                paste0("[", Sys.time(), "]"))
    }
    
    # barrier riverID for barrier i
    #indices_of_edges_containing_barriers <-
    # OLD
    # barr_river_ID <-
    #     graph %>%
    #     sfnetworks::activate("edges") %>%
    #     dplyr::filter(.data$to == from_index) %>%
    #     sf::st_drop_geometry() %>%
    #     dplyr::pull(.data$riverID)
    
    # NEW
    barr_river_ID <- sf_net_graph_equiv[sf_net_graph_equiv$to == from_index, c("riverID")] %>%
        as.vector()
        # sf_net_graph_equiv %>%
        # dplyr::filter(.data$to == from_index) %>%
        # dplyr::pull(.data$riverID)
    
    
    if (verbose == TRUE) {
        message("Complete!",
                paste0("[", Sys.time(), "]"))
    }
    
    # ---------------------------------------------------------------- #
    # identify corresponding edges for each set of downstream nodes
    # encountered on route from barrier i to sea entry j, where k contains
    # unique node paths taken on downstream route from barrier i. Note here
    # that a unique path of nodes has been returned by function
    # igraph::all_simple_paths (note there is a wrapper of this function in
    # sfnetworks, but we are using the igraph original). Importantly, all
    # edge paths are not returned by igraph or sfnetworks, so we have to
    # find these manually, because, as the documentation for igraph says for
    # igraph::all_simple_paths, "This function currently ignored multiple
    # and loop edges" in their implementation. Using EU-hydro based 
    # river network, we have no loops (because we cleaned the network), but
    # we do have many multiple edges, which is an alternative term for
    # braids. The while loop in the following code finds all possible edge
    # paths, given the unique node paths that igraph::all_simple_paths found.
    
    # It is also worth noting that "multiple edges" are probably not
    # considered by igraph or sfnetworks because all possible routes
    # increase "exponentially" (i think it might be geometrically) with
    # every downstream split that occurs; i.e., the number of routes doubles
    # with one addition, then doubles again with another downstream split,
    # and so on. This might be a variant of the "Travelling Salesperson
    # Problem".
    # ---------------------------------------------------------------- #
    
    if (verbose == TRUE) {
        message(
            "Initiating identification of all edge paths for each list of node paths returned by igraph::all_simple_paths...",
            paste0("[", Sys.time(), "]")
        )
    }
    
    # if(parallel == TRUE) {
    #     
    #      options(future.globals.maxSize = 2000 * 1024^2)
    #     
    #      future::plan("multisession")
    #     barrier_i_edge_paths <-
    #         future.apply::future_lapply(1:length(barrier_i_node_paths), function(k) {
    #             # #######################################
    #             # # USEFUL PLOT
    #             # # plot complete set of edges including multiples
    #             # graph %>%
    #             #   sfnetworks::activate("nodes") %>%
    #             #   dplyr::slice({barrier_i_node_paths[[k]] %>%
    #             #   as.vector()}) %>%
    #             #   plot()
    #             # #######################################
    #             
    #             if (verbose == TRUE) {
    #                 cat("\r",
    #                     "Subsetting sfnetwork and extracting riverID, to, and from attributes.",
    #                     paste0(
    #                         " (Node path ",
    #                         k,
    #                         " of ",
    #                         length(barrier_i_node_paths),
    #                         " paths) ",
    #                         "[",
    #                         Sys.time(),
    #                         "]","\r"
    #                     )
    #                 )
    #             }
    #             
    #             # ------------------------------------- #
    #             # All edges between nodes
    #             edge_edge_paths <-
    #                 graph %>%
    #                 sfnetworks::activate("nodes") %>%
    #                 dplyr::slice({
    #                     barrier_i_node_paths[[k]] %>%
    #                         as.vector()
    #                 }) %>%
    #                 sfnetworks::activate("edges") %>%
    #                 sf::st_drop_geometry() %>%
    #                 dplyr::select("from", "to", "riverID") %>%
    #                 tibble::as_tibble(.name_repair = "minimal") %>%
    #                 as.matrix()
    #             
    #             # #######################################
    #             # # USEFUL PLOT
    #             # # plot igraph representation to visually inspect nodes and edges
    #             # # sequence (noting that edge directions have been reversed)
    #             # graph %>%
    #             #   sfnetworks::activate("nodes") %>%
    #             #   dplyr::slice({barrier_i_node_paths[[k]] %>%
    #             #   as.vector()}) %>%
    #             #   sfnetworks::activate("edges") %>%
    #             #   igraph::as.igraph() %>%
    #             #   plot()#
    #             # #######################################
    #             
    #             # source and sink node ids (barrier node index is number id that
    #             # appears in "to" column but not "from" column)
    #             
    #             if (verbose == TRUE) {
    #                 cat("\r",
    #                     "Identifying all possible routes between node pair...",
    #                     paste0(
    #                         " (Node path ",
    #                         k,
    #                         " of ",
    #                         length(barrier_i_node_paths),
    #                         " paths) ",
    #                         "[",
    #                         Sys.time(),
    #                         "]","\r"
    #                     )
    #                 )
    #             }
    #             
    #             barrier_node_index <-
    #                 setdiff(edge_edge_paths[,"to"], edge_edge_paths[,"from"])
    #             
    #             outflow_node_index <-
    #                 setdiff(edge_edge_paths[,"from"], edge_edge_paths[,"to"])
    #             
    #             #now find all unique simple routes from barrier_node_index to
    #             #outflow_node_index identifying all possible routes between each node
    #             #pairing janitor::get_dupes(dat = edge_edge_paths, c("from"))
    #             #construct/chain each path individually, by iteration
    #             
    #             if (verbose == TRUE) {
    #                 cat("\r",
    #                     "Initiating vectors for while loop...",
    #                     paste0(
    #                         " (Node path ",
    #                         k,
    #                         " of ",
    #                         length(barrier_i_node_paths),
    #                         " paths) ",
    #                         "[",
    #                         Sys.time(),
    #                         "]","\r"
    #                     )
    #                 )
    #             }
    #             
    #             paths.list_upd <- list(barr_river_ID)
    #             first_node_id <- barrier_node_index
    #             nodes.list_upd <- list(first_node_id)
    #             next_node_id = first_node_id
    #             last_edge = FALSE
    #             
    #             if (verbose == TRUE) {
    #                 cat("\r",
    #                     "Tracing all edge paths iteratively...",
    #                     paste0(
    #                         " (Node path ",
    #                         k,
    #                         " of ",
    #                         length(barrier_i_node_paths),
    #                         " paths) ",
    #                         "[",
    #                         Sys.time(),
    #                         "]","\r"
    #                     )
    #                 )
    #             }
    #             
    #             # ------------------------------------------------------- #
    #             # WHILE LOOP TO SIMULTANEOUSLY TRACE ALL DOWNSTREAM ROUTES FROM
    #             # BARRIER i TO SEA ENTRY j
    #             # ------------------------------------------------------- #
    #             while (last_edge == FALSE) {
    #                 
    #                 
    #                 # remove map_depth
    #                 next_edge_riverID.list <-
    #                     lapply(
    #                         next_node_id,
    #                         FUN = function(x) {
    #                             return((edge_edge_paths[which(edge_edge_paths[,"to"] %fin% x),"riverID"]))
    #                         }
    #                     ) %>%
    #                     Filter(f = Negate(function(x) is.null(unlist(x))), .) %>%
    #                     ifelse(lengths(.) == 0, NA, .)
    #                 
    #                 next_node_id.list <-
    #                     lapply(
    #                         next_node_id,
    #                         FUN = function(x) {
    #                             return((edge_edge_paths[which(edge_edge_paths[,"to"] %fin% x),"from"]))
    #                         }
    #                     ) %>%
    #                     Filter(Negate(function(x) is.null(unlist(x))), .) %>%
    #                     ifelse(lengths(.) == 0, NA, .)
    #                 
    #                 # could try https://stackoverflow.com/a/26540063 instad of purrr:compact
    #                 
    #                 # append to paths list duplicate paths or not before append. This
    #                 # allows simultaneous route finding.
    #                 
    #                 # --------------------------#
    #                 # RIVERIDS
    #                 
    #                 paths.list_upd =
    #                     paths.list_upd %>%
    #                     {
    #                         if (length(unique(next_node_id.list)) == 1) {
    #                             rep(., times = lengths(next_edge_riverID.list))
    #                         } else {
    #                             rep(., times = lengths(next_edge_riverID.list))
    #                         }
    #                     } %>%
    #                     {
    #                         Map(., f = c, (next_edge_riverID.list %>%
    #                                            unlist()))
    #                         
    #                     }
    #                 
    #                 # -------------------
    #                 # NODES
    #                 nodes.list_upd =
    #                     nodes.list_upd %>%
    #                     {
    #                         if (length(unique(next_node_id.list)) == 1) {
    #                             rep(., times = lengths(next_node_id.list))
    #                         } else {
    #                             rep(., times = lengths(next_node_id.list))
    #                         }
    #                     } %>%
    #                     {
    #                         Map(., f = c, (next_node_id.list %>%
    #                                            unlist()))
    #                     }
    #                 
    #                 
    #                 next_node_id_check <-
    #                     nodes.list_upd %>%
    #                     lapply(
    #                         .,
    #                         FUN = function(x) {
    #                             lapply(
    #                                 .,
    #                                 FUN = function(z) {
    #                                     z[[length(z)]]
    #                                 }
    #                             )
    #                         }
    #                     ) %>%
    #                     purrr::pluck(1) %>%
    #                     unlist(recursive = TRUE) %>%
    #                     .[!is.na(.)]
    #                 
    #                 next_node_id <-
    #                     nodes.list_upd %>%
    #                     lapply(
    #                         .,
    #                         FUN = function(x) {
    #                             lapply(
    #                                 .,
    #                                 FUN = function(z) {
    #                                     z[[length(z)]]
    #                                 }
    #                             )
    #                         }
    #                     ) %>%
    #                     purrr::pluck(1)
    #                 
    #                 last_edge = ifelse(all(next_node_id_check == outflow_node_index),
    #                                    TRUE,
    #                                    FALSE)
    #             }
    #             
    #             if (verbose == TRUE) {
    #                 cat("\r",
    #                     "All edge paths traced!",
    #                     paste0(
    #                         " (Node path ",
    #                         k,
    #                         " of ",
    #                         length(barrier_i_node_paths),
    #                         " paths) ",
    #                         "[",
    #                         Sys.time(),
    #                         "]","\r"
    #                     )
    #                 )
    #             }
    #             
    #             if (verbose == TRUE) {
    #                 cat("\r",
    #                     "Bundling all edge paths into list...",
    #                     paste0(
    #                         " (Node path ",
    #                         k,
    #                         " of ",
    #                         length(barrier_i_node_paths),
    #                         " paths) ",
    #                         "[",
    #                         Sys.time(),
    #                         "]","\r"
    #                     )
    #                 )
    #             }
    #             
    #             all_downsream_paths_from_barrier_i_to_sea_entry_j_node_path_k <-
    #                 paths.list_upd %>%
    #                 lapply(function(x) {
    #                     # purrr::discard(x, is.na)
    #                     x[!is.na(x)]
    #                 })
    #             
    #             # # ---------------------------------------------- #
    #             
    #             return(all_downsream_paths_from_barrier_i_to_sea_entry_j_node_path_k)
    #             
    #         },future.seed=TRUE)
    #     
    #     
    #} else 
    #    if(parallel == FALSE){
        
        barrier_i_edge_paths <-
            lapply((1:length(barrier_i_node_paths)), function(k) {
                # cat("\r",k)
                # #######################################
                # # USEFUL PLOT
                # # plot complete set of edges including multiples
                # graph %>%
                #   sfnetworks::activate("nodes") %>%
                #   dplyr::slice({barrier_i_node_paths[[k]] %>%
                #   as.vector()}) %>%
                #   plot()
                # #######################################
                
                if (verbose == TRUE) {
                    cat("\r",
                        "Subsetting sfnetwork and extracting riverID, to, and from attributes.",
                        paste0(
                            " (Node path ",
                            k,
                            " of ",
                            length(barrier_i_node_paths),
                            " paths) ",
                            "[",
                            Sys.time(),
                            "]","\r"
                        )
                    )
                }
                
                # ------------------------------------- #
                # All edges between nodes
                edge_edge_paths <-
                    graph %>%
                    sfnetworks::activate("nodes") %>%
                    dplyr::slice({
                        barrier_i_node_paths[[k]] %>%
                            as.vector()
                    }) %>%
                    sfnetworks::activate("edges") %>%
                    sf::st_drop_geometry() %>%
                    dplyr::select("from", "to", "riverID") %>%
                    tibble::as_tibble(.name_repair = "minimal") %>%
                    as.matrix()
                
                # #######################################
                # # USEFUL PLOT
                # # plot igraph representation to visually inspect nodes and edges
                # # sequence (noting that edge directions have been reversed)
                # graph %>%
                #   sfnetworks::activate("nodes") %>%
                #   dplyr::slice({barrier_i_node_paths[[k]] %>%
                #   as.vector()}) %>%
                #   sfnetworks::activate("edges") %>%
                #   igraph::as.igraph() %>%
                #   plot()#
                # #######################################
                
                # source and sink node ids (barrier node index is number id that
                # appears in "to" column but not "from" column)
                
                if (verbose == TRUE) {
                    cat("\r",
                        "Identifying all possible routes between node pair...",
                        paste0(
                            " (Node path ",
                            k,
                            " of ",
                            length(barrier_i_node_paths),
                            " paths) ",
                            "[",
                            Sys.time(),
                            "]","\r"
                        )
                    )
                }
                
                barrier_node_index <-
                    setdiff(edge_edge_paths[,"to"], edge_edge_paths[,"from"])
                
                outflow_node_index <-
                    setdiff(edge_edge_paths[,"from"], edge_edge_paths[,"to"])
                
                # now find all unique simple routes from barrier_node_index to outflow_node_index
                # identifying all possible routes between each node pairing
                #janitor::get_dupes(dat = edge_edge_paths, c("from"))
                # construct/chain each path individually, by iteration
                
                if (verbose == TRUE) {
                    cat("\r",
                        "Initiating vectors for while loop...",
                        paste0(
                            " (Node path ",
                            k,
                            " of ",
                            length(barrier_i_node_paths),
                            " paths) ",
                            "[",
                            Sys.time(),
                            "]","\r"
                        )
                    )
                }
                
                paths.list_upd <- list(barr_river_ID)
                first_node_id <- barrier_node_index
                nodes.list_upd <- list(first_node_id)
                next_node_id = first_node_id
                last_edge = FALSE
                
                if (verbose == TRUE) {
                    cat("\r",
                        "Tracing all edge paths iteratively...",
                        paste0(
                            " (Node path ",
                            k,
                            " of ",
                            length(barrier_i_node_paths),
                            " paths) ",
                            "[",
                            Sys.time(),
                            "]","\r"
                        )
                    )
                }
                
                # ------------------------------------------------------- #
                # WHILE LOOP TO SIMULTANEOUSLY TRACE ALL DOWNSTREAM ROUTES FROM
                # BARRIER i TO SEA ENTRY j
                # ------------------------------------------------------- #

                while (last_edge == FALSE) {

                    # remove map_depth
                    next_edge_riverID.list <-
                        lapply(
                            next_node_id,
                            FUN = function(x) {
                                return((edge_edge_paths[which(edge_edge_paths[,"to"] %fin% x),"riverID"]))
                            }
                        ) %>%
                        Filter(f = Negate(function(x) is.null(unlist(x))), .) %>%
                        ifelse(lengths(.) == 0, NA, .)
                    
                    next_node_id.list <-
                        lapply(
                            next_node_id,
                            FUN = function(x) {
                                return((edge_edge_paths[which(edge_edge_paths[,"to"] %fin% x),"from"]))
                            }
                        ) %>%
                        Filter(Negate(function(x) is.null(unlist(x))), .) %>%
                        ifelse(lengths(.) == 0, NA, .)
                    
                    # could try https://stackoverflow.com/a/26540063 instad of purrr:compact
                    
                    # append to paths list duplicate paths or not before append. This
                    # allows simultaneous route finding.
                    
                    # --------------------------#
                    # RIVERIDS
                    
                    paths.list_upd =
                        paths.list_upd %>%
                        {
                            if (length(unique(next_node_id.list)) == 1) {
                                # .
                                rep(., times = lengths(next_edge_riverID.list))
                            } else {
                                # list(.) %>%
                                rep(., times = lengths(next_edge_riverID.list))
                            }
                        } %>%
                        {
                            Map(., f = c, (next_edge_riverID.list %>%
                                               unlist()))
                            
                        }
                    
                    # -------------------
                    # NODES
                    nodes.list_upd =
                        nodes.list_upd %>%
                        {
                            if (length(unique(next_node_id.list)) == 1) {
                                rep(., times = lengths(next_node_id.list))
                            } else {
                                rep(., times = lengths(next_node_id.list))
                            }
                        } %>%
                        {
                            Map(., f = c, (next_node_id.list %>%
                                               unlist()))
                        }
                    
                    # old
                    # next_node_id_check <-
                       #  nodes.list_upd %>%
                       #  lapply(
                       #      .,
                       #      FUN = function(x) {
                       #          lapply(
                       #              .,
                       #              FUN = function(z) {
                       #                   z[[length(z)]]
                       #                  #tail(z,n = 1)
                       #                  #dplyr::last(z)
                       #              }
                       #          )
                       #      }
                       #  ) %>%
                       #  purrr::pluck(1) %>%
                       #  unlist(recursive = TRUE) %>%
                       # .[!is.na(.)]
                    
                    # new
                    # https://stackoverflow.com/a/67633454
                    next_node_id_check <- purrr::map2(nodes.list_upd, lengths(nodes.list_upd), purrr::pluck) %>%
                        unlist(recursive = TRUE) %>%
                        .[!is.na(.)]

                    # old
                    # next_node_id <-
                    #     nodes.list_upd %>%
                    #     lapply(
                    #         .,
                    #         FUN = function(x) {
                    #             lapply(
                    #                 .,
                    #                 FUN = function(z) {
                    #                      z[[length(z)]]
                    #                     #tail(z,n = 1)
                    #                     #dplyr::last(z)
                    #                 }
                    #             )
                    #         }
                    #     ) %>%
                    #     purrr::pluck(1)
                    
                    # new
                    # https://stackoverflow.com/a/67633454
                    next_node_id <- purrr::map2(nodes.list_upd, lengths(nodes.list_upd), purrr::pluck)

                    last_edge = ifelse(all(next_node_id_check == outflow_node_index),
                                       TRUE,
                                       FALSE)
                }
                
                if (verbose == TRUE) {
                    cat("\r",
                        "All edge paths traced!",
                        paste0(
                            " (Node path ",
                            k,
                            " of ",
                            length(barrier_i_node_paths),
                            " paths) ",
                            "[",
                            Sys.time(),
                            "]","\r"
                        )
                    )
                }
                
                if (verbose == TRUE) {
                    cat("\r",
                        "Bundling all edge paths into list...",
                        paste0(
                            " (Node path ",
                            k,
                            " of ",
                            length(barrier_i_node_paths),
                            " paths) ",
                            "[",
                            Sys.time(),
                            "]","\r"
                        )
                    )
                }
                
                all_downsream_paths_from_barrier_i_to_sea_entry_j_node_path_k <-
                    paths.list_upd %>%
                    lapply(function(x) {
                        # purrr::discard(x, is.na)
                        x[!is.na(x)]
                    })
                
                # # ---------------------------------------------- #
                
                return(all_downsream_paths_from_barrier_i_to_sea_entry_j_node_path_k)
                
            })
 #   }
    
    if (verbose == TRUE) {
        message("\nFiltering to retain only unique paths...",
                paste0("[", Sys.time(), "]"))
    }
    
    # filter unique downstream edge paths only
    barrier_i_edge_paths_unique <- barrier_i_edge_paths %>%
        unlist(recursive = FALSE) %>%
        unique()
    
    if (verbose == TRUE) {
        message("Completed!",
                paste0("[", Sys.time(), "]"))
    }
    
    return(list("node_paths" = barrier_i_node_paths,
                "edge_paths"= barrier_i_edge_paths_unique))
}

#' @title Find all downstream segments from a specified river segment, returning
#'   a list in the case of anabranching networks.
#'
#' @description This function is a wrapper for
#'   all_simple_paths_for_complex_graphs to make finding all unique edge paths
#'   to sea entry easier. The function receives a sf LINESTRING that describes a
#'   river network and a reach riverID of interest. This object is converted by
#'   the function into an sfnetwork object, which contains an attribute tibble
#'   and corresponding directed acyclic graph (DAG) igraph component of nodes
#'   and edges (where each edge corresponds to a reach riverID). The function
#'   returns a list tables, each representing a unique path, containing a
#'   sequence of riverIDs ordered from the riverID of interest to all possible
#'   sea entry locations.
#'
#' @param HSnetwork_sfnetworks An input sfnetworks LINESTRING river network.
#' @param HSnetwork Corresponding LINESTRING river network.
#' @param ID The riverID from which to start the path to sea.
#' @param verbose Boolean. Print commentary of algorithm (useful for debugging).
#' @param ... Any parameters relevant to igraph::all_simple_paths or
#'   all_simple_paths_for_complex_graphs.
#'
#' @details The primary motivation for this function is make it easier to work
#'   with the all_simple_paths_for_complex_graphs, by providing a
#'   a more intuitive set of arguments. In this way, the user does not need to
#'   pre-process their river network into an sfnetwork object and find the
#'   relevant from and to nodes associated with the igraph component of the
#'   object. Note that this method is designed for finding all possible "edge"
#'   paths from a reach of interest to the sea on a directed acyclic graph that
#'   may contain anabranching sections and multiple sea outlets (but not
#'   cycles). Like all_simple_paths_for_complex_graphs, this
#'   function fills the gap left by igraph::all_simple_paths, which only returns
#'   node paths for complex graphs, but receives and returns a more intuitive
#'   output for users of the sf package.
#'   
#'   This function is intended to facilitate river network analyses for river
#'   systems that contain anabranching/islands and/or multiple outlets, not
#'   "simple" dendritic (e.g., in relation to in-stream barriers). See other
#'   packages (e.g., riverconn), for analysis of simple dendritic graphs/river
#'   networks (e.g., CCM2).
#'
#' @return A list of tables containing complete unique paths to sea from a user
#'   defined starting reach.
#' 
#' @export
all_downstream_routes_to_sea <- function(HSnetwork_sfnetworks,
                                         HSnetwork,
                                         ID,
                                         verbose = TRUE,
                                         ...){
    
    if (verbose == TRUE) {
        message(
            "Converting sf object to sfnetworks...",
            paste0("[", Sys.time(), "]")
        )
    }
    
    # sea entry node index or indices for sea entries

    sea_entry_node_index =
        HSnetwork[HSnetwork$NEXT == "",c("to")] %>%
        # HSnetwork %>%
        # dplyr::filter(.data$NEXT == "") %>%
        # dplyr::select("to") %>%
        unlist() %>%
        as.vector()

    # upstream riverIDs which need to be filtered out at a later step, but are
    # inherently identified during the search for node paths to sea implemented
    # by igraph.
    upstream_riverIDs_of_roi = 
        HSnetwork[HSnetwork$riverID == ID,c("PREVIOUS")] %>%
        as.vector() %>%
        # HSnetwork %>%
        # dplyr::filter(.data$riverID == ID) %>%
        # dplyr::pull("PREVIOUS") %>%
        stringr::str_split(., pattern = " ",
                           simplify = TRUE) %>%
        as.vector() %>%
        as.integer()
    
    if (verbose == TRUE) {
        message(
            "Finding all unique edge and node path routes to sea...",
            paste0("[", Sys.time(), "]")
        )
    }
    
    # EDGE PATHS
    # all edge paths from upstream node of chosen upstream edge to downstream node of sea entry edge
    barrier_i_edge_and_node_paths_unique <- lapply(1:length(sea_entry_node_index), function(i){
  #cat("\r",i)
        upstream_edge_index =
            HSnetwork %>%
            dplyr::reframe("up_edge_index" = which(.data$riverID == ID)) %>%
            unlist() %>%
            as.vector()
        
        # upstream node of upstream edge for source
        upstream_node_index =
            HSnetwork %>%
            dplyr::slice(upstream_edge_index) %>%
            tibble::as_tibble(.name_repair = "minimal") %>%
            dplyr::select("from") %>%
            unlist() %>%
            as.vector()
        
        
        # graph = HSnetwork_sfnetworks
        # sf_net_graph_equiv = HSnetwork
        # from = upstream_node_index
        # to = sea_entry_node_index[i]
        # mode = "out"
        # verbose = verbose
        
        barrier_i_edge_paths_unique_i =
            all_simple_paths_for_complex_graphs(
                graph = HSnetwork_sfnetworks,
                sf_net_graph_equiv = HSnetwork,
                from = upstream_node_index,
                to = sea_entry_node_index[i],
                mode = "out",
                verbose = verbose,
                ...
            )
        
        return(barrier_i_edge_paths_unique_i)
    }) 
    
    if (verbose == TRUE) {
        message(
            "Extracting edge path routes to sea only...",
            paste0("[", Sys.time(), "]")
        )
    }
    
    barrier_i_edge_paths_unique = barrier_i_edge_and_node_paths_unique %>%
        lapply(., function(xy){
            edge_paths_only =  xy[["edge_paths"]]
            return(edge_paths_only)
        }) %>%
        unlist(recursive = FALSE)
    
    if (verbose == TRUE) {
        message(
            "Removing reaches immediately above reach of interest from edge path...",
            paste0("[", Sys.time(), "]")
        )
    }
    
    edge_paths_to_return = barrier_i_edge_paths_unique %>%
        lapply(.,FUN =  function(jk){
            rev(jk[!c(jk %fin% upstream_riverIDs_of_roi)])
        })
    
    # return list of tables
    result_to_return =  edge_paths_to_return %>%
        lapply(.,FUN =  function(jkl){

            res = 
                HSnetwork[HSnetwork$riverID %fin% jkl,]
                # HSnetwork %>%
                # dplyr::filter(.data$riverID %fin% jkl)
            
            return(res)
        })
    
    nrows_to_return_per_table = lapply(1:length(result_to_return), 
                                       function(jklm) {
        nrow(result_to_return[[jklm]])
    }) %>%
        do.call(c, .)
    
    result_to_return_no_empty <- result_to_return[nrows_to_return_per_table > 0]
    
    # remove any distributaries that occur at the start/upstream node of the
    # reach of interest. Again this is a feature of using igraph to find all
    # node and edge paths.
    
    result_to_return_no_empty_no_distributary_routes =
        lapply(1:length(result_to_return_no_empty), function(y){
            result_to_return_no_empty[[y]]$riverID[[1]]
       }) %>%
        do.call(c,.)
    
    keep_table <- result_to_return_no_empty_no_distributary_routes == ID
    
    result = result_to_return_no_empty[keep_table]
    
    return(result)
    
}
