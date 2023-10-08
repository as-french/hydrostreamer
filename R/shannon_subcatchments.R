#' Simplified subcatchments for the river Shannon, Ireland
#'
#' A dataset containing a subset of simplified reach-specific sub-catchments for
#' the river Shannon, Ireland. This sf POLYGON object was derived using
#' hydrostreamer::voronoi_rivers with inputs of simplified EU-Hydro river
#' segments and coastline derived using rmapshaper.
#'
#' @format An sf object with 2206 rows and 6 variables: \describe{
#'   \item{riverID}{A unique identifier, numeric}
#'   \item{manning_n}{Manning's roughness, numeric}
#'   \item{region_id}{AMBER region identifier, character}
#'   \item{river_group}{AMBER river group identifier, numeric}
#'   \item{elev_m}{subcatchment elevation in m, numeric}
#'   \item{geom}{simple feature geometry, sfc_POLYGON sfc} ... }
#' @source
#'   \url{https://land.copernicus.eu/imagery-in-situ/eu-hydro/eu-hydro-river-network-database}
#'
#' @references \url{https://github.com/ateucher/rmapshaper}
#' @references \url{https://github.com/mkkallio/hydrostreamer}
"shannon_subcatchments"
