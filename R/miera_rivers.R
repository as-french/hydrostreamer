#' Simplified river network for the river Miera, Spain
#'
#' A simple feature dataset containing a subset of simplified EU-Hydro river
#' network for the river Miera, Spain. This LINESTRING was simplified to 5%
#' original detail using the rmapshaper package, which preserves topologies.
#'
#' @format An sf object with 111 rows and 8 variables:
#' \describe{
#'   \item{riverID}{id for each river reach, a numeric}
#'   \item{river_group}{id for each river group, a numeric}
#'   \item{region_id}{AMBER region code, a numeric}
#'   \item{lake_id}{id for lake nitersecting river reach, an integer}
#'   \item{gmean_m}{Elevation above sea level in m, a numeric}
#'   \item{grad}{Gradient from start to end node of reach, a numeric}
#'   \item{UCA_km2}{Upstream catchment area in square kilometers, a numeric}
#'   \item{geom}{simple feature geometry, sfc_LINESTRING sfc}
#'   ...
#' }
#' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-hydro/eu-hydro-river-network-database}
#' @references \url{https://github.com/ateucher/rmapshaper}
"miera_rivers"
