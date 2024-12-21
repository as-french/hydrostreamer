#' Simplified river network for the river Miera, Spain (split)
#'
#' A simple feature dataset containing a subset of simplified EU-Hydro river
#' network for the river Miera, Spain. This dataset is derived from
#' hydrostreamer::miera_rivers. Here, network edges have been split to
#' facilitate the constant velocity routing algorithm with hourly climate grid
#' runoff product ER5-Land.
#'
#' @format An sf object with 832 rows and 5 variables:
#' \describe{
#'   \item{riverID}{id for each river reach, a numeric}
#'   \item{old_riverID}{id for each river reach prior to split, a numeric}
#'   \item{river_group}{id for each river group, a numeric}
#'   \item{Maidment_velocity_ms}{Maidment velocity in m per s, a numeric}
#'   \item{geom}{simple feature geometry, sfc_LINESTRING sfc}
#'   ...
#' }
#' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-hydro/eu-hydro-river-network-database}
#' @references \url{https://github.com/ateucher/rmapshaper}
"miera_rivers_split"
