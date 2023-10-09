#' River basins for river network in Southeast Asia
#'
#' A simple feature dataset containing subbasins for a river network in
#' Southeast Asia derived from ALOS World 3D at 30 meter resolution DEM (Tanado
#' et al 2014).
#'
#' @format An sf object with 216 rows and 2 variables: \describe{
#'   \item{SEGMENT_ID}{A unique identifier, numeric}
#'   \item{geom}{simple feature geometry, sfc_MULTIPOLYGON sfc} ... }
#' @source
#'   \url{https://portal.opentopography.org/raster?opentopoID=OTALOS.112016.4326.2}
#'
#' @references \url{https://doi.org/10.5194/isprsannals-II-4-71-2014}
"basins_data"
