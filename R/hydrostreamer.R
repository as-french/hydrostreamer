"_PACKAGE"
#' hydrostreamer: A package for interpolatinng distributed runoff products on to 
#' explicit river network
#'
#' Hydrostreamer provides functions to interpolate multiple distributed runoff 
#' data on to an explicitly represented river network, route runoff down the 
#' network, and optimise predictions against observed streamflow.
#'
#' @importFrom dplyr %>%
#' @importFrom utils setTxtProgressBar txtProgressBar hasName write.csv
#' @importFrom quadprog solve.QP
#' @importFrom dplyr bind_rows
#' @importFrom lubridate %m+%
#' @importFrom methods hasArg
#' @importFrom stats complete.cases lm optim
#' @importFrom Matrix rankMatrix
#' @importFrom Rcpp evalCpp
#' @importFrom rlang := .data
#' @importFrom hydroTSM sfreq vector2zoo daily2monthly daily2annual
#'   monthly2annual dm2seasonal drawTimeAxis time2season seasonalfunction fdc
#'   fdc.default fdc.zoo fdc.matrix fdc.data.frame
#' @useDynLib hydrostreamer, .registration = TRUE
# #' @docType package
#' @name hydrostreamer
NULL
