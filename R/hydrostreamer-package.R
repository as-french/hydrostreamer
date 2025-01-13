#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom fastmatch %fin%
#' @importFrom dplyr %>% bind_rows .data
#' @importFrom rlang :=
#' @importFrom utils setTxtProgressBar txtProgressBar hasName write.csv
#' @importFrom quadprog solve.QP
#' @importFrom lubridate %m+%
#' @importFrom methods hasArg
#' @importFrom stats complete.cases lm optim
#' @importFrom Matrix rankMatrix
#' @importFrom Rcpp evalCpp
#' @importFrom hydroTSM sfreq vector2zoo daily2monthly daily2annual
#'   monthly2annual dm2seasonal drawTimeAxis time2season seasonalfunction fdc
#'   fdc.default fdc.zoo fdc.matrix fdc.data.frame
#' @useDynLib hydrostreamer, .registration = TRUE
utils::globalVariables(".") # https://github.com/tidyverse/magrittr/issues/29
## usethis namespace: end
NULL
