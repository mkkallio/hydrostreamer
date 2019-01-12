#' hydrostreamer: A package for downscaling distributed runoff products on to 
#' explicit river segments.
#'
#' hydrostreamer provides functions to downscale distributed runoff data 
#' into an explicitly represented river network using. Downscaling is done by 
#' the spatial #' relationship between an areal unit of runoff and an overlaid 
#' river network. Value of the runoff unit is divided among intersecting river 
#' segments. hydrostreamer provides several methods for the assignment. Simple 
#' river routing algorithms are also provided to estimate discharge at each 
#' segment.
#'
#' @import raster
#' @import hydroGOF
#' @importFrom dplyr %>%
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom quadprog solve.QP
#' @importFrom dplyr bind_rows
#' @importFrom lubridate %m+%
#' @importFrom methods hasArg
#' @useDynLib hydrostreamer, .registration = TRUE
#' @docType package
#' @name hydrostreamer
NULL
