#' hydrostreamer: A package for downscaling distributed runoff products in to 
#' explicit river segments.
#'
#' hydrostreamer provides functions to downscale distributed runoff data 
#' into an explicitly represented river network. Downscaling is done by the spatial 
#' relationship between an areal unit of runoff and an overlaid river network. 
#' Value of the runoff unit is divided among intersecting river segments. hydrostreamer
#' provides several methods for the assignment. Simple river routing algorithms 
#' are also provided to estimate discharge at each segment.
#' @importFrom dplyr %>%
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom quadprog solve.QP
#' @useDynLib hydrostreamer, .registration = TRUE
#' @docType package
#' @name hydrostreamer
NULL
