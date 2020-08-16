#' Apply river routing
#' 
#' Apply river routing using any of the provided methods. The function takes 
#' \code{HS} object with runoff timeseries as an input. See details.
#'
#' There are currently two routing algorithms implemented: 
#' \code{'instant'}aneous flow, where all runoff is routed through the entire 
#' river network at every timestep. There is no lag in instantaneous routing, 
#' which  means it may not be reasonable for large river networks. 
#' \code{'constant'} velocity river routing routes runoff down the river network
#' with a constant, user specified velocity. See further details from 
#' the documentation of each method:
#' \itemize{
#'   \item \code{\link{accumulate_runoff_instant}}
#'   \item \code{\link{accumulate_runoff_constant}}
#' }
#'
#' @param HS A 'HS' object obtained by \code{\link{interpolate_runoff}}
#' @param method Character string specifying the method to be used. 
#' @param ... Arguments passed to the routing algorithm, and to 
#'   \code{\link{river_network}}, if it has not been run already.
#' @param verbose Whether or not to print progress information. Defaults to 
#'   \code{FALSE}.
#'
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#' 
#' @export
accumulate_runoff <- function(HS, 
                              method=c("instant", "constant"), 
                              ..., 
                              verbose = FALSE) {
  # ----------------------------------------------------------------------------
  # test input
  
  test <- inherits(HS, "HS")
  if(!test) stop("Input must be of class 'HS'")
  
  # test if routing is needed, or if it has already been done
  ind <- find_attribute(HS, "next_col", TRUE)
  
  test <- length(ind) == 0
  if(test) {
    if(!hasArg("next_down")) next_down <- NULL
    if(!hasArg("previous")) previous <- NULL
    if(!hasArg("na_value")) na_value <- NULL
    
    if(verbose) message("No routing information found: running 'river_network()")
    HS <- river_network(HS, next_down, previous, na_value, verbose = verbose)
    
  }
  
  
  # ----------------------------------------------------------------------------
  # route
  if(method[1] == "instant") {
    
    output <- accumulate_runoff_instant(HS, verbose = verbose)
    
  } else if (method[1] == "constant") {
    
    params <- list(...)
    
    if(!hasArg("velocity")) {
      params[["velocity"]] <- 1
    }
    
    output <- accumulate_runoff_constant(HS, 
                                         velocity = params$velocity,
                                         verbose = verbose)
    
  }

  # ----------------------------------------------------------------------------
  # output
  
  output <- assign_class(output, "HS")
  return(output)
}


