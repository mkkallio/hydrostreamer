#' Apply river routing
#' 
#' Apply river routing using any of the provided methods. The function takes 
#' \code{HS} object with runoff timeseries as an input.
#'
#' There are currently three routing algorithms implemented: 
#' \code{'instant'}aneous flow, where all runoff is routed through the entire 
#' river network at every timestep. There is no lag in instantaneous routing, 
#' which  means it may not be reasonable for large river networks. 
#' \code{'simple'} river routing contains a simple lag based on given flow 
#' velocity and river segment lengths. \code{'muskingum'} implements a muskingum
#' river routing algorithm where k (storage) parameter is computed from given 
#' flow velocity and river segment length. Large difference in timesteps of 
#' runoff may result in computational instability. See further details from 
#' the documentation of each method:
#' \itemize{
#'   \item \code{\link{accumulate_runoff_instant}}
#'   \item \code{\link{accumulate_runoff_simple}}
#'   \item \code{\link{accumulate_runoff_muskingum}}
#' }
#'
#' @param HS A 'HS' object obtained by \code{\link{downscale_runoff}}
#' @param method Character string specifying the method to be used. 
#' @param ... Arguments passed to the routing algorithm.
#' @param verbose Whether or not to print progress information. Defaults to 
#'   \code{FALSE}.
#'
#' @return Returns the input object \code{HS}) with an added list column
#'   \code{discharge_ts} containing routed discharge estimates for each river
#'    segment. 
#' 
#' @export
accumulate_runoff <- function(HS, 
                              method=c("instant", "simple", "muskingum"), 
                              ..., 
                              verbose = FALSE) {
  
    class(HS) <- method[1]
    UseMethod("accumulate_runoff", HS)
  
}

#' @export
accumulate_runoff.instant <- function(HS,
                                      method=c("instant", 
                                               "simple", 
                                               "muskingum"), 
                                      ..., 
                                      verbose = FALSE) {
    
    output <- accumulate_runoff_instant(HS, 
                                        verbose = verbose)
    output <- assign_class(output, "HS")
    
    return(output)
}





#' @export
accumulate_runoff.muskingum <- function(HS,
                                        method=c("instant", 
                                                 "simple", 
                                                 "muskingum"), 
                                        ..., 
                                        verbose = FALSE) {
    
    params <- list(...)
   
    if(!hasArg("x")) {
        stop("Muskingum routing requires parameter x")
        x <- NULL
    }
    if(!hasArg("velocity")) {
        params[["velocity"]] <- 1
    }
    
    output <- accumulate_runoff_muskingum(HS, 
                                          velocity = params$velocity,
                                          x = params$x,
                                          verbose = verbose)
    output <- assign_class(output, "HS")
    
    return(output)
}



#' @export
accumulate_runoff.simple <- function(HS,
                                     method=c("instant", 
                                              "simple", 
                                              "muskingum"), 
                                     ..., 
                                     verbose = FALSE) {
    
    params <- list(...)
    
    if(!hasArg("velocity")) {
        params[["velocity"]] <- 1
    }
    
    output <- accumulate_runoff_simple(HS, 
                                          velocity = params$velocity,
                                          verbose = verbose)
    output <- assign_class(output, "HS")
    
    return(output)
}


