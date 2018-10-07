#' Computes summaries from hydrostreamer::HS* objects
#' 
#' Allows easy computation of summaries across runoff datasets with user defined 
#' functions. The functions provided are run either individually for, or across, 
#' each runoff/downscaled/flow sets in an HS* object. 
#' 
#' @param HS Any HS* object, or a list of tables with column \code{Date}.
#' @param summarise_over_all Compute functions individually, or across all data.
#'   Defaults to \code{TRUE}.
#' @param aggregate_monthly Summarise over the timestamps in data, or aggregate
#'   results to the 12 months of the year.
#' @param funs Functions to evaluate. By default, computes \code{min, mean, median and 
#'   max.}
#' @param ... Additional arguments passed to \code{funs}.
#' 
#' @return Returns the input HS* object, or a list, where runoff/downscaled/discharge
#'   is replaced with the computed summaries.
#' 
#' @export
ensemble_summary <- function(HS, 
                             summarise_over_all = TRUE, 
                             aggregate_monthly = FALSE,
                             funs=c("min","mean","median","max"), 
                             ...) {
    if(aggregate_monthly && class(HS) != "HSflow") {
        warning("Routing does not work appropriately for data aggregated to months. 
                Use original timeseries for routing.")
    }
    UseMethod("ensemble_summary")
}

#' @export
ensemble_summary.list <- function(HS,
                                  summarise_over_all = TRUE, 
                                  aggregate_monthly = FALSE, 
                                  funs=c("min","mean","median","max"),
                                  ...) {
    
    nrunoff <- length(HS)
    if (summarise_over_all && nrunoff != 1) {
        HS <- summarise_over_all(HS)
    }
    
    data <- HS
    
    output <- do_summary_fun(data, funs, aggregate_monthly, ...)
    
    return(output)
}



#' @export
ensemble_summary.HSgrid <- function(HS,
                                    summarise_over_all = TRUE,
                                    aggregate_monthly = FALSE,
                                    funs=c("min","mean","median","max"), 
                                    ...) {
    
    if ("quantile" %in% funs) stop("function 'quantile' not supported 
                                    for HSgrid object due to problems 
                                    further down the line.")
    
    nrunoff <- length(HS$runoff)
    if (summarise_over_all && nrunoff != 1) {
        HS$runoff <- summarise_over_all(HS$runoff)
    }
    
    data <- HS$runoff
    
    output <- do_summary_fun(data, funs, aggregate_monthly, ...)
    
    HS$runoff <- output
    return(HS)
}

#' @export
ensemble_summary.HSweights <- function(HS,  
                                       summarise_over_all = TRUE,
                                       aggregate_monthly = FALSE, 
                                       funs=c("min","mean","median","max"),
                                       ...) {
    
    if ("quantile" %in% funs) stop("function 'quantile' not supported 
                                    for HSweights object due to problems 
                                    further down the line.")
    
    nrunoff <- length(HS$grid$runoff)
    if (summarise_over_all && nrunoff != 1) {
        HS$runoff <- summarise_over_all(HS$grid$runoff)
    }
    
    data <- HS$grid$runoff
    
    output <- do_summary_fun(data, funs, aggregate_monthly, ...)
    
    HS$grid$runoff <- output
    return(HS)
}


#' @export
ensemble_summary.HSrunoff <- function(HS,        
                                      summarise_over_all = TRUE, 
                                      aggregate_monthly = FALSE,    
                                      funs=c("min","mean","median","max"),  
                                      ...) {
    
    if ("quantile" %in% funs) stop("function 'quantile' not supported 
                                    for HSrunoff object due to problems 
                                    further down the line.")
    
    nrunoff <- length(HS$downscaled)
    if (summarise_over_all && nrunoff != 1) {
        HS$downscaled <- summarise_over_all(HS$downscaled)
    }
    
    data <- HS$downscaled
    
    output <- do_summary_fun(data, funs, aggregate_monthly, ...)
    
    HS$downscaled <- output
    return(HS)
}


#' @export
ensemble_summary.HSflow <- function(HS,          
                                    summarise_over_all = TRUE, 
                                    aggregate_monthly = FALSE,   
                                    funs=c("min","mean","median","max"), 
                                    ...) {
    
    nrunoff <- length(HS$discharge)
    if (summarise_over_all && nrunoff != 1) {
        HS$discharge <- summarise_over_all(HS$discharge)
    }
    
    data <- HS$discharge
    
    output <- do_summary_fun(data, funs, aggregate_monthly, ...)
    
    HS$discharge <- output
    return(HS)
}
