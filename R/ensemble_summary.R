#' Computes summary timeseries in HS object
#' 
#' Allows easy computation of summaries across runoff datasets with user defined 
#' functions. The functions provided are run either individually for, or across, 
#' each runoff/downscaled/discharge timeseries in an HS* object. 
#' 
#' Applicable functions take a vector of numeric values as input, and return 
#' a single numerical value.
#' 
#' @param HS \code{HS} object, or a list of data frames with column \code{Date}.
#' @param summarise_over_timeseries Apply function(s) column-wise (to
#'   each timeseries separately, \code{TRUE}), or row-wise for each date in 
#'   timeseries(\code{FALSE}, default).
#' @param aggregate_monthly Return results as averages for the 12 months of the
#'   year? Defaults to \code{FALSE}.
#' @param funs Functions to evaluate. By default, computes \code{min, mean, 
#' median and max}.
#' @param drop Drop existing timeseries in \code{runoff_ts}, \code{discharge_ts},
#'   or not.  
#' @param ... Additional arguments passed to \code{funs}.
#' @param verbose Indicate progress, or not. Defaults to \code{FALSE}.
#' 
#' @return Returns the input \code{HS} object, or a list, where 
#'   runoff/downscaled/discharge is replaced with the computed summaries.
#' 
#' @export
ensemble_summary <- function(HS, 
                             summarise_over_timeseries = FALSE, 
                             aggregate_monthly = FALSE,
                             funs=c("min","mean","median","max"), 
                             drop=FALSE,
                             ...,
                             verbose = FALSE) {
    UseMethod("ensemble_summary")
}

#' @export
ensemble_summary.list <- function(HS,
                                  summarise_over_timeseries = FALSE, 
                                  aggregate_monthly = FALSE, 
                                  funs=c("min","mean","median","max"),
                                  drop = FALSE,
                                  ...,
                                  verbose = FALSE) {
    Date <- NULL
    Month <- NULL
    Pred <- NULL
    Value <- NULL
    Stat <- NULL
    Prediction <- NULL
    
    total <- length(HS)
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    for (seg in seq_along(HS)) {
        data <- HS[[seg]]
        
        if (summarise_over_timeseries) {
            if (aggregate_monthly) {
                data <- data %>%
                    tibble::as_tibble(.name_repair = "minimal") %>%
                    dplyr::mutate(Month = lubridate::month(Date)) %>%
                    dplyr::group_by(Month) %>%
                    dplyr::select(-Date) %>%
                    dplyr::summarise_all(.funs=funs, na.rm=TRUE) %>%
                    tidyr::gather(Pred, Value,-Month) %>%
                    dplyr::mutate(Stat = stringr::word(Pred, -1, sep="_"),
                                  Prediction = stringr::str_replace(Pred, 
                                                    paste0("_", Stat), "")) %>%
                    dplyr::select(Month, Prediction, Stat, Value)
            } else {
                data <- data %>%
                    tibble::as_tibble(.name_repair = "minimal") %>%
                    dplyr::select(-Date) %>%
                    dplyr::summarise_all(.funs=funs, ...) %>%
                    tidyr::gather(Pred, Value) %>%
                    dplyr::mutate(Stat = stringr::word(Pred, -1, sep="_"),
                                  Prediction = stringr::str_replace(Pred, 
                                                    paste0("_", Stat), "")) %>%
                    dplyr::select(Prediction, Stat, Value)
            }
            
        } else {
            data <- data %>% 
                tidyr::gather(Prediction, Value, -Date)
            
            if (aggregate_monthly) {
                data <- data %>%
                    tibble::as_tibble(.name_repair = "minimal") %>%
                    dplyr::mutate(Month = lubridate::month(Date)) %>%
                    dplyr::group_by(Month) %>%
                    dplyr::select(-Date,-Prediction) %>%
                    dplyr::summarise_all(.funs=funs) 
            } else {
                data <- data %>%
                    tibble::as_tibble(.name_repair = "minimal") %>%
                    dplyr::group_by(Date) %>%
                    dplyr::select(-Prediction) %>%
                    dplyr::summarise_all(.funs=funs) 
            }
            if (!drop) {
                data <- dplyr::left_join(HS[[seg]], data, by="Date")
            }
            
        }
        
        
        HS[[seg]] <- data
        if(verbose) setTxtProgressBar(pb, seg)
    }
    
    if(verbose) close(pb)
    
    return(HS)
}



#' @export
ensemble_summary.HS <- function(HS,
                                summarise_over_timeseries = FALSE,
                                aggregate_monthly = FALSE,
                                funs=c("min","mean","median","max"), 
                                drop = FALSE,
                                ...,
                                verbose = FALSE) {
    
    runoff <- hasName(HS, "runoff_ts")
    discharge <- hasName(HS, "discharge_ts")
    
    if (runoff) {
        data <- HS$runoff_ts
        data <- ensemble_summary(data, 
                                 summarise_over_timeseries,
                                 aggregate_monthly,
                                 funs,
                                 drop = drop,
                                 ...,
                                 verbose = verbose)
        if(summarise_over_timeseries) {
            HS$runoff_summary <- data
        } else {
            HS$runoff_ts <- data
        }
        
    }
    
    if (discharge) {
        data <- HS$discharge_ts
        data <- ensemble_summary(data, 
                                 summarise_over_timeseries,
                                 aggregate_monthly,
                                 funs,
                                 drop = drop,
                                 ...)
        if(summarise_over_timeseries) {
            HS$discharge_summary <- data
        } else {
            HS$discharge_ts <- data
        }
        
    }
    
    HS <- reorder_cols(HS)
    HS <- assign_class(HS, "HS")
    return(HS)
}

