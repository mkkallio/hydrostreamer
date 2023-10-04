#' Combines runoff and/or discharge in HS objects
#' 
#' The function provides the ability to do a weighted combination of  runoff 
#' and/or discharge timeseries in \code{HS} object using user provided weights.
#' Function can process several input weight sets. 
#' 
#' If a named list of weights is provided to the function, their names will be 
#' preserved in the combinations. Otherwise the combinations are named using a 
#' running number.
#' 
#' If \code{intercept} is provided, it is added to the combined timeseries.
#' If \code{bias} is provided, the combined timeseries is adjusted by
#' timeseries / (1-bias/100). 
#'  
#' Note that if the timeseries of dates is different in each runoff timeseries
#' in the input, the output includes only those dates which are present in 
#' every input timeseries.
#' 
#' The combination is only applied to columns \code{runoff_ts}, and 
#' \code{discharge_ts}. Columns \code{observation_ts} and 
#' \code{control_ts} are left untouched if present.
#' 
#' @param HS An \code{HS} object
#' @param weights A vector of weights with the same length as the number of
#'   runoff timeseries in \code{HS}, or a list of weight vectors.
#' @param intercept If weights are associated with an intercept, include it here.
#'   Default intercept = 0.
#' @param bias Bias in percentage. Defaults to 0.
#' @param drop Whether to remove source timeseries and replace them with 
#'   combinations, or keep source timeseries and add combination as new column.
#' @param monthly If provided weights are for each month, set to \code{TRUE}
#'   to apply weights separately for each month. 
#' 
#' @return Returns the input \code{HS} object where runoff timeseries is 
#'   replaced by the the combined timeseries'.
#' 
#' @examples 
#' \dontrun{
#'   weights <- c(0.2,0.2,0.3,0.3,0)
#'   combined <- combine_runoff(HS, weights)
#'   
#'   listweights <- list(weights1 = c(0.2,0.2,0.3,0.3,0),
#'                       weights2 = c(0.1,0.2,0.3,0.2,0.2),
#'                       weights3 = c(0,0,0,0.5,0.5))
#'  combined2 <- combine_runoff(HS, listweights)
#' }
#' 
#' 
#' @export
combine_runoff <- function(HS, 
                           weights, 
                           intercept = 0,
                           bias = 0,
                           drop = FALSE, 
                           monthly = FALSE) {
    UseMethod("combine_runoff")
}


#' @method combine_runoff list 
#' @export
combine_runoff.list <- function(HS, 
                                weights, 
                                intercept = 0,
                                bias = 0,
                                drop = FALSE, 
                                monthly = FALSE) { 
    
    Date <- NULL
    unit <- units::deparse_unit(dplyr::pull(HS[[1]], 2))
    
    if(!is.list(weights)) weights <- list(combination = weights)
    
    # check intercept
    if(length(intercept) != length(weights)) {
        if(length(intercept) == 1) intercept <- rep(intercept, 
                                                    length(weights))
        if(monthly) {
            if(!is.list(intercept)) {
                intercept <- list(intercept)
                intercept <- lapply(intercept, function(x) {
                    if(length(x) == 1) {
                        return(rep(x,12))
                    } else if(length(x) != 12) {
                        return(FALSE)
                    } else {
                        return(x)
                    }
                    
                })
                if(FALSE %in% intercept) stop(paste0("length(intercept) is not",
                                                     " 1 or 12. Pleace check!"))
            }
        }
        
    }
    
    # check bias
    if(length(bias) != length(weights)) {
        if(length(bias) == 1) bias <- rep(bias,length(weights))
        
        if(monthly) {
            if(!is.list(bias)) {
                bias <- list(bias)
                bias <- lapply(bias, function(x) {
                    if(length(x) == 1) {
                        return(rep(x,12))
                    } else if(length(x) != 12) {
                        return(FALSE)
                    } else {
                        return(x)
                    }
                })
                if(FALSE %in% bias) stop(paste0("length(bias) is not",
                                                     " 1 or 12. Pleace check!"))
            }
        }
        
    }
    
    wnames <- names(weights)

    
    ### if combination is done for each month
    if (monthly) {
        
        # process every river segment
        for(seg in seq_along(HS)) {
            
            data <- matrix(NA, ncol = length(weights), nrow = nrow(HS[[seg]]))
            colnames(data) <- wnames
            
            # process every set of weights
            for(w in seq_along(weights)) {
                modelind <- which(colnames(HS[[seg]]) %in% 
                                      colnames(weights[[w]]), 
                                  arr.ind=TRUE)
                
                for(m in 1:12) {
                    mw <- unlist(weights[[w]][m,])
                    int <- intercept[[w]][m]
                    b <- bias[[w]][m]
                    mw[is.na(mw)] <- 0
                    
                    # make sure weights and timeseries match in order
                    order <- match(colnames(HS[[seg]])[modelind], names(mw))
                    mw <- c(int, mw[order])
                    
                    month <- lubridate::month(HS[[seg]]$Date) == m
                    p <- t(cbind(1,as.matrix(HS[[seg]][month,modelind])))
                    result <- as.vector(mw %*% p)
                    
                    data[month,w] <- result
                }
            }
            data <- tibble::as_tibble(data, .name_repair = "minimal")
            
            for(i in 1:ncol(data)) {
                data[,i] <- units::as_units(dplyr::pull(data, i), unit)
            }
            
            if(drop) {
                new <- dplyr::select(HS[[seg]], Date)
                new <- dplyr::bind_cols(new, data)
                colnames(new) <- c("Date", wnames)
                HS[[seg]] <- new
            } else {
                colnames(data) <- wnames
                new <- dplyr::bind_cols(HS[[seg]], data)
                HS[[seg]] <- new
            } 
            
        }
        
        
    } else { # if combination is done for the entire timeseries
        
        # process every river segment
        for(seg in seq_along(HS)) {
            
            data <- matrix(NA, ncol = length(weights), nrow = nrow(HS[[seg]]))
            colnames(data) <- wnames
            for(w in seq_along(weights)) {
                mw <- unlist(weights[[w]])
                int <- intercept[[w]]
                modelind <- which(colnames(HS[[seg]]) %in% names(weights[[w]]), 
                                  arr.ind=TRUE)
                
                order <- match(colnames(HS[[seg]])[modelind], names(mw))
                mw <- c(int, mw[order])
                
                p <- t(cbind(1,as.matrix(HS[[seg]][,modelind])))
                result <- as.vector(mw %*% p)
               
                data[,w] <- result
            }
            data <- tibble::as_tibble(data, .name_repair = "universal")
            
            for(i in 1:ncol(data)) {
                data[,i] <- units::as_units(dplyr::pull(data, i), unit)
            }
            
            if(drop) {
                new <- dplyr::select(HS[[seg]], Date)
                new <- dplyr::bind_cols(new, data)
                colnames(new) <- c("Date", wnames)
                HS[[seg]] <- new
            } else {
                colnames(data) <- wnames
                new <- dplyr::bind_cols(HS[[seg]], data)
                HS[[seg]] <- new
            } 
            
        }
    }
    
    return(HS)
}

#' @method combine_runoff HS 
#' @export
combine_runoff.HS <- function(HS, 
                              weights, 
                              intercept = 0,
                              bias = 0,
                              drop = FALSE, 
                              monthly = FALSE) {
    
    if("discharge_ts" %in% names(HS)) {
        comb <- combine_runoff(HS$discharge_ts, 
                                weights, 
                                intercept = intercept,
                                bias = bias, 
                                drop = drop, 
                                monthly = monthly)
        HS$discharge_ts <- comb
    }
    
    if("runoff_ts" %in% names(HS)) {
        comb <- combine_runoff(HS$runoff_ts, 
                                weights, 
                                intercept = intercept,
                                bias = bias,
                                drop = drop, 
                                monthly = monthly)
        HS$runoff_ts <- comb
    }
    
    HS <- reorder_cols(HS)
    HS <- assign_class(HS, "HS")
    return(HS)
}

