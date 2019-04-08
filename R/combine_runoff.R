#' Combines runoff in HS* objects
#' 
#' The function provides the ability to combine runoff from \code{HSgrid}, 
#' or \code{HS} using user provided weights. Function can process several input 
#' weight sets at once. 
#' 
#' If a named list of weights is provided to the function, their names will be 
#' preserved in the output. Otherwise the combinations are named in a running
#' order.
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
#' \code{discharge_ts}, if they exist. Columns \code{observation_ts} and 
#' \code{control_ts} are left untouched if present.
#' 
#' @param HS An \code{HS} object
#' @param weights A vector of weights with the same length as the number of
#'   runoff timeseries in \code{HS}, or a list of weight vectors.
#' @param intercept If weights are associated with an intercept, include it here.
#'   Default intercept = 0.
#' @param bias Bias in percentage. Defaults to \code{NULL}.
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
#'   combined <- combine_runoff(HSgrid, weights)
#'   
#'   listweights <- list(weights1 = c(0.2,0.2,0.3,0.3,0),
#'                   weights2 = c(0.1,0.2,0.3,0.2,0.2),
#'                   thirdw = c(0,0,0,0.5,0.5))
#'  combined2 <- combine_runoff(HSgrid, listweights)
#' }
#' 
#' 
#' @export
combine_runoff <- function(HS, 
                           weights, 
                           intercept = 0,
                           bias = NULL,
                           drop = FALSE, 
                           monthly = FALSE) {
    UseMethod("combine_runoff")
}



#' @export
combine_runoff.list <- function(HS, 
                                weights, 
                                intercept = 0,
                                bias = NULL,
                                drop = FALSE, 
                                monthly = FALSE) { 
    
    Date <- NULL
    
    
    
    if(!is.list(weights) || is.data.frame(weights)) {
        weights <- list(combination = weights)
    }
    wnames <- names(weights)
    if(is.null(wnames)) wnames <- paste0("combination",1:length(weights))
    
    if (is.null(bias)) {
        bias_correct <- FALSE
    } else {
        bias_correct <- TRUE   
    } 
    
    ### if combination is done for each month
    if (monthly) {
        
        # process every river segment
        for(seg in seq_along(HS)) {
            
            data <- matrix(NA, ncol = length(weights), nrow = nrow(HS[[seg]]))
            colnames(data) <- wnames
            
            # process every set of weights
            for(w in seq_along(weights)) {
                modelind <- which(colnames(HS[[seg]]) %in% colnames(weights[[w]]), 
                                  arr.ind=TRUE)
                
                for(m in 1:12) {
                    mw <- unlist(weights[[w]][m,])
                    intercept <- mw[1]
                    mw[is.na(mw)] <- 0
                    
                    # make sure weights and timeseries match in order
                    order <- match(colnames(HS[[seg]])[modelind], names(mw))
                    mw <- c(intercept, mw[order])
                    
                    month <- lubridate::month(HS[[seg]]$Date) == m
                    p <- t(cbind(1,as.matrix(HS[[seg]][month,modelind])))
                    result <- as.vector(mw %*% p)
                    
                    if(bias_correct) {
                        result <-result / (1 - bias[w,m] / 100)
                    }
                    data[month,w] <- result
                }
            }
            data <- tibble::as_tibble(data)
            
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
        
        # if combination is done for the entire timeseries
    } else {
        
        # process every river segment
        for(seg in seq_along(HS)) {
            
            data <- matrix(NA, ncol = length(weights), nrow = nrow(HS[[seg]]))
            colnames(data) <- wnames
            for(w in seq_along(weights)) {
                modelind <- which(colnames(HS[[seg]]) %in% names(weights[[w]]), 
                                  arr.ind=TRUE)
                
                wmean <- apply(HS[[seg]][,modelind], 1, weighted.mean, 
                               weights[[w]][-1])
                wmean <- wmean + weights[[w]][1]
                
                if (bias_correct) {
                    wmean <- wmean / (1 - bias[[w]] / 100)
                }
                data[,w] <- wmean
            }
            data <- tibble::as_tibble(data)
            
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

#' @export
combine_runoff.HS <- function(HS, 
                              weights, 
                              intercept = 0,
                              bias = NULL,
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

