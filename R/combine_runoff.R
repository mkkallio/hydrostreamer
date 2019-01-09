#' Combines runoff in HS* objects
#' 
#' The function provides the ability to combine runoff from \code{HSgrid}, 
#' \code{HSweights}, \code{HSrunoff}, or \code{HSflow} using user provided
#' weights. Function can process several input weight sets at once. 
#' 
#' If a named list of weights is provided to the function, their names will be 
#' preserved in the output. Otherwise the combinations are named in a running
#' order.
#' 
#' Note that if the timeseries of dates is different in each runoff timeseries
#' in the input, the output includes only those dates which are present in 
#' every input timeseries.
#' 
#' @param HS An \code{HS*} object
#' @param weights A vector of weights with the same length as the number of
#'   runoff timeseries in \code{HS}, or a list of weight vectors.
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
combine_runoff <- function(HS, weights=NULL) {
    UseMethod("combine_runoff")
}



#' @export
combine_runoff.list <- function(HS, weights) {
        
    if(!is.list(weights)) weights <- list(combination = weights)
    
    wnames <- names(weights)
    if(is.null(wnames)) wnames <- paste0("combination",1:length(weights))

    dates <- lapply(HS, FUN = function(x) x$Date)
    dates <- Reduce(intersect, dates) %>% 
        lubridate::as_date()
    
    output <- list()
    for(w in seq_along(weights)) {
        for(i in seq_along(HS)) {
            if(i==1) {
                n <- ncol(HS[[i]])
                dateind <- HS[[i]]$Date %in% dates
                out <- HS[[i]][dateind,2:n]*as.numeric(weights[[w]][i])
            } else {
                n <- ncol(HS[[i]])
                dateind <- HS[[i]]$Date %in% dates
                out <- out + HS[[i]][dateind,2:n]*as.numeric(weights[[w]][i])
            }
        }
        out <- cbind(Date = dates, out)
        output[[ wnames[w] ]] <- out
    }
    
    
    return(output)
    
}

#' @export
combine_runoff.HSflow <- function(HS, weights) {
    
    comb <- combine_runoff.list(HS$discharge, weights)
    HS$discharge <- comb
    
    return(HS)
}

#' @export
combine_runoff.HSrunoff <- function(HS, weights) {
    
    comb <- combine_runoff.list(HS$downscaled, weights)
    HS$downscaled <- comb
    
    return(HS)
}

#' @export
combine_runoff.HSgrid <- function(HS, weights) {
    
    comb <- combine_runoff.list(HS$runoff, weights)
    HS$runoff <- comb
    
    return(HS)
}

#' @export
combine_runoff.HSweights <- function(HS, weights) {
    
    comb <- combine_runoff.list(HS$grid$runoff, weights)
    HS$grid$runoff <- comb
    
    return(HS)
}