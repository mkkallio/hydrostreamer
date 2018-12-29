#' @export
combine_discharge <- function(HS, weights=NULL) {
    UseMethod("combine_discharge")
}



#' @export
combine_discharge.list <- function(HS, weights) {
        
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
combine_discharge.HSflow <- function(HS, weights) {
    
    comb <- combine_discharge.list(HS$discharge, weights)
    HS$discharge <- comb
    
    return(HS)
}

#' @export
combine_discharge.HSrunoff <- function(HS, weights) {
    
    comb <- combine_discharge.list(HS$downscaled, weights)
    HS$downscaled <- comb
    
    return(HS)
}

#' @export
combine_discharge.HSgrid <- function(HS, weights) {
    
    comb <- combine_discharge.list(HS$runoff, weights)
    HS$runoff <- comb
    
    return(HS)
}

#' @export
combine_discharge.HSweights <- function(HS, weights) {
    
    comb <- combine_discharge.list(HS$grid$runoff, weights)
    HS$grid$runoff <- comb
    
    return(HS)
}