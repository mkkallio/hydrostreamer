#' Computes a reference discharge
#' 
#' Function returns the river network consisting of all upstream segments 
#' from the specified river segment.
#' 
#' @param timeseries Which timeseries to apply \code{f} to. Eithr "runoff",
#' "discharge", "observation", or "control".
#' @inheritParams compute_upstream_aggregate
#' 
#' @return A vector of the collected stats, or the input \code{HS} with a 
#'   specified column storing the vector. 
#' 
#' @export
compute_hydrological_signatures <- function(HS, 
                                            fun = mean,
                                            timeseries = "discharge",
                                            output_col = NULL,
                                            verbose = FALSE,
                                            ...) {
    
    Date <- NULL
    
    
    # --------------------------------------------------------------------------
    # test inputs
    
    test <- inherits(HS, "HS")
    if(!test) stop("HS input should be of class 'HS'.")
    
    test <- is.function(fun)
    if(!test) stop("Input 'fun' should be a function.")
    
    ts <- paste0(timeseries, "_ts")
    test <- hasName(HS, ts)
    if(!test) stop("Column ", ts, " not found in HS input.")
    
    # --------------------------------------------------------------------------
    # process
    
    n <- nrow(HS)
    
    data <- dplyr::pull(HS, ts)
    out <- vector("list", n)
    names(out) <- names(data)
    
    # record units if any
    test <- dplyr::pull(data[[1]], 2) %>% 
        inherits("units")
    if(test) {
        units <- sapply(data[[1]][,-1], units::deparse_unit)
    } else {
        units <- NULL
    }
    
    if(verbose) pb <- txtProgressBar(0, n, style = 3)
    for(i in 1:n) {
        
        
        
        if(is.null(units)) {
            
            out[[i]] <- data[[i]] %>% 
                dplyr::select(-Date) %>% 
                dplyr::summarise_all(.funs = fun, ...)
            
        } else {
            
            tbl <- dplyr::select(data[[i]], -Date)
            
            for(ii in seq_along(tbl)) {
                tbl[,ii] <- units::drop_units(tbl[,ii])
            }
            
            tbl <- dplyr::summarise_all(tbl, .funs = fun, ...)
            
            for(ii in seq_along(tbl)) {
                tbl[,ii] <- units::as_units(dplyr::pull(tbl,ii), units[ii])
            }
            
            out[[i]] <- tbl
        }
        
        if(verbose) setTxtProgressBar(pb, n)
    }
    if(verbose) close(pb)
    
    if(is.null(output_col)) {
        return(out)
    } else {
        output <- dplyr::mutate(HS, !!output_col := out, 
                                .before = "UP_SEGMENTS")
        
        output <- reorder_cols(output)
        output <- assign_class(output, "HS")
        return(output)
    }
}
