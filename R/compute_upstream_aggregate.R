#' Computes an aggeregate value for all upstream segments
#' 
#' Function returns the river network consisting of all upstream segments 
#' from the specified river segment.
#' 
#' @param HS a \code{HS} object obtained passed through
#'    \code{\link{river_network}}.
#' @param values  A vector of values to be aggregated, or a name of the column
#'   in \code{HS} storing the values. If the input is a \code{units} object,
#'   the unit is preserved.
#' @param fun Function applied to the collected values upstream. The function
#'   must take \code{na.rm} argument.
#' @param output_col Column name in \code{HS} where the collected values will
#'   be stored. if NULL, the function returns the collected values as a vector.
#' @param ... Additional arguments passed to \code{fun}.
#' @inheritParams compute_HSweights
#' 
#' @return A vector of the collected stats, or the input \code{HS} with a 
#'   specified column storing the vector. 
#' 
#' @export
compute_upstream_aggregate <- function(HS, 
                                       values, 
                                       fun = sum,
                                       output_col = NULL,
                                       verbose = FALSE,
                                       ...) {
    
    riverID <- NULL
    UP_SEGMENTS <- NULL
    
    
    # --------------------------------------------------------------------------
    # test input
    
    test <- inherits(HS, "HS")
    if(!test) stop("HS input should be of class 'HS'.")
    
    test <- is.character(values)
    if(test) {
        test <- hasName(HS, values)
        if(test) {
            values <- dplyr::pull(HS, values)   
        } else {
            stop("Couldn't find column ", values, " in HS.")
        }
    }
    if(inherits(values, "units")) {
        unit <- units::deparse_unit(values)
        values <- units::drop_units(values)
    } else {
        unit <- NULL
    }
    
    
    test <- is.function(fun)
    if(!test) stop("Input 'fun' should be a function.")
    
    
    # --------------------------------------------------------------------------
    # process
    
    n <- nrow(HS) 
    
    IDs <- HS$riverID
    
    order <- HS %>%
        dplyr::select(riverID, UP_SEGMENTS) %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::arrange(UP_SEGMENTS) %>%
        dplyr::select(riverID) %>%
        unlist() %>%
        match(IDs)
    
    ind <- find_attribute(HS, "next_col", TRUE)
    nextriver <- dplyr::pull(HS, ind) %>%
        match(IDs)
    
    
    collected <- matrix(NA, nrow = n, ncol = n)
    diag(collected) <- values
    
    if(verbose) pb <- txtProgressBar(min = 0, max = n, style = 3)
    for(i in seq_along(order)) {
        ii <- order[i]
        NEXT <- nextriver[ii]
        val <- values[ii]
        
        while(!is.na(NEXT)) {
            collected[ii,NEXT] <- val
            NEXT <- nextriver[NEXT]
        }
        
        if(verbose) setTxtProgressBar(pb, i)
    }
    
    
    out <- apply(collected,2,fun, na.rm=TRUE, ...)
    if(verbose) close(pb)
    
    test <- is.null(unit)
    if(!test) out <- units::as_units(out, unit)
    
    test <- is.null(output_col)
    if(test) {
        return(out)
    } else {
        output <- dplyr::mutate(HS, !!output_col := out, 
                                .before = "UP_SEGMENTS")
        
        output <- reorder_cols(output)
        output <- assign_class(output, "HS")
        return(output)
    }
}
