#' Computes the maximum upstream network length
#' 
#' Function returns the maximum length of upstream river network for each 
#' river segment in the network.
#' 
#' @param HS a \code{HS} object obtained passed through
#'    \code{\link{river_network}}.
#' @param values  A vector of values to be aggregated, or a name of the column
#'   in \code{HS} storing the values. If the input is a \code{units} object,
#'   the unit is preserved.
#' @param output_col Column name in \code{HS} where the collected values will
#'   be stored. if NULL, the function returns the collected values as a vector.
#' @inheritParams compute_HSweights
#' 
#' @return A vector of the collected stats, or the input \code{HS} with a 
#'   specified column storing the vector. 
#' 
#' @export
compute_network_length <- function(HS, 
                                   values, 
                                   output_col = NULL,
                                   verbose = FALSE) {
    
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
    
    max_length <- values
    
    if(verbose) pb <- txtProgressBar(min = 0, max = n, style = 3)
    prog <- 0
    for(i in order) {
        nxt <- nextriver[i]

        test <- !is.na(nxt)
        if(test) {
            l <- values[i]
            nl <- values[nxt]
            nml <- max_length[nxt]
            ml <- nml - nl
            
            # maximum length
            test <- l > ml
            if(test) {
                max_length[nxt] <- nl + l
            } 
            
        }
        prog <- prog + 1
        if(verbose) setTxtProgressBar(pb, prog)
    }
    if(verbose) close(pb)
    
    test <- is.null(unit)
    if(!test) out <- units::as_units(max_length, unit)
    
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
