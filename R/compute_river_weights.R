#' Compute weights for river segments within runoff area features.
#' 
#' Computes weights for each individual river segments falling in the 
#' areal units of the runoff \emph{HS}. Weight is computed from a 
#' numerical property of segments by \emph{x/sum(x)} where x are the 
#' river segments contained in a areal unit of runoff (\emph{HS}). 
#' This function is called by \code{\link{compute_HSweights}}.
#' 
#' \emph{seg_weights} should be one of the following: "equal", "length", 
#'   "strahler", or a numeric vector which specifies the weight for each 
#'   river segment. 
#'   \itemize{
#'     \item \emph{equal} option assigns equal weights to all river segments 
#'       within a runoff unit.
#'     \item \emph{length} option weights river segments within a runoff unit 
#'       based on the length of the segment.
#'     \item \emph{strahler} option weights river segments based on the 
#'       Strahler number computed for the supplied river network.
#'     \item A numeric vector with length equal to the number of features 
#'       in \emph{river}. 
#' }
#' 
#' @param grid A \code{HS} object or an \code{sf POLYGON} object used for
#'   areal interpolation.
#' @param seg_weights A character vector specifying type of weights, or a 
#'   numerical vector. See Details. Defaults to "length".
#' @param split Whether or not to use \code{\link{split_river_with_grid}} 
#'   before computing weights. Default is TRUE (assuming the river has not
#'   already been split)
#' @inheritParams compute_HSweights
#'
#'
#' @return Returns an 'sf' linestring object with attributes:
#'   \itemize{
#'     \item \emph{ID}. Unique ID of the feature.
#'     \item \emph{riverID}. ID of the river each segment is associated to.
#'     \item \emph{zoneID}. ID of the runoff unit the river segment is contained in.
#'     \item \emph{weights}. Weights computed for each river segment.
#' }
#'   
compute_river_weights <- function(river, 
                                  grid, 
                                  seg_weights = NULL, 
                                  riverID = "riverID", 
                                  split=TRUE) {
    
    weights <- NULL
    ID <- NULL
    zoneID <- NULL
    runoff_ts <- NULL
    line_length_corr <- NULL

    if(!any(colnames(river) == riverID)) stop("riverID column '", 
                                           riverID, 
                                           "' does not exist in river input")
    if(riverID != "riverID") river <- dplyr::rename(river, 
                                                     riverID = riverID)  
    
    if("runoff_ts" %in% colnames(grid)) grid <- dplyr::select(grid, -runoff_ts)

    if(is.null(seg_weights)) {
        dasymetric <- FALSE
    } else {
        dasymetric <- TRUE
        test <- hasName(river, seg_weights)
        if(!test) stop("No column ", seg_weights," in river input")
        
        test <- sum(is.null(river[,seg_weights]))
        test2 <- sum(is.na(river[,seg_weights]))
        if(test+test2 > 0) stop("Missing values in column ", seg_weights)
    }
    
    ##############
    # preprocess
    
    # split river
    if(split) river <- split_river_with_grid(river,
                                             grid,
                                             riverID = riverID)
    
    #get elements of rivers intersecting polygons
    riverIntsc <- suppressWarnings(
                    suppressMessages(
                        sf::st_contains(grid,
                                        river,
                                        sparse=FALSE)))
    
    ##############
    # get weights
    
    if(split) {
        len <- dplyr::pull(river, line_length_corr)
        if(inherits(len, "units")) len <- units::drop_units(len)
    } else {
        len <- sf::st_length(river)
        len[units::drop_units(len) < 1] <- units::set_units(1, "m")
    }
        
    if(dasymetric) {
        # get the dasymetric variable/segment weights
        dasymetric_var <- sf::st_set_geometry(river, NULL) %>%
            dplyr::pull(seg_weights)
        
        weight <- apply(riverIntsc,1, compute_dasymetric_weights, 
                        len, dasymetric_var) %>%
            apply(1, FUN=sum) %>% 
            unlist()
        
    } else {
        
        weight <- apply(riverIntsc,1, compute_segment_weights, 
                        len) %>% 
            apply(1, FUN=sum) %>% 
            unlist()
    }

    
    ###############
    # process output
    
    if (any(names(river) == "weights")) {
        message("Replacing existing 'weights' column")
        river <- dplyr::select(river, -weights)
    }
    
    river <- tibble::add_column(river, weights = weight) %>%
        dplyr::select(ID, riverID, zoneID, weights) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        sf::st_as_sf()
    
    return(river)
}
