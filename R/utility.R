#' Switch the runoff grid in HSweights object
#' 
#' The functions changes the runoff grid in a \code{HSweights} object to another. Some simple tests are made to
#' the replacing \code{HSgrid} object to ensure that they are equivalent to the original one. The main use of this
#' function is to make it easy to compare multiple runoff products without the need to compute weights everytime.
#' 
#' The replacing grid must have the same spatial objects as in the one being replaced. This is because the weights
#' are calculated using spatial relatioships between the river segments (or segment specific catchments) and the
#' areal units in runoff time series. If the runoff unit areas change, so does the weighting, and in this case 
#' weights need to be recomputed using \code{\link{compute_weights}}.
#' 
#' @param HSweights A \code{HSweights} object obtained with \code{\link{compute_weights}}.
#' @param gridID Column name in \code{grid} containing unique IDs.
#' @inheritParams compute_weights
#'
#' @return \code{HSweights} object with a replaced \code{HSgrid} element.
#'
#' @export
switch_grid <- function(HSweights, grid, gridID = "gridID") {
    if(!any(class(grid) == "HSgrid")) {
        stop("grid input should be of class HSgrid, obtained with function polygrid_timeseries()")
    }
    if(!any(class(HSweights) == "HSweights")) {
        stop("Input should be of class HSweights. Otherwise use function compute_segment_runoff_without_HSweights()")
    }
    
    test_area <- sf::st_equals(HSweights[[3]], grid, sparse=FALSE) %>% apply(1,sum) %>% sum()
    test_area <- test_area == nrow(HSweights[[3]])
    if(!test_area) stop("Replacing grid does not contain all the polygons of HSweights, or the polygons are not equivalent.")
    
    
    if (gridID == "gridID") {
        test_ID <- !any(!HSweights[[2]]$gridID %in% grid$gridID)
    } else {
        ID <- dplyr::select_(grid, gridID) %>% sf::st_set_geometry(NULL) %>% unlist()
        test_ID <- !any(!HSweights[[2]]$gridID %in% ID)
    }
    if (!test_ID) stop("Replacing grid has ID's which do not match existing grid ID's in original grid.")
    
    if (test_area && test_ID) HSweights[[3]] <- grid else stop('Replacing grid is not equivalent to existing one.')
}



#' Find all upstream segments from a point
#' 
#' Function returns the river network consisting of all upstream segments from a specified river segment.
#' 
#' @param HSnetwork a \code{HSnetwork} (routed river network) object obtained with \code{\link{flow_network}}.
#' @param ID  The ID of the segment for which the upstream rivershed is returned.
#' @inheritParams compute_weights
#' 
#' @return A \code{HSnetwork} object (a routed river network) consisting of the segments upstream from the 
#' segment specified. 
#' 
#' @export
upstream <- function(HSnetwork, ID, riverID = "riverID") {
    
    if(!any(class(river) == "HSnetwork")) {
        stop("river input should be of class HSnetwork, obtained with function flow_network()")
    }
    
    if(!any(names(HSnetwork) == riverID)) {
        stop(paste0("ID column '", riverID,"' does not exist"))
    }
    
    sel <- HSnetwork %>% dplyr::filter(riverID == ID)
    
    if (nrow(sel) == 0) stop("ID ", ID, " does not exist in column ", riverID, ".")
    
    test <- lapply(HSnetwork$DOWNSTREAM, FUN= function(x) {ID %in% x}) %>% unlist()
    
    id <- HSnetwork %>% dplyr::select_(riverID) %>% sf::st_set_geometry(NULL) %>% unlist()
    row <- which(id== ID)
    test[row] <- TRUE
    HSnetwork$NEXT[row] <- -9999 
    
    
    HSnetwork <- HSnetwork[test,]
    return(HSnetwork)
}


