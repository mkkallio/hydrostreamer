#' Find all upstream segments from a specified river segment.
#' 
#' Function returns the river network consisting of all upstream segments 
#' from a specified river segment.
#' 
#' @param HSnetwork a \code{HSnetwork} (routed river network) object obtained
#'   with \code{\link{river_network}}.
#' @param ID  The ID of the segment for which the upstream rivershed is returned.
#' @inheritParams compute_HSweights
#' 
#' @return A \code{HSnetwork} object (a routed river network) consisting of the 
#' segments upstream from the segment specified. 
#' 
#' @export
upstream <- function(HSnetwork, ID, riverID = "riverID") {
    
    if(!any(class(HSnetwork) == "HSnetwork")) {
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


