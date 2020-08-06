#' Find all upstream segments from a specified river segment.
#' 
#' Function returns the river network consisting of all upstream segments 
#' from the specified river segment.
#' 
#' @param HSnetwork a \code{HSnetwork} (routed river network) object obtained
#'   with \code{\link{river_network}}.
#' @param ID  The ID of the segment for which the upstream river network is 
#'   returned.
#' @inheritParams compute_HSweights
#' 
#' @return The input \code{HSnetwork} object consisting of only upstream 
#'   segments.
#' 
#' @export
upstream <- function(HSnetwork, ID, riverID = "riverID") {
    

    if(!any(names(HSnetwork) == riverID)) {
        stop(paste0("ID column '", riverID,"' does not exist"))
    }
    
    rIDs <- dplyr::select(HSnetwork, riverID) %>% unlist
    prevs <- HSnetwork$PREVIOUS
    
    
    # row of river segment upstream is starting from
    curr <- which(rIDs == ID)
    
    # set NEXT of curr to -9999 (it'll be outlet)
    HSnetwork$NEXT[curr] <- -9999
    
    # upstream is a logical vector specifying all upstream segments
    upstream <- vector("logical", nrow(HSnetwork))
    upstream[curr] <- TRUE
    
    
    # start visit list and associated positions
    visitlist <- vector("list", nrow(HSnetwork))
    visitposition <- 1
    vislistpos <- 1
    
    # Initialize visitlist
    prev <- unlist(prevs[curr]) 
    for(i in seq_along(prev)) {
        visitlist[[vislistpos]] <- prev[i]
        vislistpos <- vislistpos + 1
    }
    
    # visit all the river segments in visit list. Record their upstream segs,
    # until all upstream segments have been visited.
    visit <- visitlist[[1]]
    while(!is.null(visit)) {
        
        visID <- visitlist[[visitposition]]
        IDi <- which(rIDs == visID)
        upstream[IDi] <- TRUE
        
        prev <- unlist(prevs[IDi])
        for(i in seq_along(prev)) {
            if(prev[i] != -9999) {
                visitlist[[vislistpos]] <- prev[i]
                vislistpos <- vislistpos + 1
            }
        }
        
        visitposition <- visitposition + 1
        visit <- visitlist[[visitposition]]
        
    }
    
    # select all upstream segments
    upstream <- HSnetwork[upstream,]
    
    return(upstream)
}

#' Find all downstream segments from a specified river segment.
#' 
#' Function returns the river network consisting of all river segments directly
#' downstream from the specified segment.
#' 
#' @param HSnetwork a \code{HSnetwork} (routed river network) object obtained
#'   with \code{\link{river_network}}.
#' @param ID  The ID of the segment for which the downstream river network is 
#'   returned.
#' @inheritParams compute_HSweights
#' 
#' @return A \code{HSnetwork} object (a routed river network) consisting of the 
#' segments downstream from the segment specified. 
#' 
#' @export
downstream <- function(HSnetwork, ID, riverID = "riverID") {

    . <- NULL
    
    if(!any(names(HSnetwork) == riverID)) {
        stop(paste0("ID column '", riverID,"' does not exist"))
    }
    
    rIDs <- dplyr::select(HSnetwork, riverID) %>% unlist
    nexts <- HSnetwork$NEXT
    
    
    # row of river segment downstream is starting from
    curr <- which(rIDs == ID)
    
    # set PREVIOUS of curr to -9999 (it'll be the most upstream segment)
    #HSnetwork$PREVIOUS[curr] <- -9999
    
    # downstream is a logical vector specifying all downstream segments
    downstream <- vector("logical", nrow(HSnetwork))
    downstream[curr] <- TRUE
    
    
    # start visit list and associated positions
    visitlist <- vector("list", nrow(HSnetwork))
    visitposition <- 1
    vislistpos <- 1
    
    # Initialize visitlist
    nxt <- unlist(nexts[curr]) 
    for(i in seq_along(nxt)) {
        visitlist[[vislistpos]] <- nxt[i]
        vislistpos <- vislistpos + 1
    }
    
    # visit all the river segments in visit list. Record their downstream segs,
    # until all downstream segments have been visited.
    visit <- visitlist[[1]]
    while(!is.null(visit)) {
        
        visID <- visitlist[[visitposition]]
        IDi <- which(rIDs == visID)
        downstream[IDi] <- TRUE
        
        nxt <- unlist(nexts[IDi])
        for(i in seq_along(nxt)) {
            visitlist[[vislistpos]] <- nxt[i]
            vislistpos <- vislistpos + 1
        }
        
        visitposition <- visitposition + 1
        visit <- visitlist[[visitposition]]
        
    }
    visitlist <- unlist(visitlist)[-sum(downstream)] %>% match(rIDs) %>%
        c(which(rIDs == ID),.)
    
    # select all downstream segments
    downstream <- HSnetwork[visitlist,]
    
    return(downstream)
}


