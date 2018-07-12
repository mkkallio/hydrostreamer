
#' Generates neighbour information from a connected, directed river network.
#'
#' The function adds connectivity information to the input \emph{river} object. The function assumes 
#' that the input river is a "clean" river network: connected segments share a node at the start or 
#' end of the segment. Digitizing direction is required to be the direction of flow.
#'
#' @inheritParams compute_weights
#'
#' @return Returns the river network with class 'HSrnet' with the following attributes:
#' \itemize{
#'   \item riverID: Unique (renamed) river segment identifier.
#'   \item PREVIOUS: a list of riverID(s) of the previous river segment(s).
#'   \item NEXT: riverID of the segment where the river flows to.
#'   \item DOWNSTREAM: A list of riverID(s) of all the river segments downstream for the current segment.
#'   \item UP_SEGMENTS: the number of river segments upstream.
#' }
#' 
#' @examples 
#' \dontrun{
#' library(hydrostreamer)
#' 
#' # load data
#' data(river)
#' 
#' routed_river <- flow_network(river, riverID = "ID", verbose=TRUE)
#' 
#' }
#' 
#' @export
flow_network <- function(river, riverID = "riverID", verbose=FALSE) {
    
    PREVIOUS <- NULL
    NEXT <- NULL
    DOWNSTREAM <- NULL
    UP_SEGMENTS <- NULL
    
    #inspect input
    if(!any(class(river) == "sf")) {
        stop("river input should be an 'sf' LINESTRING object")
    }
    
    if(!any(names(river) == riverID)) {
        stop(paste0("ID column '", riverID,"' does not exist"))
    }
    
    IDs <- dplyr::select_(river, riverID) %>% 
        sf::st_set_geometry(NULL) %>%
        unlist()
    nSegments <- NROW(river)
    FROM <- vector("list", nSegments)
    #FROM_ALL <- vector("list", nSegments)
    TO <- vector("list", nSegments)
    TO_ALL <- vector("list", nSegments)
    nup <- rep(1, nrow(river))
    
    
    #First part: get starting and ending coordinates
    if(verbose) message("Processing part 1/3: seeking adjacent segments")
    p <- 0
    start <- list()
    end <- list()
    
    coords <- sf::st_coordinates(river)
    
    for (i in 1:nSegments) {
        segcoords <- coords[coords[,"L1"] == i,]
        start[[i]] <- segcoords[1,1:2]
        end[[i]] <- segcoords[NROW(segcoords),1:2]
    }
    start <- t(as.data.frame(start))
    end <- t(as.data.frame(end))
    
    #First part: match the coordinates to find previous and next segments
    total <- nSegments
    if(verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    for (i in 1:nSegments) {
        # to which river segments river flows from
        x <- end[i,1] == start[,1]
        y <- end[i,2] == start[,2]
        source <- which(x & y)
        
        if (length(source)==0) {
            TO[[i]] <- -9999
        } else {
            TO[[i]] <- as.numeric(IDs[source])
        }
        
        # from which river segments river flows to
        x <- start[i,1] == end[,1]
        y <- start[i,2] == end[,2]
        source <- which(x & y)
        if (length(source)==0) {
            FROM[[i]] <- -9999
        } else {
            FROM[[i]] <- as.numeric(IDs[source])
        }
        
        if(verbose) setTxtProgressBar(pb, i)
    }
    if(verbose) close(pb)
    
    # collect ALL from and ALL to nodes for each river segment
    if(verbose) cat("\n")
    if(verbose) message("Processing part 2/3: collecting all upstream and downstream segments")
    total <- nSegments
    if(verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    for (i in 1:nSegments) {
        to <- TO[[i]]
        n <- 0
        all <- list()
        while(to != -9999) {
            n <- n+1
            
            nextID <- which(IDs == to)
            if(length(nextID) == 0) nextID <- -9999
            all[[n]] <- IDs[nextID]
            to <- TO[[nextID]]
            if(to == -9999){
                break
            }
        }
        all <- unlist(all)
        TO_ALL[[i]] <- all
        
        all <- which(IDs %in% all)
        nup[all] <- nup[all] +1
        
        if(verbose) setTxtProgressBar(pb, i)
    }
    
    if(verbose) close(pb)
    
    TO <- unlist(TO)
    
    # if the last features in river are river outlets (next ID == -9999), the TO_ALL list will be shorter
    # than nrow(river). If this is the case, grow TO_ALL with lists containing NULL
    if(length(TO_ALL) != nrow(river)) {
        for (i in 1:(nrow(river)-length(TO_ALL)))
            ind <- length(TO_ALL)
            TO_ALL[[ ind+i ]] <- list(NULL)
    }
    
    #  process output
    #cat("\n")
    if(verbose) message("Processing part 3/3: output")
    remove <- c(riverID, "NEXT", "PREVIOUS", "DOWNSTREAM")
    remove <- names(river) %in% remove
    river <- river[, !remove]
    river$riverID <- IDs
    river$PREVIOUS <- FROM
    river$NEXT <- TO
    river$DOWNSTREAM <- TO_ALL
    river$UP_SEGMENTS <- nup
    river <- dplyr::select(river, riverID, PREVIOUS, NEXT, DOWNSTREAM, UP_SEGMENTS, dplyr::everything())
    class(river) <- append(class(river), "HSnetwork")
    
    return(river)
}

