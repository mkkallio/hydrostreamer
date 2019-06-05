#' Generates routing information for river routing algorithms
#'
#' The function adds connectivity information to the input \emph{river} object. 
#' The function assumes that the input river is a "clean" river network: 
#' connected segments share a node at the start or end of the segment. 
#' Digitizing direction is required to be the direction of flow.
#'
#' @inheritParams compute_HSweights
#'
#' @return Returns the river network with class 'HSnetwork' with the following attributes:
#' \itemize{
#'   \item riverID: Unique (renamed) river segment identifier.
#'   \item PREVIOUS: a list of riverID(s) of the previous river segment(s).
#'   \item NEXT: riverID of the segment where the river flows to.
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
#' routed_river <- river_network(river, riverID = "ID", verbose=TRUE)
#' 
#' }
#' 
#' @export
river_network <- function(river, riverID = "riverID", verbose=FALSE) {
    
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
    
    
    #######
    # Create assisting variables 
    #######
    
    IDs <- dplyr::select_(river, riverID) %>% 
        sf::st_set_geometry(NULL) %>%
        unlist()
    nSegments <- NROW(river)
    FROM <- vector("list", nSegments)
    #FROM_ALL <- vector("list", nSegments)
    TO <- vector("list", nSegments)
    #TO_ALL <- vector("list", nSegments)
    nup <- rep(1, nrow(river))
    
    
    #First part: get starting and ending coordinates
    p <- 0
    start <- list()
    end <- list()
    
    
    coords <- river %>% 
        sf::st_cast("LINESTRING", warn=FALSE) %>% 
        sf::st_coordinates()
    
    for (i in 1:nSegments) {
        segcoords <- coords[coords[,"L1"] == i,]
        start[[i]] <- segcoords[1,1:2]
        end[[i]] <- segcoords[NROW(segcoords),1:2]
    }
    start <- t(as.data.frame(start))
    end <- t(as.data.frame(end))
    
    #Second part: match the coordinates to find previous and next segments
    total <- nSegments+1
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
    
    river$NEXT <- unlist(TO)
    river$PREVIOUS <- FROM
    
    for(i in 1:nSegments) {
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
        all <- which(IDs %in% all)
        nup[all] <- nup[all] +1
        
        if(verbose) setTxtProgressBar(pb, i)
    }

    remove <- c(riverID)
    remove <- names(river) %in% remove
    river <- river[, !remove]
    river$riverID <- IDs
    river$UP_SEGMENTS <- nup
    river <- dplyr::select(river, riverID, PREVIOUS, NEXT, 
                           UP_SEGMENTS, dplyr::everything()) %>%
        tibble::as_tibble(river) %>%
        sf::st_as_sf()
    
    if(verbose) setTxtProgressBar(pb, nSegments+1)
    if(verbose) close(pb)
    
    
    river <- reorder_cols(river)
    river <- assign_class(river, c("HSnetwork", "HS"))
    
    return(river)
}

