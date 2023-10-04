#' Generates routing information for river routing algorithms
#'
#' The function adds connectivity information to the input \emph{river} object. 
#' The function assumes that the input river is a "clean" river network: 
#' connected segments share a node at the start or end of the segment. 
#'
#' @inheritParams compute_HSweights
#' @param next_down Column with the ID(s) of the next segment rivers flows to
#' @param previous Column with the ID(s) of the previous segment river flows 
#'   from
#' @param na_value Value in column \code{next_down} and \code{previous} 
#'   signifying no downstream/upstream segments
#' @param force if the river already has routing information, overwrite it?
#'
#' @return Returns the river network with class 'HSnetwork' with the following 
#'   attributes:
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
river_network <- function(river, 
                          next_down = NULL,
                          previous = NULL,
                          na_value = NULL,
                          force = FALSE,
                          riverID = "riverID", 
                          verbose = FALSE) {
    
    PREVIOUS <- NULL
    NEXT <- NULL
    DOWNSTREAM <- NULL
    UP_SEGMENTS <- NULL
    
    if(is.null(next_down) && !is.null(previous)) {
        stop("non-null previous must be accompanied with non-null next_down.")
    }
    
    # check whether river is already routed?
    attrs <- get_HS_attr(river)
    if(!is.null(attrs)) {
        test <- all(is.na(attrs))
        if(!test && !force) stop(paste0("Routing information already exist. ",
                                        "Use force=TRUE to overwrite."))
    }
    
    #inspect input
    if(!any(class(river) == "sf")) {
        stop("river input should be an 'sf' object")
    }
 
    if(!any(names(river) == riverID)) {
        stop(paste0("ID column '", riverID,"' does not exist"))
    }
    
    if(is.null(next_down)) nodes <- TRUE else nodes <- FALSE
    
    if(!nodes){
        test <- is.character(next_down)
        if(!test) stop("next_down must be type character, giving a column name.")
        test <- hasName(river, next_down)
        if(!test) stop(paste0("Column ", next_down, " not found in river"))
        test <- is.null(na_value)
        if(test) stop("na_value must be provided with next_down")
    }
    
    
    
    #######
    # Create assisting variables 
    #######
    
    IDs <- dplyr::pull(river, riverID)
    nSegments <- NROW(river)
    FROM <- vector("list", nSegments)
    #FROM_ALL <- vector("list", nSegments)
    TO <- vector("list", nSegments)
    #TO_ALL <- vector("list", nSegments)
    nup <- rep(0, nrow(river))
    
    if(verbose) message("Creating routing information..")
    
    ##########
    # create NEXT, PREVIOUS from intersecting nodes, or from provided next_down
    # column. 
    if(verbose) pb <- txtProgressBar(min = 0, max = nSegments*2, style = 3)
    if(nodes) {
        #First part: get starting and ending coordinates
        p <- 0
        start <- vector("list", nSegments)
        end <- list("list", nSegments)
        
        
        coords <- river %>% 
            sf::st_cast("MULTILINESTRING", warn=FALSE) %>% 
            sf::st_coordinates()
        
        for (i in 1:nSegments) {
            segcoords <- coords[coords[,"L2"] == i,]
            start[[i]] <- segcoords[1,1:2]
            end[[i]] <- segcoords[NROW(segcoords),1:2]
        }
        start <- t(as.data.frame(start))
        end <- t(as.data.frame(end))
        
        #Second part: match the coordinates to find previous and next segments
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
    } else if(is.null(previous)) {
        # when only next_down provided
        
        TO <- dplyr::pull(river, next_down) 
        if(!is.null(na_value)) {
            TO[TO == na_value] <- -9999
        }
        TO <- as.list(TO)

        for(i in 1:nSegments) {
            id <- IDs[i]
            from <- sapply(TO, function(x) x == id)
            if(sum(from) == 0) {
                FROM[[i]] <- -9999
            } else {
                FROM[[i]] <- IDs[from]
            }
            if(verbose) setTxtProgressBar(pb, i)
        }
        
    } else {
        # if both next_down and previous are provided
        TO <- dplyr::pull(river, next_down) 
        if(!is.null(na_value)) {
            TO[TO == na_value] <- -9999
        }
        TO <- as.list(TO)
        
        FROM <- dplyr::pull(river, previous) 
        if(!is.null(na_value)) {
            FROM[FROM == na_value] <- -9999
        }
        TO <- as.list(TO)
        # if(verbose) setTxtProgressBar(pb, i)
    }
    # if(verbose) close(pb)
    names(TO) <- IDs
    names(FROM) <- IDs
    
    river$NEXT <- TO
    river$PREVIOUS <- FROM
    
    
    #######################
    #######################
    # get number of upstream river segments
    if(verbose) pb <- txtProgressBar(min = 0, max = nSegments*2, style = 3)
    for(i in 1:nSegments) {
        to <- TO[[i]]
        n <- 0
        all <- list()
        while(all(to != -9999)) {
            n <- n+1
            
            nextID <- which(IDs %in% to)
            # if(length(nextID) == 0) {
            #     nextID <- -9999
            #     break
            # }
            all[[n]] <- IDs[nextID]
            to <- unlist(lapply(seq_along(nextID), function(x) TO[nextID[x]]))
            #to <- TO[[nextID]]
            if(all(to == -9999) || is.null(to)) {
                break
            }
        }
        all <- unlist(all)
        all <- which(IDs %in% all)
        nup[all] <- nup[all] + 1
        
        if(verbose) setTxtProgressBar(pb, nSegments + i)
    }
    if(verbose) close(pb)
   ########################
    #######################

    #remove <- c(riverID)
    #remove <- names(river) %in% remove
    #river <- river[, !remove]
    river$riverID <- IDs
    river$UP_SEGMENTS <- nup
    river <- dplyr::select(river, riverID, NEXT, PREVIOUS, 
                           UP_SEGMENTS, dplyr::everything()) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        sf::st_as_sf()
    
    
    
    river <- reorder_cols(river)
    river <- assign_class(river, c("HS"))
    river <- mod_HS_attributes(river, next_col = TRUE, col= "NEXT")
    river <- mod_HS_attributes(river, prev_col = TRUE, col = "PREVIOUS")
    
    return(river)
}

