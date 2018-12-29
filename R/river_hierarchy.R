#' Computes river hierarchies for a HSnetwork object.
#'
#' Computes river hierarchies from a routed river network (\emph{HSnetwork} 
#' object), or from an unrouted, connected river network. In case the input 
#' river network is unrouted, \code{\link{river_network}} is run before 
#' computing river hierarchy.
#'
#' @param type Type hierarchy to compute. Currently only \code{strahler} 
#'   stream order implemented.
#' @inheritParams compute_HSweights
#'
#' @return Returns the river network with added column with the selected river 
#'   hierarchy.
#' 
#' @examples 
#' \dontrun{
#' library(hydrostreamer)
#' library(dplyr)
#' 
#' # load data
#' data(river)
#' 
#' # without prior routing
#' river <- river_hierarchy(river, riverID="ID")
#' 
#' # with routed network
#' routed_river <- flow_network(river, riverID = "ID") %>%
#'     river_hierarchy()
#' }
#' 
#' @export
river_hierarchy <- function(river, type="strahler", riverID = "riverID") {
    
    if(!any(class(river)=="sf")) {
        stop("river input should be of class 'HSnetwork' obtained with function 
             flow_network() or an 'sf' linestring object.")
    }
    test <- !"HSnetwork" %in% class(river)
    if (test) river <- river_network(river, riverID=riverID)
    
    from <- river$PREVIOUS
    to <- river$NEXT
    ID <- dplyr::select(river, riverID) %>% 
        sf::st_set_geometry(NULL) %>%    
        unlist()
    
    
    n_seg <- NROW(river)
    strahler <- rep(1, n_seg)
    rounds_with_no_edits <- 0
    edits <- 1
    
    while (rounds_with_no_edits < 5) {
        if (edits == 0) rounds_with_no_edits <- rounds_with_no_edits+1
        if (rounds_with_no_edits == 5) break
        edits <- 1
        # run for every river segment
        for (seg in 1:n_seg) {
            n_sources <- length(unlist(from[[seg]]))
            # check if the segment is headwaters (no inflowing segments)
            if (n_sources == 1 && unlist(from[[seg]]) == -9999) {
                
            } else if (n_sources == 1 && unlist(from[[seg]]) != -9999) {
                # what to assign if only one inflowing segment
                prev_seg <- as.numeric(unlist(from[seg]))
                row <- ID == prev_seg
                
                # if the current stream order DOES NOT EQUAL TO inflowing 
                # stream order
                if (!strahler[seg] == strahler[row]){ 
                    strahler[seg] <- strahler[row]
                    edits <- edits+1
                }
                
            } else {
                # what to do if more than one inflowing river segment
                prev_segs <- unlist(from[seg])
                
                str <- vector("numeric", length = length(prev_segs))
                # get the strahler number of the incoming river segments
                for(pseg in 1:length(prev_segs)){
                    row <- ID == prev_segs[pseg]
                    str[pseg] <- strahler[row]
                }
                
                max_value <- max(str)
                n_max_values <- table(str)[as.character(max_value)]
                
                
                if (n_max_values == 1) {
                    if (!strahler[seg] == max_value) {
                        strahler[seg] <- max(str)
                        edits <- edits+1
                    }
                } else {
                    if (!strahler[seg] == max_value+1) {
                        strahler[seg] <- max(str)+1
                        edits <- edits+1
                    }
                }
                
            }
        }
        edits <- edits-1
        
    }
    test <- any(names(river) == "STRAHLER")
    if(test) {
        river <- river[,-"STRAHLER"]
        river <- tibble::add_column(river, 
                                    STRAHLER = strahler, 
                                    .before=length(names(river)))
        message("Replacing the existing column 'STRAHLER'.")
    } else {
        river <- tibble::add_column(river, 
                                    STRAHLER = strahler, 
                                    .before=length(names(river)))
    }
    
    return(river)
    
}
