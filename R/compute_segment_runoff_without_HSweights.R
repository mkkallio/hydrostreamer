#' Computes specific runoff generated in the river segment without using a HSweights object.
#' 
#' This function can be used to compute downscaled, segment specific, unrouted, runoff manually from a selection
#' of input data. This can be used if for some reason not all information required in previous steps is available.
#' See details below. Called by \code{\link{compute_segment_runoff}}.
#' 
#' The minimum information needed to compute segment specific runoff are:
#' \itemize{
#'   \item{river}{At minimum a data.frame with unique IDs for each river segment}
#'   \item{weights}{At minimum, a data.frame with columns specifying the river ID corresponding to \code{river}, 
#'   grid ID corresponding to \code{grid}, and weight used for multiplying runoff value in \code{grid}}
#'   \item{grid}{A \code{HSgrid} object obtained with \code{\link{polygrid_timeseries}}}
#' }
#' In practise using this function is not recommended, and instead the \code{HSweights} object should be constructed
#' and used.
#'
#' @param river River network information. 
#' @param weights With ID and weights information.
#' @param grid A \code{HSgrid} object obtained with \code{\link{polygrid_timeseries}}
#' @param rID Name of the column in \code{river} with unique IDs.
#' @param wID Name of the column in \code{weights} with IDs corresponding to rID.
#' @inheritParams compute_segment_runoff
#'
#' @return Returns input river object (class 'HSrunoff') added with timeseries information as new attribute columns.
#'
#' @examples 
#' \dontrun{
#' #to be added
#' } 
#' 
#' @export
compute_segment_runoff_without_HSweights <- function(river, weights, grid, rID = "riverID", wID = "riverID",
                                                     timesteps = NULL, unit = "mm/s", verbose = FALSE) {
    
    area_m2 <- NULL
    
    if(!any(class(grid) == "HSgrid")) {
        stop("grid input should be of class HSgrid, obtained with function polygrid_timeseries()")
    }
    
    if(!any(class(river) == "sf")) {
        stop("river input should be an 'sf' LINESTRING object")
    }
    
    # Start processing
    nweights <- NROW(weights)
    nCells <- NROW(grid)
    if (is.null(timesteps)) {
        message("No timesteps specified: computing for all timesteps")
        timesteps <- 1:(NCOL(grid)-3)
    }
    
    runoff <- dplyr::select(grid, -c(gridID, area_m2)) %>%
        sf::st_set_geometry(NULL)
    
    
    #prepare a table to collect time series
    Q_ts <- dplyr::select_(river, rID) %>% dplyr::rename_("riverID" = rID)
    
    #weights id's corresponding river ids
    wID <- dplyr::select_(weights, wID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    rID <- dplyr::select_(river, rID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    #process every timestep, but first initiate progress bar
    total <- length(timesteps)
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    for (ts in timesteps) {
        
        #initiate discharge of the current timestep
        Q <- vector("numeric", NROW(river))
        #process every segment
        for (seg in 1:nweights) {
            
            #compute discharge of the segment at timestep
            cell <- weights$gridID[seg]
            discharge <- NULL
            gridID <- which(grid$gridID == cell)
            riverID <- which(rID == wID[seg])
            
            weight <- weights$weights[seg]
            area <- grid$area_m2[gridID]
            
            if (unit == "mm/s") {
                discharge <- runoff[gridID,ts]/1000 * area * weight
            }
            if (unit == "m3/s") {
                discharge <- runoff[gridID,ts] * weight
            }
            
            #add discharge to the segment
            Q[riverID] <- Q[riverID] + unclass(discharge)
            #Q <- unlist(Q)
        }
        Q_ts <- cbind(Q_ts, Q)
        col <- length(names(Q_ts))-1
        names(Q_ts)[col] <- paste0("TS",ts)
        if (verbose) setTxtProgressBar(pb, ts)
    }
    if (verbose) close(pb)
    
    # add class to return object, and add specific columns from river object
    class(Q_ts) <- append(class(Q_ts), "HSrunoff")
    Q_ts <- tibble::add_column(Q_ts, PREVIOUS = river$PREVIOUS, NEXT = river$NEXT, DOWNSTREAM = river$DOWNSTREAM,
                               .after=1)
    return(Q_ts)
}
