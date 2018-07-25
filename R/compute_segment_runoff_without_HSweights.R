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
                                                     timesteps = NULL, unit = "mm/s") {
    
    area_m2 <- NULL
    gridID <- NULL
    if(!any(class(grid) == "HSgrid")) {
        stop("grid input should be of class HSgrid, obtained with function polygrid_timeseries()")
    }
    
    if(!any(class(river) == "sf")) {
        stop("river input should be an 'sf' LINESTRING object")
    }
    
    # PREPARE INPUT FOR FORTRAN SUBROUTINE
    nriv <- NROW(river)
    nseg <- NROW(weights)
    ng <- NROW(grid)
    if (is.null(timesteps)) {
        message("No timesteps specified: computing for all timesteps")
        timesteps <- 1:(NCOL(grid)-3)
    }
    nts <- length(timesteps)
    
    
    rIDs <- dplyr::select_(river, rID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    gIDs <- dplyr::select(grid, gridID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    
    wrIDs <- dplyr::select_(weights, wID) %>% 
        sf::st_set_geometry(NULL) %>%
        unlist() %>%
        match(rIDs)
    
    wgIDs <- dplyr::select(weights, gridID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist() %>%
        match(gIDs)
    
    
    weightvec <- dplyr::select(weights, weights) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    gridareas <- dplyr::select(grid, area_m2) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    runoffTS <- dplyr::select(grid, -c(gridID, area_m2)) %>%
        sf::st_set_geometry(NULL) %>% as.matrix()
    runoffTS <- runoffTS[,timesteps]
    #runoffTS <- runoffTS*1e6
    
    QTS <- matrix(0, nrow = nriv, ncol = nts)
    
    if (unit == "mm/s") convert <- TRUE
    if (unit == "m3/s") convert <- FALSE
    
    QTS <- .Fortran("compute_runoff", 
                    PACKAGE = "hydrostreamer",
                    as.integer(nriv),
                    as.integer(nseg),
                    as.integer(ng),
                    as.integer(nts),
                    as.integer(wrIDs),
                    as.integer(wgIDs),
                    as.double(weightvec),
                    as.double(gridareas),
                    runoffTS,
                    QTS,
                    as.logical(convert),
                    as.double(rep(0,nseg)),
                    as.integer(rep(0,nseg)))[[10]]
    
    
    colnames(QTS) <- paste0("TS", timesteps)
    
    Q_ts <- dplyr::select_(river, rID) %>% dplyr::rename_("riverID" = rID) %>% cbind(QTS)
    
    # add class to return object, and add specific columns from river object
    class(Q_ts) <- append(class(Q_ts), "HSrunoff")
    Q_ts <- tibble::add_column(Q_ts, PREVIOUS = river$PREVIOUS, NEXT = river$NEXT, DOWNSTREAM = river$DOWNSTREAM,
                               .after=1)
    return(Q_ts)
}