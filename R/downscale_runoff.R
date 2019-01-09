#' Computes specific runoff generated in the river segment.
#' 
#' Computes downscaled, unrouted, river segment specific runoff using 
#' the given \code{HSweights} object. 
#' 
#' 
#' @param HSweights An object of class 'HSweights', obtained with 
#'   \code{compute_HSweights}, or constructed with function
#'   \code{create_HSweights}.
#' @param unit Unit of runoff. Can be either "mm/s", or "m3/s". 
#'   Defaults to mm/s (~ equivalent to kg/m2/s).
#' @param rID Name of the column in river with unique IDs.
#' @param wID Name of the column in weights with IDs corresponding to rID.
#'
#' @return The routed river network object which has been enhanced with 
#'   a runoff timeseries. Runoff is given in \eqn{m^3/s}.
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(hydrostreamer)
#' 
#' # load data
#' data(river)
#' data(basin)
#' runoff <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))
#' 
#' # create HSgrid
#' grid <- polygrid_timeseries(grid, aoi=basin)
#' 
#' # weights based on length of river segments
#' len <- compute_weights(river, grid, "length", aoi=basin, riverID="ID")
#' 
#' # compute segment specific runoff
#' runoff <- compute_segment_runoff(len, verbose=TRUE)
#' }
#' 
#' @export
downscale_runoff <- function(HSweights, 
                             rID = "riverID", 
                             wID = "riverID", 
                             unit = "mm/s") {
    
    if(!any("HSweights" %in% class(HSweights))) {
        stop("Input should be of class HSweights.")
    }
    
    area_m2 <- NULL
    gridID <- NULL
    Date <- NULL
    
    river <- HSweights$river
    weights <- HSweights$weights
    grid <- HSweights$grid
    
    output <- list(river = river, downscaled = list())
    
    ngrids <- length(grid$runoff)
    
    nriv <- NROW(river)
    nseg <- NROW(weights)
    ng <- NROW(grid$grid)
    
    rIDs <- dplyr::select_(river, rID) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    gIDs <- dplyr::select(grid$grid, gridID) %>%
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
    gridareas <- dplyr::select(grid$grid, area_m2) %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    
    for (g in seq_along(grid$runoff)) {
        
        nts <- nrow(grid$runoff[[g]])
    
        runoffTS <- dplyr::select(grid$runoff[[g]], -Date) %>%
            as.matrix() 
        
        QTS <- matrix(0, nrow = nts, ncol = nriv)
        
        if (unit == "mm/s") convert <- TRUE
        if (unit == "m3/s") convert <- FALSE
        
        if(convert) {
            for (seg in 1:nseg) {
                QTS[, wrIDs[seg] ] <- weightvec[seg] * 
                    runoffTS[, wgIDs[seg] ] * 
                    gridareas[ wgIDs[seg] ] / 1000
                
            }
        } else {
            for (seg in 1:nseg) {
                QTS[, wrIDs[seg] ] <- weightvec[seg] * 
                    runoffTS[, wgIDs[seg] ]
            }
        }
        
        
        QTS <- QTS %>% data.frame()
        colnames(QTS) <- rIDs
        QTS$Date <- grid$runoff[[g]]$Date 
        QTS <- dplyr::select(QTS, Date, dplyr::everything())
        
        
        if(is.null(names(grid$runoff))) {
            output$downscaled[[g]] <- QTS
        } else {
            name <- names(grid$runoff)[g]
            output$downscaled[[ name ]] <- QTS
        }
        
    }
    
    class(output) <-  c("HSrunoff", class(output))
    return(output)
}
