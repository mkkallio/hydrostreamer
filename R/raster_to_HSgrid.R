#' Create HS from a raster
#' 
#' Polygonizes a raster and adds the values in the raster stack to a data frame
#' in list column \code{runoff_ts}.
#'
#' @param rasters A raster object, or a list of raster objects, or a list of
#'   URIs to files readable by \code{raster} package.
#' @param unit Unit of the timeseries, coercable with \code{units::set_units()}.
#' @param date Date of the first layer in \code{raster}, or a vector of dates 
#'   with the length of layers in \code{raster}, or a list of vectors of dates,
#'   corresponding to the dates in each raster layer.
#' @param timestep Length of timestep in raster. Currently supporting 
#'   \code{"hour"}, \code{"day"}, \code{"month"}. Only needed if \code{date} is
#'   not a vector of dates.
#' @param aoi Area of interest as an 'sf' polygon object, 
#'   'SpatialPolygons', or 'SpatialPolygonsDataFrame'. Optional, but 
#'   recommended for all applications.
#' @param names A name (names) for the runoff timeserie(s).
#' @param verbose Print progress indication or not.
#'
#' @return See \code{\link{create_HS}}.
#'   
#' @export
raster_to_HS <- function(rasters, 
                         unit,
                         date, 
                         timestep = NULL, 
                         aoi = NULL, 
                         names = NULL,
                         verbose = FALSE) {
    
    . <- NULL
    Date <- NULL
    zoneID <- NULL
    
    # -------------------------------------------------------------------------
    # TEST
    
    # test rasters input
    test <- is.list(rasters)
    if(!test) {
        test2 <- is.character(rasters)
        if(test2) {
            rasters <- as.list(rasters)
        } else {
            test3 <- any(class(rasters) %in% 
                             c("RasterStack", "RasterBrick", "RasterLayer"))
            if(test3) {
                rasters <- list(rasters)
            } else {
                stop("rasters input must be either a raster or an ",
                     "URI to a raster")
            }
        }
    }
    
    
    # test date and timestep object
    test <- length(date) == 1 && is.null(timestep)
    if(test) stop("Please provide timestep")
    
    test <- length(date) == 1 && class(date) != "Date"
    if(test) stop("date input should be a 'Date' object (use e.g. 
                  date('2000/01/01') ), or a vector of dates with 
                  length equal to number of timesteps in runoff")
    
    test <- !is.list(date) && length(date) == 1
    if(test) date <- as.list(rep(date, length(rasters)))
    
    test <- !is.list(date) && length(date) != 1
    if(test) date <- as.list(date)
    
    test <- length(date) == length(rasters)
    if(!test) stop("length(date) != length(rasters)")
    
    test <- !is.null(timestep) && !timestep %in% c("day","month")
    if(test) stop("Only 'day' or 'month' currently timesteps supported.")
    
    # test names
    nrasters <- length(rasters)
    test <- is.null(names) 
    if(test) {
        names <- paste0("runoff_", seq(1,nrasters,by=1))
    } else {
        test2 <- length(names) == nrasters
        if(!test2) stop("length(names) != length(rasters)")
    }
    
    total <- length(rasters)
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3) 
    
    
    #---------------------------------------------------------------------------
    # PROCESS DATA
    
    # process all rasters
    for(rast in seq_along(rasters)) {
        
        # load the raster in question
        test3 <- any(class(rasters[[rast]]) %in% 
                         c("RasterStack", "RasterBrick", "RasterLayer"))
        if(!test3) {
            raster <- raster::brick(rasters[[rast]])
        } else {
            raster <- rasters[[rast]]
        }
        
        # see if aoi exists, whether it is an 'sf' or 'sp', and crop
        if (!is.null(aoi)) {
            
            # if aoi is an 'sf' object, else if its an 'sp' object, else stop
            accepted <- c("sf", "sfc", "sfg")
            if( any(class(aoi) %in% accepted) ) {
                aoi <- methods::as(aoi, "Spatial")
                raster <- raster::crop(raster, aoi, snap="out")
                aoi <- sf::st_as_sf(aoi)
            } else if ( grepl("Spatial", class(aoi), fixed=TRUE) ) {
                raster <- raster::crop(raster, aoi)
                aoi <- sf::st_as_sf(aoi)
            } else {
                stop("Input area of interest should be an object of spatial",
                    " class from either 'sf' or 'sp' packages")
            }
        }
        
        # polygonize
        grid <- raster::rasterToPolygons(raster)
        
        
        # change class to 'sf', intersect grid to aoi, if exists
        if(!is.null(aoi)) {
            grid <- suppressWarnings(
                suppressMessages(
                    methods::as(grid, "sf") %>%
                        sf::st_intersection(., sf::st_union(aoi))
                )
            )
        } else {
            grid <- methods::as(grid, "sf")
        }
        
        # create the output
        grid$zoneID <- 1:NROW(grid)
        
        data <- dplyr::select(grid, -zoneID) %>% 
            sf::st_set_geometry(NULL) %>% 
            t() %>% 
            data.frame()
        colnames(data) <- grid$zoneID
        rownames(data) <- NULL
        
        # process dates
        dates <- date[[rast]]
        
        if(length(dates) == 1) {
            if(timestep == "month") {
                enddate <- dates %m+% months(raster::nlayers(raster) -1)
            } else if(timestep == "day") {
                enddate <- dates %m+% 
                             lubridate::days(raster::nlayers(raster) -1)
            } else if(timestep == "hour") {
                enddate <- dates %m+% 
                             lubridate::hours(raster::nlayers(raster) -1)
            }
            dates <- seq(dates, enddate, by = timestep)
        }  else {
            test <- length(dates) == nlayers(raster)
            if(test) stop(paste0("length(dates) != nlayers(raster) for raster",
                                 " number ",rast))
        }
        data$Date <- dates
        
        grid <-  dplyr::select(grid, zoneID)
        
        if(rast == 1) {
            output <- create_HS(grid, data, name = names[[rast]], unit = unit)
        } else {
            add <- create_HS(grid, data, name = names[[rast]], unit = unit)
            output <- add_HS(output, add)
         
            
        }
        if (verbose) setTxtProgressBar(pb, rast)
        }
    if(verbose) close(pb)
    
    
    #return
    return(output)
}

