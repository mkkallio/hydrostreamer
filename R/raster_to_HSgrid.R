#' Polygonizes a raster and adds cell values as attribute columns to 
#' an 'sf' polygon object.
#'
#' @param raster A Raster* object.
#' @param aoi Area of interest as an 'sf' polygon object, 
#'   'SpatialPolygons', or 'SpatialPolygonsDataFrame'. Optional, but 
#'   recommended for all applications.
#' @param timestep Length of timestep in raster, e.g. \code{"hour"}, 
#' \code{"day"}, \code{"month"}
#'
#' @return Returns an 'sf' polygon object, with each polygon representing 
#'   a raster cell, and which has been cropped to the area of interest. 
#'   Columns are ID, area of the polygon in m^2, and each timestep as separate
#'   column. Output has class 'HSgrid'.
#'   
#' @export
raster_to_HSgrid <- function(raster, date, timestep = NULL, aoi = NULL, name=NULL) {
    
    . <- NULL
    gridID <- NULL
    area_m2 <- NULL
    
    # test input
    test <- !any(class(raster) %in% c("RasterLayer", "RasterStack", "RasterBrick"))
    if (test) stop("raster needs to be a Raster* object")
    test <- class(date) != "Date"
    if(test) stop("date input should be a 'Date' object (use e.g. 
                   date('2000/01/01') ), or a vector of dates with 
                   length equal to number of timesteps in runoff")
    test <- length(date) == 1 && is.null(timestep)
    if(test) stop("Please provide timestep")
    test <- length(date) != 1 && length(date) != raster::nlayers(raster)
    if(test) stop("length(date) != nlayers(raster)")
    test <- !is.null(timestep) && !timestep %in% c("day","month")
    if(test) stop("Only 'day' or 'month' timesteps supported.")
    
    
    
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
            stop("Input area of interest should be an object of spatial class 
                 from either 'sf' or 'sp' packages")
        }
    }
    
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
    grid$gridID <- 1:NROW(grid)
    grid$area_m2 <- sf::st_area(grid)
    
    output <- list()
    output[["grid"]] <- dplyr::select(grid, gridID, area_m2)
    
    data <- dplyr::select(grid, -area_m2, -gridID) %>% 
        st_set_geometry(NULL) %>% 
        t() %>% 
        data.frame()
    colnames(data) <- grid$gridID
    rownames(data) <- NULL
    
    # process dates
    if(length(date) == 1) {
        enddate <- date %m+% months(raster::nlayers(raster) -1)
        date <- seq(date, enddate, by = timestep)
    } 
    data$Date <- date
    
    output[["runoff"]] <- list()
    if(is.null(name)) {
        output[["runoff"]][[1]] <- dplyr::select(data, Date, dplyr::everything())
    } else {
        output[["runoff"]][[name]] <- dplyr::select(data, Date, dplyr::everything())
    }

    
    # assign class
    class(output) <- c("HSgrid", class(output))
    
    #return
    return(output)
}

