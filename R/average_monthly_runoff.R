
# MONTHLY AVERAGE

#' Monthly average of a runoff timeseries. Expects the timeseries to start from January and end in December.
#'
#' @param x A 'HSgrid' object obtained with \code{\link{polygrid_timeseries}}, or a 'RasterStack', or a 'RasterBrick'.
#' @inheritParams polygrid_timeseries
#'
#' @return Returns either a 'HSgrid' object with monthly summary of the timeseries, or a 'RasterBrick',
#'   depending on the class of input.
#'   
#'   
#' @export
average_monthly_runoff <- function(x) {
    UseMethod("average_monthly_runoff")
}


#' @export
average_monthly_runoff.HSgrid <- function(x) {
    message("Assuming first timestep to be January, and finishing in December.")
    ts <- dplyr::select(x, -gridID, -area_m2) %>% sf::st_set_geometry(NULL)
    n_ts <- NCOL(ts)
    n_year <- NCOL(ts)/12
    
    for (month in 1:12) {
        mts <- seq(month, (n_ts-12+month), by=12)
        mts <- ts[,mts]
        
        if (month == 1) {
            monthly_runoff <- apply(mts, 1, mean)
        } else {
            mrro <- apply(mts, 1, mean)
            monthly_runoff <- cbind(monthly_runoff, mrro)
        }
    }
    colnames(monthly_runoff) <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
    
    x <- dplyr::select(x, gridID, area_m2)
    x <- cbind(x, monthly_runoff)
    class(x) <- append(class(x),"HSgrid")
    return(x)
}

#######
# METHODS
#######

#' @export
average_monthly_runoff.RasterBrick <- function(x) {
    message("Assuming first timestep to be January, and finishing in December.")
    n_ts <- raster::nlayers(x)
    n_year <- n_ts/12
    
    for (month in 1:12) {
        mts <- seq(month, (n_ts-12+month), by=12)
        
        if (month == 1) {
            monthly_runoff <- raster::calc(x[[mts]], mean)
        } else {
            mrro <- raster::calc(x[[mts]], mean)
            monthly_runoff <- raster::addLayer(monthly_runoff, mrro)
        }
    }
    x <- raster::brick(monthly_runoff)
    return(x)
}


#' @export
average_monthly_runoff.RasterStack <- function(x) {
    message("Assuming first timestep to be January, and finishing in December.")
    n_ts <- raster::nlayers(x)
    n_year <- n_ts/12
    
    for (month in 1:12) {
        mts <- seq(month, (n_ts-12+month), by=12)
        
        if (month == 1) {
            monthly_runoff <- raster::calc(x[[mts]], mean)
        } else {
            mrro <- raster::calc(x[[mts]], mean)
            monthly_runoff <- raster::addLayer(monthly_runoff, mrro)
        }
    }
    x <- raster::brick(monthly_runoff)
    return(x)
}
