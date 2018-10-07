#' Writes any HS* object as a single geopackage where each input 
#' runoff is written as it's own layer.
#'
#' @param x A \code{HSgrid}, \code{HSrunoff}, or a \code{HSflow} object.
#' @param filename Filename to write to.
#' @param ... options passed to \code{\link[sf]{st_write}}.
#' 
#' @export
HSwrite <- function(x, filename, ...) {
    UseMethod("HSwrite")
}


#' @export
HSwrite.HSgrid <- function(x, filename, ...) {
    
    n <- length(x$runoff)
    names <- names(x$runoff)
    if(is.null(names)){
        names <- paste0("runoff", seq(1,n,by=1))
    }
    
    for (i in seq_along(x$runoff)) {
        
        if (any(colnames(x$runoff[[i]]) == "Date")) {
            dates <- x$runoff[[i]]$Date
            rdata <- x$runoff[[i]] %>% 
                dplyr::select(-Date) %>%
                t()
            colnames(rdata) <- as.character(dates)
            
        } else {
            dates <- x$runoff[[i]]$Month
            rdata <- x$runoff[[i]] %>% 
                dplyr::select(-Month) %>%
                t()
            colnames(rdata) <- as.character(dates)
        }
        
        layer <- cbind(x$grid, rdata)
       
        if(i == 1) {
            sf::st_write(layer, filename, layer=names[[i]], driver="GPKG", ...)
        } else {
            sf::st_write(layer, filename, layer=names[[i]], update=TRUE, driver="GPKG", ...)
        }
    }
}

#' @export
HSwrite.HSrunoff <- function(x, filename, ...) {
    
    n <- length(x$downscaled)
    names <- names(x$downscaled)
    if(is.null(names)){
        names <- paste0("downscaled_runoff", seq(1,n,by=1))
    }
    
    for (i in seq_along(x$downscaled)) {
        
        if (any(colnames(x$downscaled[[i]]) == "Date")) {
            dates <- x$downscaled[[i]]$Date
            rdata <- x$downscaled[[i]] %>% 
                dplyr::select(-Date) %>%
                t()
            colnames(rdata) <- as.character(dates)
            
        } else {
            dates <- x$downscaled[[i]]$Month
            rdata <- x$downscaled[[i]] %>% 
                dplyr::select(-Month) %>%
                t()
            colnames(rdata) <- as.character(dates)
        }
        
        layer <- cbind(x$river, rdata)
        
        if(i == 1) {
            sf::st_write(layer, filename, layer=names[[i]], driver="GPKG", ...)
        } else {
            sf::st_write(layer, filename, layer=names[[i]], update=TRUE, driver="GPKG", ...)
        }
    }
}


#' @export
HSwrite.HSflow <- function(x, filename, ...) {
    
    n <- length(x$discharge)
    names <- names(x$discharge)
    if(is.null(names)){
        names <- paste0("discharge", seq(1,n,by=1))
    }
    
    for (i in seq_along(x$discharge)) {
        
        if (any(colnames(x$discharge[[i]]) == "Date")) {
            dates <- x$discharge[[i]]$Date
            rdata <- x$discharge[[i]] %>% 
                dplyr::select(-Date) %>%
                t()
            colnames(rdata) <- as.character(dates)
            
        } else {
            dates <- x$discharge[[i]]$Month
            rdata <- x$discharge[[i]] %>% 
                dplyr::select(-Month) %>%
                t()
            colnames(rdata) <- as.character(dates)
        }
        
        layer <- cbind(x$river, rdata)
        
        if(i == 1) {
            sf::st_write(layer, filename, layer=names[[i]], driver="GPKG", ...)
        } else {
            sf::st_write(layer, filename, layer=names[[i]], update=TRUE, driver="GPKG", ...)
        }
    }
}

# to be added...
HSwrite.HSobs <- function(x, filename, ...) {}

# to be added...
HSwrite.HSoptim <- function(x, filename, ...) {}