#' Delineate multiple catchments from multiple input points
#'
#' This algorithm delineates multiple catchments from input point 
#' locations and a drainage direction layer. Catchments are delineated 
#' upstream from the point given, until another given outlet point is 
#' met. The result is a set of catchments which are unique to the points 
#' given.
#' 
#'
#' @param outlets An 'sf' point object, with locations of the catchment 
#' outlets for delineation. Obtained e.g. with \code{\link{river_outlets}}
#' @param riverID Name of the column in outlets with unique identifiers.
#' @param output Whether to return a raster or vectors of delineated catchments. 
#' Accepts "vector", "v", "raster", or "r".
#' @inheritParams river_outlets
#' @inheritParams compute_HSweights
#'
#' @return If output is \code{vector} (default), returns vectorized (polygon) 
#'   catchments specific to the outlet  points given. The output is 'sf' 
#'   POLYGON class with attributes:
#' \itemize{
#'   \item riverID: The ID given in \code{outlets}.
#'   \item NCELLS: The number of raster cells the catchment consists of.
#'   \item AREA_M2: Surface area of the catchment in m^2. 
#' }
#' If output is \code{raster}, returns a raster with delineated areas.
#' 
#' @examples 
#' \dontrun{
#' # to be added
#' }
#'
#' @export
delineate_basin <- function(outlets, 
                            drain.dir, 
                            riverID = "riverID", 
                            output = "vector", 
                            verbose = FALSE) {
    
    #do checks
    if (sf::st_is(outlets[1,], "LINESTRING")) {
        outlets <- river_outlets(outlets, drain.dir)
    }
    
    if (!output %in% c("vector", "raster", "v", "r")) {
        stop("output-argument must be either 'vector', 'v', 'raster', or 'r'")
    }
    
    #prepare data and delineate
    if (verbose) message("Preparing..")
    rp <- raster::cellFromXY(drain.dir, sf::st_coordinates(outlets))
    outlets$cell <- rp
    outlets <- outlets[!is.na(outlets$cell),]
    ID <- outlets %>% dplyr::select_(riverID) %>% sf::st_set_geometry(NULL) %>% 
        unlist()
    ny <- nrow(drain.dir)
    nx <- ncol(drain.dir)
    nseeds <- nrow(outlets)
    drdir <- raster::values(drain.dir)
    delbas <- vector("numeric", raster::ncell(drain.dir))
    if (verbose) message(paste0("Delineating ", nseeds, " basins.."))
    delbas <- .Fortran("delineate", 
                       as.integer(nx), 
                       as.integer(ny), 
                       as.integer(nseeds), 
                       as.integer(outlets$cell), 
                       as.integer(ID), 
                       as.integer(drdir), 
                       as.integer(delbas), 
                       PACKAGE = 'hydrostreamer')[[7]]
    
    delbas[delbas == 0] <- NA

    
    
    if (output %in% c("vector","v")) {
        if (verbose) message("Converting to vector. This may take considerable 
                             amount of time.")
        
        # count cells in each basin
        ncells <- rep(0, length(ID))
        for (i in 1:length(ID)) {
            bcells <- sum(delbas == ID[i], na.rm=TRUE)
            ncells[i] <- bcells
        }
        
        raster::values(drain.dir) <- delbas
        delbas <- drain.dir
        
        # vectorize raster
        delbas <- raster::rasterToPolygons(delbas, dissolve=TRUE) %>%
            sf::st_as_sf()
        names(delbas)[1] <- "riverID"
        areas <- st_area(delbas)
        cols <- cbind(riverID = ID, NCELLS = ncells, AREA_M2 = areas)
        delbas <- merge(delbas, cols)
    }
    
    if(output %in% c("raster", "r")) {
        raster::values(drain.dir) <- delbas	
        delbas <- drain.dir
    }

    return(delbas)
}