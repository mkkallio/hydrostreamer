#' Delineate catchments from multiple input points
#'
#' This algorithm delineates multiple catchments from input point locations and a drainage direction layer.
#' Catchments are delineated upstream from the point given, until another given outlet point is met. The result
#' is a set of catchments which are unique to the points given.
#' 
#' Note that this function is currently implemented using R alone, which means that the for large areas of interest,
#' computation time may take long. Reading the drainage direction raster entirely to memory (using 
#' \code{\link[raster]{readAll}} from the \code{raster} package) speeds up the computation#' considerably, and is strongly 
#' suggested.   
#'
#' @param outlets An 'sf' point object, with locations of the catchment outlets for delineation. Obtained
#' e.g. with \code{\link{river_outlets}}
#' @param riverID Name of the column in outlets with unique identifiers.
#' @inheritParams river_outlets
#' @inheritParams compute_weights
#'
#' @return Returns polygonized catchment specific to the outlet points given. The output is 'sf' POLYGON class
#' with attributes:
#' \itemize{
#'   \item riverID: The ID given in \code{outlets}.
#'   \item ncells: The number of raster cells the catchment consists of.
#' }
#' 
#' @examples 
#' \dontrun{
#' # to be added
#' }
#'
#' @export
delineate_basin <- function(outlets, drain.dir, riverID = "riverID", verbose = FALSE) {
    
    #do checks
    if (sf::st_is(outlets[1,], "LINESTRING")) outlets <- river_outlets(outlets, drain.dir)
    
    #prepare data and delineate
    if (verbose) message("Preparing..")
    rp <- raster::cellFromXY(drain.dir, sf::st_coordinates(outlets))
    outlets$cell <- rp
    outlets <- outlets[!is.na(outlets$cell),]
    ID <- outlets %>% dplyr::select_(riverID) %>% sf::st_set_geometry(NULL) %>% unlist()
    ny <- nrow(drain.dir)
    nx <- ncol(drain.dir)
    nseeds <- nrow(outlets)
    drdir <- raster::values(drain.dir)
    delbas <- vector("numeric", raster::ncell(drain.dir))
    if (verbose) message(paste0("Delineating ", nseeds, " basins.."))
    delbas <- .Fortran("delineate", as.integer(nx), as.integer(ny), as.integer(nseeds), as.integer(outlets$cell), 
                       as.integer(ID), as.integer(drdir), as.integer(delbas), PACKAGE = 'hydrostreamer')[[7]]
    
    if (verbose) message("Postprocessing..")
    # count cells in each basin
    ncells <- rep(0, length(ID))
    for (i in 1:length(ID)) {
        bcells <- sum(delbas == ID[i], na.rm=TRUE)
        ncells[i] <- bcells
    }
    raster::values(drain.dir) <- delbas
    delbas <- drain.dir
    ncells <- cbind(riverID = ID, ncells)
    
    # vectorize raster
    delbas <- raster::rasterToPolygons(delbas, dissolve=TRUE) %>%
        sf::st_as_sf()
    names(delbas)[1] <- "riverID"
    delbas <- merge(delbas, ncells)
    
    return(delbas)
}