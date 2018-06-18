#' Delineate catchments from multiple input points
#'
#' This algorithm delineates multiple catchments from input point locations and a drainage direction layer.
#' Catchments are delineated upstream from the point given, until another given outlet point is met. The result
#' is a set of catchments which are unique to the points given.
#' 
#' Note that this function is currently implemented using R alone, which means that the for large areas of interest,
#' computation time may be long. Reading the drainage direction raster entirely to memory (using 
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
    
    rp <- raster::cellFromXY(drain.dir, sf::st_coordinates(outlets))
    outlets$cell <- rp
    outlets <- outlets[!is.na(outlets$cell),]
    ID <- outlets %>% dplyr::select_(riverID) %>% sf::st_set_geometry(NULL) %>% unlist()
    
    #create empty raster for delineated basins
    delbas <- drain.dir
    raster::values(delbas) <- 0
    raster::values(delbas)[outlets$cell] <- ID
    
    cell_list <- vector("list", nrow(outlets))
    
    maxbasin <- round(ncell(drain.dir)/nrow(outlets)*10,0)
    
    if(verbose) pb <- txtProgressBar(min = 0, max = nrow(outlets), style = 3)
    
    # delineate upstream from every point given
    for (point in 1:nrow(outlets)) {
        
        #start the cellvisit list
        c2v <- list()
        basincells <- vector("numeric", maxbasin)
        vpos <- 1
        cells <- next_cell_up(outlets$cell[point], drain.dir)
        if (length(cells) == 0) next #if there are no upcells, move to next point
        for (i in 1:length(cells)) {
            
            if (!cells[i] %in% outlets$cell) {
                c2v[[length(c2v)+1]] <- cells[i]
                basincells[vpos] <- cells[i]
            }
            vpos <- vpos+1
        }
        
        # visit all cells upstream, until another delineation point, and save info
        while (length(c2v) >0) {
            if (length(c2v) == 0) break
            
            cells <- next_cell_up(c2v[[1]], drain.dir)
            if (!length(cells) == 0) {
                for (i in 1:length(cells)) {
                    
                    if (!cells[i] %in% outlets$cell) {
                        c2v[[length(c2v)+1]] <- cells[i]
                        basincells[vpos] <- cells[i]
                    }
                    vpos <- vpos+1
                }
            }
            c2v[[1]] <- NULL
        }
        
        cell_list[[point]] <- basincells[!basincells == 0]
        if (verbose) setTxtProgressBar(pb, point)
    }
    if (verbose) close(pb)
    
    #record which cells were visited from which point
    for (i in 1:length(cell_list)) {
        cells <- cell_list[[i]]
        raster::values(delbas)[cells] <- ID[i]
        raster::values(delbas)[raster::values(delbas) == 0] <- NA
    }
    
    # count the number of cells in the basin
    ncells <- rep(0, length(ID))
    
    for (i in 1:length(ID)) {
        bcells <- sum(raster::values(delbas) == ID[i], na.rm=TRUE)
        ncells[i] <- bcells
    }
    ncells <- cbind(riverID = ID, ncells)
    
    delbas <- raster::rasterToPolygons(delbas, dissolve=TRUE) %>%
        sf::st_as_sf()
    names(delbas)[1] <- "riverID"
    delbas <- merge(delbas, ncells)
    
    return(delbas)
    
}