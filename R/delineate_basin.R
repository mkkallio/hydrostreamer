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
#' @param use_rsaga Whether to try using RSAGA for basin vectorization or not.
#' Even if TRUE, requires RSAGA environment to be found.
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
#' @author Vili Virkki, Marko Kallio
#'
#' @export
delineate_basin <- function(outlets, 
                            drain.dir, 
                            riverID = "riverID", 
                            output = "vector",
                            use_rsaga = TRUE,
                            verbose = FALSE) {
    
    VALUE <- NULL
    
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
    #ID <- outlets %>% dplyr::select_(riverID) %>% sf::st_set_geometry(NULL) %>% 
    #    unlist()
    ID <- outlets %>% dplyr::pull(riverID)
    #tempID <- 1:nrow(outlets)
    ny <- nrow(drain.dir)
    nx <- ncol(drain.dir)
    nseeds <- nrow(outlets)
    drdir <- raster::values(drain.dir)
    drdir[is.na(drdir)] <- 0
    delbas <- vector("numeric", raster::ncell(drain.dir))
    #delbas[outlets$cell] <- ID
    delbas[outlets$cell] <- 1:nseeds
    if (verbose) message(paste0("Delineating ", nseeds, " basins.."))
    
    delbas <- delineatecpp(outlets$cell-1,
                           ID,
                           drdir,
                           delbas,
                           nx,
                           ny)
    
    # delbas <- .Fortran("delineate", 
    #                    as.integer(nx), 
    #                    as.integer(ny), 
    #                    as.integer(nseeds), 
    #                    as.integer(outlets$cell), 
    #                    as.integer(ID), 
    #                    as.integer(drdir), 
    #                    as.integer(delbas), 
    #                    PACKAGE = 'hydrostreamer')[[7]]
    
    delbas[delbas == 0] <- NA
    
    if (output %in% c("vector","'v")) {
        
        if (verbose) message("Converting to vector..")
        
        # count cells in each basin
        ncells <- rep(0, length(ID))
        for (i in 1:length(ID)) {
            bcells <- sum(delbas == ID[i], na.rm=TRUE)
            ncells[i] <- bcells
        }
        
        raster::values(drain.dir) <- delbas
        delbas <- drain.dir
        
        # vectorize basins using SAGA-GIS tools if
        # 1. RSAGA is available to use
        # 2. it is desired to use it
        # 3. x and y resolutions of raster match (RSAGA doesn't work otherwise)
        if (!!requireNamespace("RSAGA") &
            use_rsaga &
            isTRUE(all.equal(raster::xres(delbas), raster::yres(delbas)))){
            
            saga_env <- tryCatch({
                # rsaga.env() pushes a warning even if everything is right
                saga_env <- suppressWarnings(RSAGA::rsaga.env())
            }, error = function(e) {
                # RSAGA environment was not found automatically.
                # Please fix your environment or give use_rsaga = FALSE
                # for this function to disable trying to use RSAGA.
                message(e)
                stop("\nPlease check your RSAGA environment.")
            })
            
            # prepare files
            delbas_grid_path <- tempfile(fileext = ".sgrd")
            delbas_shapes_path <- tempfile(fileext = ".shp")
            raster::writeRaster(delbas, 
                                delbas_grid_path, 
                                format="SAGA", 
                                overwrite=TRUE)
            
            # vectorize raster
            RSAGA::rsaga.geoprocessor(lib = "shapes_grid",
                                      module = 6,
                                      env = saga_env,
                                      param = list(GRID = delbas_grid_path,
                                                   POLYGONS = delbas_shapes_path),
                                      show.output.on.console = verbose)
            
            # read from file and set CRS
            delbas <- sf::st_read(delbas_shapes_path, quiet = !verbose) %>%
                sf::st_set_crs(4326)
            
            # select only river segment basins (leave out encircling zero 
            # values polygon) and only value (river ID) as attribute besides
            # geometry
            delbas <- delbas[2:nrow(delbas),]
            delbas <- dplyr::select(delbas, VALUE)
            
        } else {
            
            if (verbose) message("Using raster::rasterToPolygons.",
                                 " This may take long.")
            
            # vectorize raster without RSAGA
            delbas <- raster::rasterToPolygons(delbas, dissolve=TRUE) %>%
                sf::st_as_sf()
            names(delbas)[1] <- "riverID"
            #delbas$riverID <- ID[delbas$riverID]
            
        }
        
        
        #cols <- cbind(riverID = ID, NCELLS = ncells)
        #delbas <- merge(delbas, cols)
        names(delbas)[1] <- "riverID"
        match <- match(delbas$riverID, ID)
        delbas$NCELLS <- ncells[match]
        delbas$AREA_M2 <- sf::st_area(delbas)
        
    }
    
    if(output %in% c("raster", "r")) {
        raster::values(drain.dir) <- delbas	
        delbas <- drain.dir
    }
    
    return(delbas)
}