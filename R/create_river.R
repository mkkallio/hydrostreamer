#' Creates a river network and river segment drainage basins from a DEM
#' 
#' This function creates a river network and individual river segment
#' drainage basins from a digital elevation model. Outputs are written
#' as gpkg files which can be directly used in hydrostreamer.
#'
#' This package takes advantage of SAGA-GIS tools through RSAGA to
#' first derive a river network and drainage directions and further
#' to delineate individual river segment basins with hydrostreamer tools.
#' 
#' First, a DEM is read in and it is filled to ensure continuous flow
#' paths throughout the given area into basin outlets instead of random
#' sinks. SAGA-GIS ta_preprocessing module 4, which takes advantage of a
#' sink fill algorithm by Wang & Liu (2006), is used in the filling.
#' 
#' After this, SAGA-GIS ta_channels module 5 is used to derive the river
#' network and drainage directions throughout the area provided. Finally,
#' drainage basins are delineated for each river segment.
#' 
#' Outputs are gpkg files which can be read and used directly in
#' hydrostreamer. In return value, absolute file paths to resulting
#' files are given as a list.
#' 
#' @param dem A RasterLayer object in WGS84 coordinate system
#'            representing terrain elevation in raster values.
#' @param minslope Minimum slope angle (degrees) to preserve between DEM cells
#'                 when deriving river segments. Note that when using meters
#'                 as vertical units and degrees as horizontal units the
#'                 minimum slope calculation is not able to do the conversion.
#'                 If the river network doesn't form, increase this parameter.
#'                 Increasing minslope leads to more carefully carved flowpaths
#'                 and decreasing it may result in flat areas due to elevation
#'                 changes becoming less than the numerical accuracy of output
#'                 layer. For further information, see SAGA-GIS tool
#'                 ta_preprocessing module 4.
#' @param output_path Path to which the outputs will be written.
#'                    Defaults to empty string; outputs will be
#'                    written to temp folder. Value "wd" writes
#'                    outputs to current working directory.
#' @param threshold Strahler order of small streams from which to start
#'                  drawing rivers. Changes the threshold parameter in
#'                  SAGA-GIS tool Channel Network and Drainage Basins
#'                  (ta_channels module 5). If the river network comes
#'                  out as too dense, increase the threshold and if it's
#'                  too sparse, lower the threshold.
#' @param do_basins Whether delineating individual river segment basins
#'                  or not. Basin delineation takes some extra time.
#' @param rsaga_env Valid RSAGA environment parameter list according
#'                  to specifications in function rsaga.env().
#'                  Recommended not to give when automatic detection
#'                  of RSAGA environment works. Even in manual case,
#'                  best created by calling rsaga.env() with parameters.
#' @param verbose Whether outputting intermediate messages or not.
#' 
#' @return Two-placed list of filenames to created rivers and basins.
#'         list$rivers gives path to created rivers and list$basins
#'         to created river segment basins. If list$basins is NA, no
#'         basins were created.
#' 
#' @examples 
#' \dontrun{
#' 
#'   library(sf)
#'   library(hydrostreamer)
#'   library(raster)
#' 
#'   # Read in a DEM from an existing file
#'   dem <- raster("DEMraster.tif")
#' 
#'   # Create river network and drainage basins
#'   results <- create_river(dem = dem, minslope = 1.0)
#' 
#'   # Read from output files
#'   rivers_created <- st_read(results$rivers)
#'   basins_created <- st_read(results$basins)
#' }
#' 
#' @author Vili Virkki
#' @export

create_river <- function(dem,
                         minslope,
                         threshold = 5,
                         output_path = "",
                         do_basins = TRUE,
                         rsaga_env = FALSE,
                         verbose = FALSE){
    
    ###
    ### Check user choices and that everything is correct before starting to 
    ### derive rivers
    ###
    # require RSAGA (optional package) for this part
    if (!requireNamespace("RSAGA")){
        # RSAGA namespace is not found from R installation.
        # Please install SAGA-GIS and RSAGA before applying this function.
        stop("RSAGA is required for creating river networks.")
    }
    
    # check whether the user wants to do the basins
    if (do_basins){
        message(paste0("Chosen to delineate individual river segment basins.\n",
                       "This may take time.\n",
                       "If desired, disable basin delineation by giving ",
                       "do_basins = FALSE"))
    }
    
    if (verbose){
        message("Starting to create the river network...")
    }
    
    # Try to search automatically for a SAGA environment if nothing was given
    if (class(rsaga_env) != "list"){
        saga_env <- tryCatch({
            # rsaga.env() pushes a warning even if everything is right
            saga_env <- suppressWarnings(RSAGA::rsaga.env())
        }, error = function(e) {
            # RSAGA environment was not found automatically.
            # For manual setup, please see ?rsaga.env() and
            # pass a valid list of environment parameters
            # formed according to rsaga.env documentation
            # as a parameter for this function.
            message(e)
            stop("\nPlease check your RSAGA environment.")
        })
    } else {
        # If the user has provided SAGA environment manually, set it here.
        # Assume that the environment is well-formed to avoid checking all
        # arguments inside.
        saga_env <- rsaga_env
        message(paste0("Loaded given SAGA environment assuming that it is valid.\n",
                       "Further errors in RSAGA usage may arise if the settings fail."))
    }
    
    # proj string for setting up CRS in some outputs
    p <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # check if input DEM is in WGS84
    # if not, transform into WGS84 and back to original in the end
    original_crs <- NA
    if (!identical(as.character(raster::crs(dem)), p)){
        
        warning(paste0("Input DEM was projected to WGS84 CRS.\n",
                       "Outputs were transformed back to the original CRS.\n",
                       "Note that the river network derivation was done in WGS84.\n",
                       "This may have caused inaccuracies in outputs."))
        
        # get original CRS for later use in projecting outputs
        original_crs <- raster::crs(dem, asText = TRUE)
        
        # create a template raster with same extent than input and equal
        # horizontal and vertical cell size (average of x and y if differing)
        template <- raster::projectExtent(dem, crs = p)
        if (raster::xres(template) != raster::yres(template)){
            raster::res(template) <- 
                (raster::xres(template) + raster::yres(template)) / 2
        }
        
        # resample original DEM values to projected raster
        original_dem <- raster::projectRaster(dem, crs = p)
        dem <- raster::resample(original_dem, template)
        
    }
    
    ###
    ### Derive the river network and delineate river segment basins if needed
    ###
    # Write input DEM to SAGA format
    raster_path <- tempfile(fileext = ".sgrd")
    raster::writeRaster(dem, raster_path, format="SAGA", overwrite=TRUE) 
    
    # do sink fill
    dem_filled_path <- tempfile(fileext = ".sgrd")
    RSAGA::rsaga.geoprocessor(lib = "ta_preprocessor",
                              module = 4,
                              env = saga_env,
                              param = list(ELEV = raster_path,
                                           FILLED = dem_filled_path,
                                           FDIR = tempfile(fileext = ".sgrd"),  
                                           WSHED = tempfile(fileext = ".sgrd"), 
                                           MINSLOPE = as.character(minslope)),
                              show.output.on.console = verbose)
    
    # derive river network
    fdir_path <- tempfile(fileext = ".sdat")
    channels_path <- tempfile(fileext = ".shp")
    RSAGA::rsaga.geoprocessor(lib = "ta_channels",
                              module = 5,
                              env = saga_env,
                              param = list(DEM = dem_filled_path,
                                           DIRECTION = fdir_path,
                                           SEGMENTS = channels_path,
                                           BASINS = tempfile(fileext = ".shp"),
                                           NODES = tempfile(fileext = ".shp"), 
                                           THRESHOLD = threshold),
                              show.output.on.console = verbose)
    
    if (verbose){
        message("River segments created...")
    }
    
    # read rivers, drop Z and M values and set CRS to prepare for output
    river <- sf::st_read(channels_path, quiet = !verbose) %>%
        sf::st_zm() %>%
        sf::st_set_crs(4326)
    
    # delineate basins only if the user wants (default)
    if (do_basins){
        
        if (verbose){
            message("Creating river segment basins...")
        }
        
        # read flow directions, set CRS and remove edge cells
        drain.dir <- raster::raster(fdir_path)
        crs(drain.dir) <- p
        drain.dir <- raster::trim(drain.dir, values = 255)
        
        # reclassify drainage direction for hydrostreamer's understanding
        rcvals <- c(0,64, 1,128, 2,1, 3,2, 4,4, 5,8, 6,16, 7,32)
        rcl <- matrix(rcvals, nrow = 8, byrow = TRUE)
        drain.dir <- raster::reclassify(drain.dir, rcl)
        
        # find river outlets
        outlets <- hydrostreamer::river_outlets(river, drain.dir)
        
        # find river segment basins
        delbas <- hydrostreamer::delineate_basin(outlets,
                                                 drain.dir,
                                                 verbose = verbose,
                                                 riverID = "SEGMENT_ID")        
    }
    
    ###
    ### Write outputs and return
    ###
    if (verbose){
        message("Writing outputs...")
    }
    
    # 1. see whether the user has given fixed and valid output path
    # 2. if option "wd" was given, save results to current working directory
    # 3. otherwise save the results to temp files
    if (!output_path == "" & file.exists(output_path)){
        basins_path <- paste0(output_path, "/basins.gpkg")
        rivers_path <- paste0(output_path, "/rivers.gpkg")
    } else if (output_path == "wd"){
        basins_path <- paste0(getwd(), "/basins.gpkg")
        rivers_path <- paste0(getwd(), "/rivers.gpkg")
    } else {
        basins_path <- tempfile(fileext = ".gpkg")
        rivers_path <- tempfile(fileext = ".gpkg")
    }
    
    # if the input dem was in a different crs than WGS84, project results back to it
    if (!is.na(original_crs)){
        river <- sf::st_transform(river, crs = original_crs)
        if (do_basins){
            delbas <- sf::st_transform(delbas, crs = original_crs)
        }
    }
    
    # write outputs, overwrite old ones if there are some
    sf::st_write(river, 
                 rivers_path, 
                 quiet = !verbose, 
                 layer_options = "OVERWRITE=YES")
    
    if (do_basins){
        sf::st_write(delbas, 
                     basins_path, 
                     quiet = !verbose, 
                     layer_options = "OVERWRITE=YES")
    } else {
        basins_path <- NA
    }
    
    # prepare returning paths to output files
    namelist <- as.list(c(rivers_path, basins_path))
    names(namelist) <- c("rivers", "basins")
    
    if (verbose & do_basins){
        message("Finished creating the river network and 
                river segment drainage basins.")
    } else if (verbose){
        message("Finished creating the river network.")
    }
    
    return(namelist)
    
}
