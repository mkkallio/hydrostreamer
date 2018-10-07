#' Create a segment-specific Voronoi diagram from an sf LINESTRING object.
#'
#' The function creates Voronoi diagram for each segment in a directed 
#' connected river network (\code{sf LINESTRING}), where the Voronoi 
#' polygons join together at network segment intersections.
#' 
#' Creating the segment Voronoi diagram is done in the following steps:
#' \enumerate{
#'   \item Move starting and ending nodes forward or backwards (respectively) 
#'   for a small amount to ensure there are no identical nodes.
#'   \item Compute a Voronoi diagram from point cloud of river segment nodes.
#'   \item Union the individual polygons using river segment ID.
#'   \item Clip the polygons to the area of interest.
#'   \item Process erroneous polygons, if any.
#' }
#'
#' The accuracy of the final segment Voronoi diagram is depending on the 
#' density of nodes in the river network. Consider densifying geometry e.g. 
#' with \code{sf::st_segmentize()} function for higher accuracy.
#'
#' @inheritParams compute_HSweights
#'
#'
#' @return Returns an 'sf' polygon object, with a column "ID" corresponding 
#'   to the river segment IDs.
#'   
#' @export
river_voronoi<- function(river, aoi, riverID = "riverID", verbose=FALSE) {
    
    if(is.null(river)) stop("river network is required")
    if(is.null(aoi)) stop("area of interest is required")
    
    #inspect input
    if(!any(class(river) == "sf")) {
        stop("river input should be an 'sf' LINESTRING object")
    }

    IDs <- dplyr::select_(river, riverID) %>% 
        sf::st_set_geometry(NULL) %>% 
        unlist() %>% 
        unname()
    
    # Move end and start coordinates ~10m backwards, in order to prevent 
    # river nodes being at exact same position when calculating the Voronoi 
    # diagram.
    if (verbose) message("Processing nodes..")
    n <- NROW(river)
    voronoi <- move_nodes(river, verbose = verbose)
    
    #create voronoi diagram, spatially join attributes
    if (verbose) message("Processing Voronoi tesselation")
    vorPoints <- suppressWarnings(sf::st_cast(voronoi, "POINT"))
    remove <- c("NEXT", "PREVIOUS", "DOWNSTREAM","gridID")
    voronoi <- vorPoints[ , !(names(vorPoints) %in% remove)]
    bbox <- sf::st_as_sfc(sf::st_bbox(aoi))
    voronoi <- suppressMessages(
      suppressWarnings(
        sf::st_voronoi(sf::st_union(vorPoints), bbox) %>%
             sf::st_cast() %>%
             sf::st_cast("POLYGON") %>%
             sf::st_sf() %>%
             sf::st_join(vorPoints) %>%
             lwgeom::st_make_valid() %>% 
             dplyr::group_by_(riverID) %>%
             dplyr::summarise() %>%
             sf::st_intersection(sf::st_geometry(aoi))
      )
    )
    
    # fix any bad polygons
    voronoi <- fix_voronoi(voronoi, riverID = riverID, verbose = verbose)
  
    # prepare return
    if (any(names(voronoi) == "ID")) {
        voronoi$riverID <- voronoi$ID
        voronoi$ID <- 1:NROW(voronoi)
    } else {
        voronoi <- tibble::add_column(voronoi, ID = 1:NROW(voronoi), .before=1)
    }
    voronoi <- voronoi %>% dplyr::select(ID, riverID, dplyr::everything())
    
    return(voronoi)
}


