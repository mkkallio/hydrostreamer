#' Create segment-specific Voronoi diagram from a network.
#'
#' The function creates Voronoi diagram for each segment in a directed connected river network, where the 
#' Voronoi polygons join together at network segment intersections.
#' 
#' Creating the segment Voronoi polygons is done in the following steps:
#' \enumerate{
#'   \item Extract segment end nodes.
#'   \item Create a buffer of the end nodes, and take difference between the river network and the buffer layer.
#'   \item Extract all nodes of the river network, and create Voronoi polygons for all nodes.
#'   \item Dissolve the individual polygons using river segment ID.
#'   \item Clip the polygons to the area of interest.
#'   \item Process erroneous polygons if any.
#' }
#'
#' Thus, the accuracy of the final segment Voronoi diagram is depending on the density of nodes in the river network.
#' Consider densifying geometry e.g. with sf::st_segmentize function for higher accuracy.
#'
#' @param min A numeric value. Removes segments which are shorter than the value in meters, in order to remove unnecessarily
#'   short river segments. Defaults to 10 meters.
#' @param tolerance Radius of the buffer used to take difference. Defaults to 0.001 (in map units).
#' @inheritParams compute_weights
#'
#'
#' @return Returns an 'sf' polygon object, with a column "ID" corresponding to the river segment IDs.
#' @export
#'
#'
river_voronoi <- function(river, aoi, riverID = "riverID", min=10, tolerance = 0.001, verbose=FALSE) {
    
    #inspect input
    if(!any(class(river) == "sf")) {
        stop("river input should be an 'sf' LINESTRING object")
    }
    
    #get rid of extremely small segments
    # set units for the minimum length
    units(min) <- with(units::ud_units, m)
    len <- sf::st_length(river)
    short <- len < min
    river <- river[!short,]
    if (verbose) {
        message(paste0("Cleaned away ", table(short)[2], " segments shorter than ", min, " meter(s). These will not take part in Voronoi polygon creation."))
    }
  
    ID <- dplyr::select_(river, riverID) %>% sf::st_set_geometry(NULL) %>% unlist()

    if (verbose) message("Extracting line segment end points...")
    n <- NROW(river)
    coords <- sf::st_coordinates(river)
    p4s <- sf::st_crs(river)

    # extract end nodes
    p4s <- sf::st_crs(river)[[2]]
    if( grepl("longlat", p4s, fixed=TRUE) ) {
        for (i in 1:n) {
            segcoords <- coords[coords[,3] == i,]
            node <- sf::st_point(segcoords[NROW(segcoords),1:2]) %>%
                sf::st_sfc()
            if(i==1) {
                end <- node
            } else {
                end <- rbind(end, node)
            }
        }
        end <- sf::st_sfc(end) %>% sf::st_set_crs(p4s)
    } else {
        end <- sf::st_line_sample(river, sample=1)
    }

    if (verbose) message("Creating buffers...")
    # take buffers
    buffer <- suppressWarnings(suppressMessages(sf::st_buffer(end, dist = tolerance) %>%
        sf::st_union() %>%
        sf::st_set_crs(p4s)))


    if (verbose) message("Taking difference")
    #take difference between buffer and river
    total <- n
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    for(i in 1:n) {
        vorLine <- suppressWarnings(suppressMessages(sf::st_difference(river[i,], buffer)))
        if (i == 1) {
            vorRiv <- vorLine
        } else {
            vorRiv <- rbind(vorRiv, vorLine)
        }
        if (verbose) setTxtProgressBar(pb, i)
    }
    if (verbose) close(pb)


    #create voronoi, spatially join attributes
    if (verbose) message("Processing Voronoi tesselation")
    vorPoints <- suppressWarnings(sf::st_cast(vorRiv, "POINT"))
    remove <- c("NEXT", "PREVIOUS", "DOWNSTREAM","gridID")
    vorPoints <- vorPoints[ , !(names(vorPoints) %in% remove)] %>%
        dplyr::rename_(ID = riverID)
    bbox <- sf::st_as_sfc(sf::st_bbox(aoi))
    voronoi <- suppressMessages(suppressWarnings(sf::st_voronoi(sf::st_union(vorPoints), bbox) %>%
        sf::st_cast() %>%
        sf::st_cast("POLYGON") %>%
        sf::st_sf() %>%
        sf::st_join(vorPoints) %>%
        lwgeom::st_make_valid() %>% # fix any broken geometries
        dplyr::group_by_(.dots = list(~ID)) %>%
        dplyr::summarise() %>%
        sf::st_intersection(sf::st_geometry(aoi))))

    # the process may generate geometrycollections instead of polygons --> this will work on them
    v.gc <- sf::st_is(voronoi, "GEOMETRYCOLLECTION")

    if (any(v.gc)) {
        if (verbose) message("Fixing bad polygons (GEOMETRYCOLLECTION)")
        p.geom <- voronoi[v.gc,]
        p.geom <- sf::st_collection_extract(p.geom, "POLYGON")
        voronoi[v.gc,] <- p.geom
    }

    # sometimes there are voronoi areas left which were not assigned any ID (why??). The following code merges them to the neighbouring polygon
    # with which it shares the longest border segment.
    IDs <- voronoi[, names(voronoi) %in% riverID] %>%
        sf::st_set_geometry(NULL) %>%
        unlist()
    v.na <- is.na(IDs)

    if (any(v.na)) {
        if (verbose) message("Fixing bad polygons (missing ID)")
        pp <- sf::st_cast(voronoi[v.na,], "POLYGON")
        for (i in 1:NROW(pp)) {
            #find which polygons touch the problem polygon
            touching <- suppressMessages(sf::st_touches(pp[i,], voronoi, sparse=FALSE))
            tv <- voronoi[touching,]

        #compute the lenghts of border line for every touching polygon and find which one is longest
        len <- list()
        n <- table(touching)[2]
        for (tp in 1:n) {
            line <- suppressMessages(sf::st_intersection(pp[i,], tv[tp,]))
            l_len <- sf::st_length(line)
            len[[tp]] <- l_len
        }
        len <- unlist(len)
        longest <- which(len == max(len))

        #union the two polygons
        v.union <- suppressMessages(suppressWarnings(sf::st_union(pp[i,], tv[longest,])))

        #replace geometry in voronoi
        row <- which(IDs == IDs[touching][longest])
        v.union <- sf::st_set_geometry(voronoi[row,], sf::st_geometry(v.union))
        voronoi[row,] <- v.union
        }
        #remove geometries with NA id
        voronoi <- voronoi[!v.na,]
    }

    # process IDs
    if (any(names(voronoi) == "ID")) {
        voronoi$riverID <- voronoi$ID
        voronoi$ID <- 1:NROW(voronoi)
      
    } else {
        voronoi <- tibble::add_column(voronoi, ID = 1:NROW(voronoi), .before=1)
    }
    voronoi <- voronoi %>% dplyr::select(ID, riverID, dplyr::everything())

    return(voronoi)
}

