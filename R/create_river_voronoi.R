
#### CHANGE TO USE st_segmentize() and dissolve with ID
densify_geometry <- function(geom, interval, file = "densified_geom.shp") {
  #densify if TRUE
  if (!require(RQGIS)) {
    message("Densification requires package 'RQGIS' and an installation of QGIS.")
    stop()
  }
  if (require(RQGIS)) { ## add class check. needs an sp object, not sf
    message("Densifying geometries")
    RQGIS::set_env()
    params <- RQGIS::get_args_man(alg = "qgis:densifygeometriesgivenaninterval")
    params$INPUT <- geom; params$INTERVAL <- interval; params$OUTPUT <- file;
    densified <- run_qgis(alg = "qgis:densifygeometriesgivenaninterval",
                          params = params)
    densified <- st_read(file)
  }
  return(densified)
}





##### CREATE RIVER VORONOI Also problems with st_difference - does not make difference in tail node. Fix!
# default is we get rid of any segment under 10m long, and buffer radius is approximately 10m (11.3m at equator).
#### BREAK DOWN IN TO SMALLER CHUNKS
river_voronoi <- function(river, grid, basin, ID = "ID", min=10, tolerance = 0.001) {
  #get rid of extremely small segments
  # set units for the minimum length
  units(min) <- with(units::ud_units, m)
  len <- st_length(river)
  short <- len < min
  river <- river[!short,]
  message(paste0("Cleaned away ", table(short)[2], " segments shorter than ", min, " meter(s). These will not take part in Voronoi polygon creation."))


  message("Processing line segment end points...")
  n <- NROW(river)
  coords <- st_coordinates(river)
  #data <- st_set_geometry(river, NULL)
  #remove <- c("NEXT", "PREVIOUS", "DOWNSTREAM")
  #data <- data[ , !(names(data) %in% remove)]
  p4s <- st_crs(river)

  #extract end nodes
  for (i in 1:n) {
    segcoords <- coords[coords[,3] == i,]
    node <- st_point(segcoords[NROW(segcoords),1:2]) %>%
      st_sfc()
    if(i==1) {
      end <- node
    } else {
      end <- rbind(end, node)
    }
  }
  end <- st_sfc(end)



  # take buffers
  buffer <- st_buffer(end, dist = tolerance) %>%
    #st_combine() %>%
    st_set_crs(p4s)



  # take difference between buffer and river
  for(i in 1:n) {
    vorLine <- suppressWarnings(suppressMessages(st_difference(river[i,], buffer[i])))
    if (i == 1) {
      vorRiv <- vorLine
    } else {
      vorRiv <- rbind(vorRiv, vorLine)
    }
  }

  #end <- st_set_geometry(river, end)
  #end <- st_set_crs(end, p4s)

  #create voronoi, spatially join attributes
  # needs also densification of points!
  message("Processing Voronoi tesselation")
  #vorPoints <- st_line_sample(river, density = 0.001)
  vorPoints <- suppressWarnings(st_cast(vorRiv, "POINT"))
  remove <- c("NEXT", "PREVIOUS", "DOWNSTREAM","gridID")
  vorPoints <- vorPoints[ , !(names(vorPoints) %in% remove)]
  bbox <- st_as_sfc(st_bbox(grid))
  voronoi <- suppressWarnings(st_voronoi(st_union(vorPoints), bbox)) %>%
    st_cast() %>%
    st_cast("POLYGON") %>%
    st_sf() %>%
    st_join(vorPoints) %>%
    lwgeom::st_make_valid() %>% # fix any broken geometries
    group_by_(.dots = list(ID)) %>% # dissolve by ARCID (this only works with hydrosheds rivers -> support others later)
    summarise() %>%
    st_intersection(st_geometry(basin)) # only geometry so that basin attributes are not carried over

  # the process may generate geometrycollections instead of polygons --> this will work on them
  v.gc <- st_is(voronoi, "GEOMETRYCOLLECTION")

  if (any(v.gc)) {
    message("Fixing bad polygons (GEOMETRYCOLLECTION)")
    p.geom <- voronoi[v.gc,]
    p.geom <- st_collection_extract(p.geom, "POLYGON")
    voronoi[v.gc,] <- p.geom
  }

  # sometimes there are voronoi areas left which were not assigned any ID (why??). The following code merges them to the neighbouring polygon
  # with which it shares the longest border segment.
  IDs <- voronoi[, names(voronoi) %in% ID] %>%
    st_set_geometry(NULL) %>%
    unlist()
  v.na <- is.na(IDs)

  if (any(v.na)) {
    message("Fixing bad polygons (missing ID)")
    pp <- st_cast(voronoi[v.na,], "POLYGON")
    for (i in 1:NROW(pp)) {
      #find which polygons touch the problem polygon
      touching <- suppressMessages(st_touches(pp[i,], voronoi, sparse=FALSE))
      tv <- voronoi[touching,]

      #compute the lenghts of border line for every touching polygon and find which one is longest
      len <- list()
      n <- table(touching)[2]
      for (tp in 1:n) {
        line <- suppressMessages(st_intersection(pp[i,], tv[tp,]))
        l_len <- st_length(line)
        len[[tp]] <- l_len
      }
      len <- unlist(len)
      longest <- which(len == max(len))

      #union the two polygons
      v.union <- suppressMessages(suppressWarnings(st_union(pp[i,], tv[longest,])))

      #replace geometry in voronoi
      row <- which(IDs == IDs[touching][longest])
      v.union <- st_set_geometry(voronoi[row,], st_geometry(v.union))
      voronoi[row,] <- v.union
    }
    #remove geometries with NA id
    voronoi <- voronoi[!v.na,]
  }

  return(voronoi)
}

