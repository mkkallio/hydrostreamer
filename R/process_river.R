## split/process river lines

# split lines using polygons and st_intersection. minimum length in meters
## INVESTIGATE why did i use iterative intersections????? --> no need for iterations
split_river_with_grid <- function(river, grid) {
  river <- suppressMessages(suppressWarnings(st_intersection(river, select(grid, ID))))

  names(river)[names(river)=="ID"] <- "gridID"
  #river <- select(river, -area_m2)

  #get rid of multilinestrings
  p4s <- st_crs(river)
  isMLS <- st_is(river, "MULTILINESTRING")
  MLS <- river[isMLS,]
  data <- st_set_geometry(MLS, NULL)

  for (i in 1:NROW(MLS)) {
    coords <- st_coordinates(MLS[i,])
    nLines <- unique(coords[,3])
    for (j in nLines) {
      l <- st_linestring(coords[coords[,3] == j,1:2]) %>%
        st_sfc() %>%
        st_set_crs(p4s)
      l <- cbind(data[i,],l)
      if(i == 1) {
        LS <- l
      } else {
        LS <- rbind(LS,l)
      }
    }
  }
  if (any(names(LS) %in% "geometry")){
    names(LS)[names(LS) %in% "geometry"] <- "geom"
  }

  LS <- st_sf(LS)
  river <- river[!isMLS,]
  river <- rbind(river, LS)

  #add unique IDs
  river <- add_column(river, ID = 1:NROW(river), .before=1)
  #return
  return(river)
}




# TAKE A CLEAN RIVER NETWORK AND CREATE FROM-TO LISTS.
from_to_network <- function(river, ID = "ID") {
  ID <- river[, names(river) %in% ID] %>%    #### CHANGE TO USE select_()
    st_set_geometry(NULL) %>%
    unlist()
  nSegments <- NROW(river)
  FROM <- vector("list", nSegments)
  FROM_ALL <- vector("list", nSegments)
  TO <- vector("list", nSegments)
  TO_ALL <- vector("list", nSegments)


  #First part: get starting and ending coordinates
  message("Processing part 1: computing adjacent segments")
  p <- 0
  start <- list()
  end <- list()
  coords <- st_coordinates(river)
  for (i in 1:nSegments) {
    segcoords <- coords[coords[,3] == i,]
    start[[i]] <- segcoords[1,1:2]
    end[[i]] <- segcoords[NROW(segcoords),1:2]
  }
  start <- t(as.data.frame(start))
  end <- t(as.data.frame(end))

  #First part: match the coordinates to find previous and next segments
  for (i in 1:nSegments) {
    # to which river segments river flows from
    x <- end[i,1] == start[,1]
    y <- end[i,2] == start[,2]
    source <- which(x & y)

    if (length(source)==0) {
      TO[[i]] <- -9999
    } else {
      TO[[i]] <- as.numeric(ID[source])
    }

    # from which river segments river flows to
    x <- start[i,1] == end[,1]
    y <- start[i,2] == end[,2]
    source <- which(x & y)
    if (length(source)==0) {
      FROM[[i]] <- -9999
    } else {
      FROM[[i]] <- as.numeric(ID[source])
    }
    fivePercent <- round(nSegments/20,0)
    onePercent <- round(nSegments/100,0)
    if (i %% fivePercent == 0) {
      p <- p+5
      cat(paste0(p,"%"))
    } else if (i %% onePercent == 0) {
      cat(".")
    }
  }

  # collect ALL from and ALL to nodes for each river segment
  cat("\n")
  message("Processing part 2: collecting all upstream and downstream segments")
  p <- 0 #progress indicator
  for (i in 1:nSegments) {
    to <- TO[[i]]
    n <- 0
    all <- list()
    while(to != -9999) {
      n <- n+1

      nextID <- which(ID == to)
      all[[n]] <- ID[nextID]
      to <- TO[[nextID]]
      if(to == -9999){
        break
      }
    }
    all <- unlist(all)
    TO_ALL[[i]] <- all
    #
    # if(length(all) != 0) {
    #   for (j in 1:length(all)) {
    #     row <- which(ID == all[j])
    #     fromall <- FROM_ALL[[row]]
    #     fromall <- c(fromall, all[j])
    #     FROM_ALL[[row]] <- fromall
    #   }
    # }
    fivePercent <- round(nSegments/20,0)
    onePercent <- round(nSegments/100,0)
    if (i %% fivePercent == 0) {
      p <- p+5
      cat(paste0(p,"%"))
    } else if (i %% onePercent == 0) {
      cat(".")
    }
  }
  TO <- unlist(TO)
  #  process output
  cat("\n")
  message("Processing part 3: output")
  river <- add_column(river, PREVIOUS=FROM, NEXT=TO, DOWNSTREAM=TO_ALL) #removed upstream (FROM_ALL) for being redundant and easily constructed from TO_ALL
  return(river)
}
