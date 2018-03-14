## split/process river lines

# split lines using polygons and sf::st_intersection. minimum length in meters
## INVESTIGATE why did i use iterative intersections????? --> no need for iterations
split_river_with_grid <- function(river, grid) {
  river <- suppressMessages(suppressWarnings(sf::st_intersection(river, select(grid, ID))))

  names(river)[names(river)=="ID"] <- "gridID"
  #river <- select(river, -area_m2)

  #get rid of multilinestrings
  p4s <- sf::st_crs(river)
  isMLS <- sf::st_is(river, "MULTILINESTRING")
  MLS <- river[isMLS,]
  data <- sf::st_set_geometry(MLS, NULL)

  for (i in 1:NROW(MLS)) {
    coords <- sf::st_coordinates(MLS[i,])
    nLines <- unique(coords[,3])
    for (j in nLines) {
      l <- sf::st_linestring(coords[coords[,3] == j,1:2]) %>%
          sf::st_sfc() %>%
          sf::st_set_crs(p4s)

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

  LS <- sf::st_sf(LS)
  river <- river[!isMLS,]
  river <- rbind(river, LS)

  #add unique IDs
  river <- tibble::add_column(river, ID = 1:NROW(river), .before=1)

  return(river)
}




# TAKE A CLEAN RIVER NETWORK AND CREATE FROM-TO LISTS.

from_to_network <- function(river, ID = "ID") {
  ID <- select_(river, ID) %>% #river[, names(river) %in% ID] %>%
    sf::st_set_geometry(NULL) %>%
    unlist()
  nSegments <- NROW(river)
  FROM <- vector("list", nSegments)
  FROM_ALL <- vector("list", nSegments)
  TO <- vector("list", nSegments)
  TO_ALL <- vector("list", nSegments)


  #First part: get starting and ending coordinates
  message("Processing part 1: seeking adjacent segments")
  p <- 0
  start <- list()
  end <- list()

  coords <- sf::st_coordinates(river)
  for (i in 1:nSegments) {
    segcoords <- coords[coords[,3] == i,]
    start[[i]] <- segcoords[1,1:2]
    end[[i]] <- segcoords[NROW(segcoords),1:2]
  }
  start <- t(as.data.frame(start))
  end <- t(as.data.frame(end))

  #First part: match the coordinates to find previous and next segments
  total <- nSegments
  pb <- txtProgressBar(min = 0, max = total, style = 3)
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
    # fivePercent <- round(nSegments/20,0)
    # onePercent <- round(nSegments/100,0)
    # if (i %% fivePercent == 0) {
    #   p <- p+5
    #   cat(paste0(p,"%"))
    # } else if (i %% onePercent == 0) {
    #   cat(".")
    # }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # collect ALL from and ALL to nodes for each river segment
  cat("\n")
  message("Processing part 2: collecting all upstream and downstream segments")
  total <- nSegments
  pb <- txtProgressBar(min = 0, max = total, style = 3)
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
    # fivePercent <- round(nSegments/20,0)
    # onePercent <- round(nSegments/100,0)
    # if (i %% fivePercent == 0) {
    #   p <- p+5
    #   cat(paste0(p,"%"))
    # } else if (i %% onePercent == 0) {
    #   cat(".")
    # }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  TO <- unlist(TO)

  #  process output
  #cat("\n")
  message("Processing part 3: output")
  river$PREVIOUS <- FROM
  river$NEXT <- TO
  river$DOWNSTREAM <- TO_ALL
  return(river)
}





river_hierarchy <- function(river, ID = "ID", type="strahler") {

  from <- river$PREVIOUS
  to <- river$NEXT
  ID <- dplyr::select_(river, ID) %>% sf::st_set_geometry(NULL) %>%
      unlist()


  n_seg <- NROW(river)
  strahler <- rep(1, n_seg)
  rounds_with_no_edits <- 0
  edits <- 1

  while (rounds_with_no_edits < 5) {
    if (edits == 0) rounds_with_no_edits <- rounds_with_no_edits+1
    if (rounds_with_no_edits == 5) break
    edits <- 1
    # run for every river segment
    for (seg in 1:n_seg) {
      n_sources <- length(unlist(from[[seg]]))
      # check if the segment is headwaters (no inflowing segments)
      if (n_sources == 1 && unlist(from[[seg]]) == -9999) {

      } else if (n_sources == 1 && unlist(from[[seg]]) != -9999) {
        # what to assign if only one inflowing segment
        prev_seg <- as.numeric(unlist(from[seg]))
        row <- ID == prev_seg

        if (!strahler[seg] == strahler[row]){ # if the current stream order IS NOT EQUAL TO inflowing stream order
          strahler[seg] <- strahler[row]
          edits <- edits+1
        }

      } else {
        # what to do if more than one inflowing river segment
        prev_segs <- unlist(from[seg])

        str <- vector("numeric", length = length(prev_segs))
        # get the strahler number of the incoming river segments
        for(pseg in 1:length(prev_segs)){
          row <- ID == prev_segs[pseg]
          str[pseg] <- strahler[row]
        }

        max_value <- max(str)
        n_max_values <- table(str)[as.character(max_value)]


        if (n_max_values == 1) {
          if (!strahler[seg] == max_value) {
            strahler[seg] <- max(str)
            edits <- edits+1
          }
        } else {
          if (!strahler[seg] == max_value+1) {
            strahler[seg] <- max(str)+1
            edits <- edits+1
          }
        }

      }
    }
    edits <- edits-1
    #print(paste0("Edits this round: ", edits))
    #print(table(strahler))

  }
  river$STRAHLER <- strahler
  return(river)

}
