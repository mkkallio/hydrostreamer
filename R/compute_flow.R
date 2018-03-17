compute_segment_runoff <- function(x, timesteps=NULL) {
  UseMethod("compute_segment_runoff", x)
}



compute_segment_runoff.HSrgrid <- function(x, timesteps = NULL) {

    if (is.null(timesteps)) {
          river <- compute_runoff_using_line(x[[1]], x[[2]])
    } else {
          river <- compute_runoff_using_line(x[[1]], x[[2]], timesteps=timesteps)
    }


  return(river)
}


compute_segment_runoff.HSragrid <- function(x, timesteps = NULL) {

  if (is.null(timesteps)) {
      river <- compute_runoff_using_area(x[[1]], x[[2]], x[[3]])
  } else {
      river <- compute_runoff_using_area(x[[1]], x[[2]], x[[3]], timesteps=timesteps)
  }


  return(river)
}



compute_runoff_using_line <- function(river, grid, ID = "ID", timesteps = NULL, unit="mm/s") {
  # Start processing
  nSegments <- NROW(river)
  nCells <- NROW(grid)
  if (is.null(timesteps)) {
    message("No timesteps specified: computing for all timesteps")
    timesteps <- 1:(NCOL(grid)-3)
  }

  runoff <- dplyr::select(grid, -c(ID, area_m2)) %>%
    sf::st_set_geometry(NULL)


  #collect time series in a table
  Q_ts <- dplyr::select_(river, ID)


  #process every timestep, but first initiate progress bar
  total <- length(timesteps)
  pb <- txtProgressBar(min = 0, max = total, style = 3)

  for (ts in timesteps) {

    #initiate discharge of the current timestep
    Q <- vector("numeric", nSegments)
    #process every segment

    #progress bar

    for (seg in 1:nSegments) {


      #compute discharge of the segment at timestep
      cell <- river$gridID[seg]
      discharge <- NULL
      gridID <- which(grid$ID == cell)

      weight <- river$weights[seg]
      area <- grid$area_m2[gridID]


      if (unit == "mm/s") {
        discharge <- runoff[gridID,ts]/1000 * area * weight
      }
      if (unit == "m3/s") {
        discharge <- runoff[gridID,ts] * weight
      }

      #add discharge to the segment
      Q[seg] <- unclass(discharge)

    }

    Q_ts <- cbind(Q_ts, Q)
    col <- length(names(Q_ts))-1
    names(Q_ts)[col] <- paste0("TS",ts)
    setTxtProgressBar(pb, ts)
  }
  close(pb)

  # add class to return object, and add columns from river
  class(Q_ts) <- append(class(Q_ts), "HSrunoff")
  Q_ts <- add_column(Q_ts, PREVIOUS = river$PREVIOUS, NEXT = river$NEXT, DOWNSTREAM = river$DOWNSTREAM, .after=1)
  return(Q_ts)
}


compute_runoff_using_area <- function(river, basins, grid, rID = "ID", vID = "rID", timesteps = NULL, unit = "mm/s") {
  # Start processing
  nbasins <- NROW(basins)
  nCells <- NROW(grid)
  if (is.null(timesteps)) {
    message("No timesteps specified: computing for all timesteps")
    timesteps <- 1:(NCOL(grid)-3)
  }

  runoff <- dplyr::select(grid, -c(ID, area_m2)) %>%
    sf::st_set_geometry(NULL)


  #prepare a table to collect time series
  Q_ts <- dplyr::select_(river, rID) %>% rename_("ID" = rID)

  #basins id's corresponding river ids
  vID <- dplyr::select_(basins, vID) %>%
    sf::st_set_geometry(NULL) %>%
    unlist()
  rID <- dplyr::select_(river, rID) %>%
    sf::st_set_geometry(NULL) %>%
    unlist()

  #process every timestep, but first initiate progress bar
  total <- length(timesteps)
  pb <- txtProgressBar(min = 0, max = total, style = 3)

  for (ts in timesteps) {

    #initiate discharge of the current timestep
    Q <- vector("numeric", NROW(river))
    #process every segment
    for (seg in 1:nbasins) {

      #compute discharge of the segment at timestep
      cell <- basins$gridID[seg]
      discharge <- NULL
      gridID <- which(grid$ID == cell)
      riverID <- which(rID == vID[seg])

      weight <- basins$weights[seg]
      area <- grid$area_m2[gridID]

      if (unit == "mm/s") {
        discharge <- runoff[gridID,ts]/1000 * area * weight
      }
      if (unit == "m3/s") {
        discharge <- runoff[gridID,ts] * weight
      }

      #add discharge to the segment
      Q[riverID] <- Q[riverID] + unclass(discharge)
      #Q <- unlist(Q)
    }
    Q_ts <- cbind(Q_ts, Q)
    col <- length(names(Q_ts))-1
    names(Q_ts)[col] <- paste0("TS",ts)
    setTxtProgressBar(pb, ts)
  }
  close(pb)

  # add class to return object, and add specific columns from river object
  class(Q_ts) <- append(class(Q_ts), "HSrunoff")
  Q_ts <- add_column(Q_ts, PREVIOUS = river$PREVIOUS, NEXT = river$NEXT, DOWNSTREAM = river$DOWNSTREAM, .after=1)
  return(Q_ts)
}



accumulate_flow <- function(x, method="simple") {
  if(method == "simple") {
      acc <- accumulate_runoff_addition(x)
  } else {
    stop("No other method except simple addition downstream is implemented yet")
  }

  return(acc)
}




accumulate_runoff_addition <- function(river) {
  # Start processing
  nSegments <- NROW(river)

  runoff_cols <- grepl("TS", names(river), fixed=TRUE)
  runoff <- river[,runoff_cols] %>%
      sf::st_set_geometry(NULL)

  timesteps <- 1:(NCOL(runoff))


  #prepare a table to collect time series
  keep <- names(river)[!runoff_cols]
  Q_ts <- dplyr::select_(river, keep)

  #process every timestep
  total <- length(timesteps)
  pb <- txtProgressBar(min = 0, max = total, style = 3)

  # initiate accumulated runoff
  Q <- runoff
  for (ts in timesteps) {

    #process every segment
    for (seg in 1:nSegments) {

      #add discharge to all downstream segments
      discharge <- runoff[seg,ts]
      DS <- river$DOWNSTREAM[[seg]]

      if(!length(DS) == 0) {
        rows <- which(Q_ts$ID %in% DS)
        Q[rows,ts] <- Q[rows,ts] + unclass(discharge)
      }
    }
    setTxtProgressBar(pb, ts)
  }
  close(pb)

  Q <- add_column(Q, ID = river$ID)
  Q_ts <- merge(Q_ts, Q)
  class(Q_ts) <- append(class(Q_ts), "HSaccrunoff")
  return(Q_ts)
}



# compute_flow_using_line <- function(river, grid, ID = "ID", type = "length", polygons = NULL, timesteps = NULL, verbose=TRUE, unit="mm/s") {
#   # Start processing
#   nSegments <- NROW(river)
#   nCells <- NROW(grid)
#   if (is.null(timesteps)) {
#     message("No timesteps specified: computing for all timesteps")
#     timesteps <- 1:(NCOL(grid)-3)
#   }
#
#   runoff <- dplyr::select(grid, -c(ID, area_m2)) %>%
#     sf::st_set_geometry(NULL)
#
#   #all downstream river segments
#   downstream <- river$DOWNSTREAM
#   #collect time series in a table
#   Q_ts <- dplyr::select_(river, ID)
#
#
#   #process every timestep
#   total <- length(timesteps)
#   pb <- txtProgressBar(min = 0, max = total, style = 3)
#
#   for (ts in timesteps) {
#
#     #initiate discharge of the current timestep
#     Q <- vector("numeric", nSegments)
#     #process every segment
#
#     #progress bar
#
#     for (seg in 1:nSegments) {
#
#
#       #compute discharge of the segment at timestep
#       cell <- river$gridID[seg]
#       discharge <- NULL
#       gridID <- which(grid$ID == cell)
#
#       weight <- river$weights[seg]
#       area <- grid$area_m2[gridID]
#
#
#       if (unit == "mm/s") {
#         discharge <- runoff[gridID,ts]/1000 * area * weight
#       }
#       if (unit == "m3/s") {
#         discharge <- runoff[gridID,ts] * weight
#       }
#
#       #add discharge to the segment
#       Q[seg] <- Q[seg] + unclass(discharge)
#       #Q <- unlist(Q)
#       #add discharge to all downstream segments
#       DS <- river$DOWNSTREAM[[seg]]
#
#       if(!length(DS) == 0) {
#         rows <- which(Q_ts$ID %in% DS)
#         for (i in 1:length(rows)) {
#           Q[rows] <- Q[rows] + unclass(discharge)
#         }
#       }
#
#
#     }
#
#     #Q <- unlist(Q)
#     Q_ts <- cbind(Q_ts, Q)
#     col <- length(names(Q_ts))-1
#     names(Q_ts)[col] <- paste0("TS",ts)
#     setTxtProgressBar(pb, ts)
#   }
#   close(pb)
#   return(Q_ts)
# }
#
#
#
#
#
# compute_flow_using_area <- function(river, basins, grid, rID = "ARCID", vID = "ARCID", timesteps = NULL, verbose=TRUE, unit = "mm/s") {
#   # Start processing
#   nbasins <- NROW(basins)
#   nCells <- NROW(grid)
#   if (is.null(timesteps)) {
#     message("No timesteps specified: computing for all timesteps")
#     timesteps <- 1:(NCOL(grid)-3)
#   }
#
#   runoff <- dplyr::select(grid, -c(ID, area_m2)) %>%
#     sf::st_set_geometry(NULL)
#
#   #all downstream river segments
#   downstream <- river$DOWNSTREAM
#   #prepare a table to collect time series
#   Q_ts <- dplyr::select_(river, rID) %>% rename_("ID" = rID)
#
#   #basins id's corresponding river ids
#   vID <- dplyr::select_(basins, vID) %>%
#     sf::st_set_geometry(NULL) %>%
#     unlist()
#   rID <- dplyr::select_(river, rID) %>%
#     sf::st_set_geometry(NULL) %>%
#     unlist()
#
#   #process every timestep
#   total <- length(timesteps)
#   pb <- txtProgressBar(min = 0, max = total, style = 3)
#
#   for (ts in timesteps) {
#     p <- 0
#
#     # if (verbose == TRUE) {
#     #   # print progress first
#     #   tsp <- ts-min(timesteps)
#     #   tsp <- tsp/length(timesteps)
#     #   cat("\14")
#     #   cat("Processing: assigning runoff to river segments and accumulating flow.\n")
#     #   cat("Processing a total of", nbasins , "basins polygons on", nCells, "raster cells and ", length(timesteps) ," timesteps.\n\n")
#     #   cat(paste0("Overall progress: ", round(tsp*100,1),"% \n\n"))
#     #   cat("Current timestep (",ts,") processing progress: \n")
#     # }
#
#     #initiate discharge of the current timestep
#     Q <- vector("numeric", NROW(river))
#     #process every segment
#     for (seg in 1:nbasins) {
#
#       # if (verbose == TRUE) {
#       #   fivePercent <- round(nbasins/20,0)
#       #   onePercent <- round(nbasins/100,0)
#       #   if (seg %% fivePercent == 0) {
#       #     p <- p+5
#       #     cat(paste0(p,"%"))
#       #   } else if (seg %% onePercent == 0) {
#       #     cat(".")
#       #   }
#       # }
#
#
#       #compute discharge of the segment at timestep
#       cell <- basins$gridID[seg]
#       discharge <- NULL
#       gridID <- which(grid$ID == cell)
#       riverID <- which(rID == vID[seg])
#
#       weight <- basins$weights[seg]
#       area <- grid$area_m2[gridID]
#
#       if (unit == "mm/s") {
#         discharge <- runoff[gridID,ts]/1000 * area * weight
#       }
#       if (unit == "m3/s") {
#         discharge <- runoff[gridID,ts] * weight
#       }
#
#       #add discharge to the segment
#       Q[riverID] <- Q[riverID] + unclass(discharge)
#       #Q <- unlist(Q)
#       #add discharge to all downstream segments
#       DS <- river$DOWNSTREAM[[riverID]]
#
#       if(!length(DS) == 0) {
#         rows <- which(Q_ts$ID %in% DS)
#         for (i in 1:length(rows)) {
#           Q[rows] <- Q[rows] + unclass(discharge)
#         }
#       }
#     }
#     Q_ts <- cbind(Q_ts, Q)
#     col <- length(names(Q_ts))-1
#     names(Q_ts)[col] <- paste0("TS",ts)
#     setTxtProgressBar(pb, ts)
#   }
#   close(pb)
#   return(Q_ts)
# }
#
