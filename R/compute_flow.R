
compute_flow_using_line <- function(river, grid, ID = "ID", type = "length", polygons = NULL, timesteps = NULL, verbose=TRUE, unit="mm/s") {
  # Start processing
  nSegments <- NROW(river)
  nCells <- NROW(grid)
  if (is.null(timesteps)) {
    message("No timesteps specified: computing for all timesteps")
    timesteps <- 1:(NCOL(grid)-3)
  }

  runoff <- select(grid, -c(ID, area_m2)) %>%
    st_set_geometry(NULL)

  #all downstream river segments
  downstream <- river$DOWNSTREAM
  #collect time series in a table
  Q_ts <- select_(river, ID)


  #process every timestep
  for (ts in timesteps) {
    p <- 0

    if (verbose == TRUE) {
      # print progress first
      tsp <- ts-min(timesteps)
      tsp <- tsp/length(timesteps)
      cat("\14")
      cat("Processing: assigning runoff to river segments and accumulating flow.\n")
      cat("Processing a total of", nSegments , "river segments on", nCells, "raster cells and ", length(timesteps) ," timesteps.\n\n")
      cat(paste0("Overall progress: ", round(tsp*100,1),"% \n\n"))
      cat("Current timestep (",ts,") processing progress: \n")
    }

    #initiate discharge of the current timestep
    Q <- vector("numeric", nSegments)
    #process every segment
    for (seg in 1:nSegments) {

      if (verbose == TRUE) {
        fivePercent <- round(nSegments/20,0)
        onePercent <- round(nSegments/100,0)
        if (seg %% fivePercent == 0) {
          p <- p+5
          cat(paste0(p,"%"))
        } else if (seg %% onePercent == 0) {
          cat(".")
        }
      }


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
      Q[seg] <- Q[seg] + unclass(discharge)
      #Q <- unlist(Q)
      #add discharge to all downstream segments
      DS <- river$DOWNSTREAM[[seg]]

      if(!length(DS) == 0) {
        rows <- which(Q_ts$ID %in% DS)
        for (i in 1:length(rows)) {
          Q[rows] <- Q[rows] + unclass(discharge)
        }
      }


    }
    #Q <- unlist(Q)
    Q_ts <- cbind(Q_ts, Q)
    col <- length(names(Q_ts))-1
    names(Q_ts)[col] <- paste0("TS",ts)
  }
  cat("\n\n Computation completed")
  return(Q_ts)
}






compute_flow_using_area <- function(river, voronoi, grid, rID = "ARCID", vID = "ARCID", timesteps = NULL, verbose=TRUE, unit = "mm/s") {
  # Start processing
  nVoronoi <- NROW(voronoi)
  nCells <- NROW(grid)
  if (is.null(timesteps)) {
    message("No timesteps specified: computing for all timesteps")
    timesteps <- 1:(NCOL(grid)-3)
  }

  runoff <- select(grid, -c(ID, area_m2)) %>%
    st_set_geometry(NULL)

  #all downstream river segments
  downstream <- river$DOWNSTREAM
  #collect time series in a table
  Q_ts <- select_(river, rID)

  #voronoi id's corresponding river ids
  vID <- select_(voronoi, vID) %>%
    st_set_geometry(NULL) %>%
    unlist()
  rID <- select_(river, rID) %>%
    st_set_geometry(NULL) %>%
    unlist()

  #process every timestep
  for (ts in timesteps) {
    p <- 0

    if (verbose == TRUE) {
      # print progress first
      tsp <- ts-min(timesteps)
      tsp <- tsp/length(timesteps)
      cat("\14")
      cat("Processing: assigning runoff to river segments and accumulating flow.\n")
      cat("Processing a total of", nVoronoi , "Voronoi polygons on", nCells, "raster cells and ", length(timesteps) ," timesteps.\n\n")
      cat(paste0("Overall progress: ", round(tsp*100,1),"% \n\n"))
      cat("Current timestep (",ts,") processing progress: \n")
    }

    #initiate discharge of the current timestep
    Q <- vector("numeric", NROW(river))
    #process every segment
    for (seg in 1:nVoronoi) {

      if (verbose == TRUE) {
        fivePercent <- round(nVoronoi/20,0)
        onePercent <- round(nVoronoi/100,0)
        if (seg %% fivePercent == 0) {
          p <- p+5
          cat(paste0(p,"%"))
        } else if (seg %% onePercent == 0) {
          cat(".")
        }
      }


      #compute discharge of the segment at timestep
      cell <- voronoi$gridID[seg]
      discharge <- NULL
      gridID <- which(grid$ID == cell)
      riverID <- which(rID == vID[seg])

      weight <- voronoi$weights[seg]
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
      #add discharge to all downstream segments
      DS <- river$DOWNSTREAM[[riverID]]

      if(!length(DS) == 0) {
        rows <- which(Q_ts$ID %in% DS)
        for (i in 1:length(rows)) {
          Q[rows] <- Q[rows] + unclass(discharge)
        }
      }


    }
    #Q <- unlist(Q)
    Q_ts <- cbind(Q_ts, Q)
    col <- length(names(Q_ts))-1
    names(Q_ts)[col] <- paste0("TS",ts)
  }
  cat("\n\n Computation completed")
  return(Q_ts)
}

