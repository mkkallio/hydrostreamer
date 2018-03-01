# no_cores <- detectCores() - 1  
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
# Q <- foreach(seg = 1:nSegments, .combine=rbind) %dopar% flow_iterate_timesteps(seg, river, grid, runoff, unit)
# stopCluster(cl)
# 

flow_iterate_segments <- function(seg, river, grid, runoff, unit, nSegments, ts) {
  #runoff.ts <- runoff[,ts]
  #compute discharge of the segment at timestep
  cell <- river$gridID[seg]
  discharge <- NULL
  gridID <- which(grid$ID == cell)
  weight <- river$weights[seg]
  area <- grid$area_m2[gridID]
  runoff <- grid[gridID,ts+2]
  
  
  if (unit == "mm/s") {
    discharge <- runoff[gridID,ts]/1000 * area * weight
  }
  if (unit == "m3/s") {
    discharge <- runoff[gridID,ts] * weight
  }
  if (length(discharge) == 0) { discharge <- 0}
  
  #add discharge to the segment
  Q <- vector("numeric", nSegments)
  Q[seg] <- Q[seg] + unclass(discharge)
  #Q <- unlist(Q)
  #add discharge to all downstream segments
  DS <- river$DOWNSTREAM[[seg]]
  
  # add discharge to downstream segments
  if(!length(DS) == 0) {
    rows <- which(river$ID %in% DS)
    for (i in 1:length(rows)) {
      Q[rows] <- Q[rows] + unclass(discharge)
    }
  }
  return(Q)
}


### doesnt work yet
flow_iterate_timesteps <- function(timestep, river, grid, runoff, unit, nSegments) {
    runoff.ts <- runoff[,timestep]
    Q <- foreach(seg = 1:nSegments, .combine= cbind) %dopar% 
      flow_iterate_timesteps(river= river, grid= grid, runoff= runoff.ts, unit= unit, nSegments=nSegments)
    Q <- rowSums(Q)
    #for (seg in 1:nSegments) {
    #  flow_iterate_timesteps(seg, river, grid, runoff, unit)
    #}
    #Q <- unlist(Q)
}



par_compute_flow_using_line <- function(river, grid, ID = "ID", type = "length", unit = "mm/s", timesteps = NULL, verbose=TRUE, no_cores=(detectCores()-1)) {
  # Start processing
  nSegments <- NROW(river)
  nCells <- NROW(grid)
  if (is.null(timesteps)) {
    message("No timesteps specified: computing for all timesteps")
    timesteps <- 1:(NCOL(grid)-3)
  }
  
  runoff <- select(grid, -c(ID, area_m2)) %>%
    st_set_geometry(NULL)
  #collect time series in a table
  Q_ts <- select_(river, ID)
  
  for (ts in timesteps) {
    #p <- 0
      if (verbose == TRUE) {
        # print progress first
        tsp <- ts-min(timesteps)
        tsp <- tsp/length(timesteps)
        #cat("\14")
        cat("Processing: assigning runoff to river segments and accumulating flow.\n")
        cat("Processing a total of", nSegments , "river segments on", nCells, "raster cells and ", length(timesteps) ," timesteps.\n\n")
        cat(paste0("Overall progress: ", round(tsp*100,1),"% \n\n"))
        #cat("Current timestep (",ts,") processing progress: \n")
      }

      #initiate discharge of the current timestep
      #Q <- vector("numeric", nSegments)
      #process every segment

      
      #, .export= list(river, grid, runoff, unit, flow_iterate_timesteps())
      clusterExport(cl, list("river", "grid", "unit", "ts", "flow_iterate_segments"))
      Q <- foreach(seg = 1:nSegments, .combine= cbind) %dopar%
        flow_iterate_segments(river= river, grid= grid, unit= unit, nSegments=nSegments, ts=ts)
      Q <- rowSums(Q)
      #for (seg in 1:nSegments) {
      # flow_iterate_timesteps(seg, river, grid, runoff, unit)
      #}
      #Q <- unlist(Q)
      Q_ts <- cbind(Q_ts, Q)
      col <- length(names(Q_ts))-1
      names(Q_ts)[col] <- names(runoff)[ts]
     # names(Q) <- paste0("TS",timesteps)
  }
  cat("\n\n Computation completed")
  return(Q_ts)
}