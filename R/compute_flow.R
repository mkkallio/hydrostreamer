#' Computes specific runoff generated in the river segment based on either polygon or area weighting.
#'
#' @param x An object of class 'HSrgrid' or 'HSragrid', obtained with compute_weights().
#' @param timesteps A logical vector of the length of timesteps in x, or a numeric vector specifying which timesteps to process. If no timesteps are given, all will be processed.
#' @param unit Unit of runoff. Can be either "mm/s", or "m3/s". Defaults to mm/s (equivalent to kg/m2/s)
#'
#' @return River network ('sf' linestring object) with river segment specific runoff for all timesteps processed as attribute columns. Object class 'HSrunoff'.
#' @export
#'
#' @examples
#' \dontrun{
#' compute_segment_runoff(voronoi_weigths)
#' compute_segment_runoff(voronoi_weigths, timesteps=1:12)
#' }
compute_segment_runoff <- function(x, timesteps=NULL, unit="mm/s") {
    UseMethod("compute_segment_runoff")
}


#' @export
compute_segment_runoff.HSrgrid <- function(x, timesteps = NULL, unit="mm/s") {

    if (is.null(timesteps)) {
          river <- compute_runoff_using_line(x[[1]], x[[2]], unit="mm/s")
    } else {
          river <- compute_runoff_using_line(x[[1]], x[[2]], timesteps=timesteps, unit="mm/s")
    }
  return(river)
}

#' @export
compute_segment_runoff.HSragrid <- function(x, timesteps = NULL, unit="mm/s") {

  if (is.null(timesteps)) {
      river <- compute_runoff_using_area(x[[1]], x[[2]], x[[3]], unit="mm/s")
  } else {
      river <- compute_runoff_using_area(x[[1]], x[[2]], x[[3]], timesteps=timesteps, unit="mm/s")
  }
  return(river)
}


#' Computes specific runoff generated in the river segment based on line weighting.
#'
#' @param river A 'HSrnet' object obtained with river_network() function.
#' @param grid A 'HSgrid' object obtained with polygrid_timeseries() function.
#' @param ID Name of the column in river with unique IDs.
#' @param unit Either "mm/s", or "m3/s", depending on the unit of runoff timeseries in grid.
#' @inheritParams compute_segment_runoff
#'
#' @return Returns input river object (class 'HSrunoff') added with timeseries information as new attribute columns.
#' @export
#'
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
  Q_ts$PREVIOUS <- river$PREVIOUS
  Q_ts$NEXT <- river$NEXT
  Q_ts$DOWNSTREAM <- river$DOWNSTREAM
  #Q_ts <- add_column(Q_ts, PREVIOUS = river$PREVIOUS, NEXT = river$NEXT, DOWNSTREAM = river$DOWNSTREAM, .after=1)
  return(Q_ts)
}



#' Computes specific runoff generated in the river segment based on area weighting.
#'
#' @param basins An 'sf' polygon object with basin specific areas. May be obtained with river_voronoi(), delineate_basins(),
#'   or some other means. Must have an ID column corresponding to the river segment IDs, and a weights column.
#' @param rID Name of the column in river with unique IDs.
#' @param bID Name of the column in basins with IDs corresponding to rID.
#' @inheritParams compute_runoff_using_line
#'
#' @return Returns input river object (class 'HSrunoff') added with timeseries information as new attribute columns.
#' @export
#'
#'
compute_runoff_using_area <- function(river, basins, grid, rID = "ID", bID = "rID", timesteps = NULL, unit = "mm/s") {
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
  Q_ts <- dplyr::select_(river, rID) %>% dplyr::rename_("ID" = rID)

  #basins id's corresponding river ids
  bID <- dplyr::select_(basins, bID) %>%
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
      riverID <- which(rID == bID[seg])

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
  Q_ts <- tibble::add_column(Q_ts, PREVIOUS = river$PREVIOUS, NEXT = river$NEXT, DOWNSTREAM = river$DOWNSTREAM, .after=1)
  return(Q_ts)
}



#' Compute river flow based on selected routing method.
#'
#'
#' @param x A 'HSrunoff' object obtained by compute_segment_runoff()
#' @param method Character string specifying the method to be used.
#'
#' @section Details:
#' Currently only simple addition downstream is implemented, specified by value "simple" in method input.
#'
#' @return Returns the input river network where runoff timeseries is switched to accumulated flow for each river segment.
#' @export
#'
#'
accumulate_flow <- function(x, method="simple") {
  if(method == "simple") {
      acc <- accumulate_runoff_addition(x)
  } else {
    stop("No other method except simple addition downstream is implemented yet")
  }

  return(acc)
}




#' Accumulate runoff downstream
#' 
#' Applies the simplest possible river routing scheme by adding runoff to all segments downstream, for each timestep.
#'
#' @inheritParams accumulate_flow
#'
#' @return Returns the input river network where runoff timeseries is switched to accumulated flow for each river segment.
#' @export
#'
#'
accumulate_runoff_addition <- function(x) {
  # Start processing
  nSegments <- NROW(x)

  runoff_cols <- grepl("TS", names(x), fixed=TRUE)
  runoff <- x[,runoff_cols] %>%
      sf::st_set_geometry(NULL)

  timesteps <- 1:(NCOL(runoff))


  #prepare a table to collect time series
  keep <- names(x)[!runoff_cols]
  Q_ts <- dplyr::select_(x, keep)

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
      DS <- x$DOWNSTREAM[[seg]]

      if(!length(DS) == 0) {
        rows <- which(Q_ts$ID %in% DS)
        Q[rows,ts] <- Q[rows,ts] + unclass(discharge)
      }
    }
    setTxtProgressBar(pb, ts)
  }
  close(pb)

  Q <- tibble::add_column(Q, ID = x$ID)
  Q_ts <- merge(Q_ts, Q)
  class(Q_ts) <- append(class(Q_ts), "HSflow")
  return(Q_ts)
}




