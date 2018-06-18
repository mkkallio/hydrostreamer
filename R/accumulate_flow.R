#' Apply river routing based on selected routing method.
#' 
#' Apply river routing using any of the provided methods using a monthly timestep. The function takes a 
#' \code{HSrunoff} object as an input.
#'
#' Currently only instantaneous flow downstream is implemented, specified by value "simple" in method input.
#'
#' @param HSrunoff A 'HSrunoff' object obtained by \code{\link{compute_segment_runoff}}
#' @param method Character string specifying the method to be used.
#' @inheritParams compute_weights
#'
#'
#' @return Returns the river network (without routing information) where runoff timeseries is switched to 
#' routed and accumulated flow.
#' 
#' @export
accumulate_flow <- function(HSrunoff, method="instant", verbose = FALSE) {
  if(method == "instant") {
      acc <- accumulate_flow_instant(HSrunoff, verbose = verbose)
  } else {
    stop("No other method except simple instantaneous flow downstream is implemented yet")
  }

  return(acc)
}




#' Compute instantaneous flow for each timestep
#' 
#' Applies the simplest possible river routing scheme, instantaenous flow, by adding runoff from each river 
#' segment to all of the segments downstream, for each timestep.
#'
#' @inheritParams accumulate_flow
#'
#' @return Returns the input river network (class \code{HSflow}) where runoff timeseries is switched to 
#' the accumulated flow for each river segment.
#' 
#' @export
accumulate_flow_instant <- function(HSrunoff, verbose = FALSE) {
  # Start processing
  nSegments <- NROW(HSrunoff)

  runoff_cols <- grepl("TS", names(HSrunoff), fixed=TRUE)
  runoff <- HSrunoff[,runoff_cols] %>%
      sf::st_set_geometry(NULL)

  timesteps <- 1:(NCOL(runoff))


  #prepare a table to collect time series
  keep <- names(HSrunoff)[!runoff_cols]
  Q_ts <- dplyr::select_(HSrunoff, keep)

  #process every timestep
  total <- length(timesteps)
  if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)

  # initiate accumulated runoff
  Q <- runoff
  for (ts in timesteps) {

    #process every segment
    for (seg in 1:nSegments) {

      #add discharge to all downstream segments
      discharge <- runoff[seg,ts]
      DS <- HSrunoff$DOWNSTREAM[[seg]]

      if(!length(DS) == 0) {
        rows <- which(Q_ts$riverID %in% DS)
        Q[rows,ts] <- Q[rows,ts] + unclass(discharge)
      }
    }
      if (verbose) setTxtProgressBar(pb, ts)
  }
  if (verbose) close(pb)

  Q <- tibble::add_column(Q, riverID = HSrunoff$riverID)
  Q_ts <- merge(Q_ts, Q)
  class(Q_ts) <- append(class(Q_ts), "HSflow")
  return(Q_ts)
}




