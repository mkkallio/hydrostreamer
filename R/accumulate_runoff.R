#' Apply river routing based on selected routing method.
#' 
#' Apply river routing using any of the provided methods. The function takes 
#' \code{HSrunoff} object as an input.
#'
#' There are currently three routing algorithms implemented: \code{'instant'}aneous
#' flow, where all runoff is routed through the entire river network at every 
#' timestep. There is no lag in instantaneous routing, which  means it may not be
#' reasonable for large river networks. 'simple' river routing contains a simple lag 
#' based on given flow velocity and river segment lengths. 'muskingum' implements 
#' a muskingum river routing algorithm where k (storage) parameter is computed from 
#' given flow velocity and river segment length. Large difference in timesteps of 
#' runoff may result in computational instability. See further details from the 
#' documentation of each method.
#'
#' @param HSrunoff A 'HSrunoff' object obtained by \code{\link{downscale_runoff}}
#' @param method Character string specifying the method to be used. 
#' @param ... Arguments passed to the routing algorithm.
#' @inheritParams compute_HSweights
#'
#'
#' @return Returns an object of class \code{HSflow}) with 
#' \itemize{
#'   \item \code{river} River network inherited from input \code{HSrunoff}.
#'   \item \code{discharge} A list of tables with estimated discharge for each 
#'     river segment and each timestep.
#'   \item \code{Routing_Method} The algorithm used for routing.
#' }
#' @export
accumulate_runoff <- function(HSrunoff, method=c("instant", "simple", "muskingum"), 
                              ..., verbose = FALSE) {
  
    class(HSrunoff) <- method[1]
    UseMethod("accumulate_runoff", HSrunoff)
  
}


#' Compute instantaneous flow for each timestep
#' 
#' Applies the simplest possible river routing scheme, instantaenous flow, by 
#' adding runoff from each river segment to all of the segments downstream, for 
#' each timestep.
#'
#' @inheritParams accumulate_runoff
#'
#' @return Returns an object of class \code{HSflow}) with 
#' \itemize{
#'   \item \code{river} River network inherited from input \code{HSrunoff}.
#'   \item \code{discharge} A list of tables with estimated discharge for each 
#'     river segment and each timestep.
#'  \item \code{Routing_Method} The algorithm used for routing.
#' }
#' @export
accumulate_runoff.instant <- function(HSrunoff, verbose = FALSE, ...) {
    
    output <- list(river = HSrunoff$river, discharge = list())
    
    # process all of downscaled runoff
    total <- length(HSrunoff$downscaled)
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    for (d in seq_along(HSrunoff$downscaled)) {
        
        # PREPARE INPUT FOR FORTRAN SUBROUTINE
        nriv <- NROW(HSrunoff$river)
        nts <- nrow(HSrunoff$downscaled[[d]])
        rID <- HSrunoff$river$riverID %>% unlist()
        n <- HSrunoff$river$NEXT
        runoff <- HSrunoff$downscaled[[d]] %>%
            dplyr::select(-Date) %>%
            as.matrix()
        
        Q <- runoff
        
        Q <- .Fortran("routing_instant", 
                        PACKAGE = "hydrostreamer",
                        as.integer(nriv),
                        as.integer(nts),
                        as.integer(rID),
                        as.integer(n),
                        runoff,
                        Q)[[6]]
        

        Q <- data.frame(Q)
        colnames(Q) <- rID
        Q <- cbind(Date = HSrunoff$downscaled[[d]]$Date, Q)
        Q <- dplyr::select(Q, Date, dplyr::everything())
        
        
        if(is.null(names(HSrunoff$downscaled))) {
            output$discharge[[d]] <- Q
        } else {
            name <- names(HSrunoff$downscaled)[d]
            output$discharge[[ name ]] <- Q
        }
        
        if (verbose) setTxtProgressBar(pb, d)
        
    }
    if (verbose) close(pb)
    output[["Routing_Method"]] <- "Instantaneous flow (accumulate_runoff.instant)"
    class(output) <- "HSflow"
    return(output)
}


#' Implements 'muskingum' routing algorithm for vector river network
#' 
#' The function implements 'muskingum' routing scheme where the storage parameter
#' \emph{k} is computed using user input flow velocity, and the length of a 
#' river segment. Using 'muskingum' for runoff data with time interval between
#' runoff longer than day may cause instability in the output. If the interval is
#' too high, use another algorithm.
#' 
#' Warning: The function is experimental and has not been thoroughly tested yet.
#' 
#' @param HSrunoff A \code{HSrunoff} object.
#' @param velocity Flow velocity applied to compute parameter x. Can be a constant,
#' or a vector of flow values at river segments.
#' @param x Value for parameter x.
#' @inheritParams compute_HSweights
#' 
#' @return Returns an object of class \code{HSflow}) with 
#' \itemize{
#'   \item \code{river} River network inherited from input \code{HSrunoff}.
#'   \item \code{discharge} A list of tables with estimated discharge for each 
#'     river segment and each timestep.
#'   \item \code{Routing_Method} The algorithm used for routing.
#' }
#' 
#' @export
accumulate_runoff.muskingum <- function(HSrunoff, velocity=1, x, verbose=FALSE, ...) {
    
    lengths <- sf::st_length(HSrunoff$river) %>% unclass()
    IDs <- select(HSrunoff$river, riverID) %>% st_set_geometry(NULL) %>% unlist()
    order <- HSrunoff$river %>%
        select(riverID, UP_SEGMENTS) %>%
        st_set_geometry(NULL) %>%
        arrange(UP_SEGMENTS) %>%
        select(riverID) %>%
        unlist() %>%
        match(IDs)
    
    k <- lengths/velocity
    
    output <- list(river = HSrunoff$river, discharge = list())
    
    # process all of downscaled runoff
    total <- length(HSrunoff$downscaled)
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    for (d in seq_along(HSrunoff$downscaled)) {
    
        dates <- HSrunoff$downscaled[[d]]$Date
        Q <- HSrunoff$downscaled[[d]]
        nextID <- HSrunoff$river$NEXT %>%
            match(colnames(HSrunoff$downscaled[[d]]))
        
        intervals <- vector("numeric", length(dates))
        for (i in seq_along(intervals)) {
            if (i == length(intervals)) {
                intervals[i] <- lubridate::interval(dates[i], 
                                                    lubridate::ceiling_date(dates[i], unit="month")) / lubridate::seconds(1)
            } else {
                intervals[i] <- lubridate::interval(dates[i], dates[i+1]) / lubridate::seconds(1)
            }
        }
        
        krule <- mean(intervals) < 2*k*x
        
        if (any(krule == FALSE)) {
            warning(paste0( (sum(krule == FALSE) / length(krule)), " % of river segments do not satisfy 
                            'dt < 2kx' rule. Results may be unstable." ))
        }
        
        for (i in order) {
            if(is.na(nextID[i])) {
                next
            }
            mat <- matrix(0, nrow(Q)+1, 2)
            mat[1:nrow(Q), 1] <- Q[,i+1]
            mat[nrow(mat),1] <- mat[nrow(mat)-1,1]
            for (t in 1:(nrow(mat)-1)) {
                C0 <- -(k[i]*x - 0.5*intervals[t]) / (k[i] - k[i]*x + 0.5*intervals[t])
                C1 <- (k[i]*x + 0.5*intervals[t]) / (k[i] - k[i]*x + 0.5*intervals[t])
                C2 <- (k[i] - k[i]*x - 0.5*intervals[t]) / (k[i] - k[i]*x + 0.5*intervals[t])
                mat[t+1, 2] <- C0 * mat[t+1, 1] + C1 * mat[t, 1] + C2 * mat[t, 2]
                
            }
            mat[mat[, 2] < 0, 2] <- 0
            Q[, nextID[i] ] <- Q[, nextID[i] ] + mat[1:(nrow(mat)-1),2]
            
        }

        if(is.null(names(HSrunoff$downscaled))) {
            output$discharge[[d]] <- Q
        } else {
            name <- names(HSrunoff$downscaled)[d]
            output$discharge[[ name ]] <- Q
        }
        
        if (verbose) setTxtProgressBar(pb, ts)
    }
    
    if (verbose) close(pb)
    output[["Routing_Method"]] <- "Muskingum (accumulate_runoff.muskingum)"
    class(output) <- "HSflow"
    return(output)

}


#' Implements a simple lagged river routing algorithm.
#' 
#' Implements a simple lag-based routing algorithm which can be used with
#' arbitrary intervals between runoff units. Includes a possibility to
#' add boundary condition for river routing at certain river segments. 
#' 
#' If a boundary condition is supplied, inflow to the specified river segment 
#' is set to the value provided in boundary condition. The boundary condition
#' timeseries must equal the runoff timeseries being routed.
#'
#' @param boundary A \code{HSobs} object with boundary condition for routing.
#' @inheritParams accumulate_runoff.muskingum
#' @inheritParams compute_HSweights
#'
#' @return Returns an object of class \code{HSflow}) with 
#' \itemize{
#'   \item \code{river} River network inherited from input \code{HSrunoff}.
#'   \item \code{discharge} A list of tables with estimated discharge for each 
#'     river segment and each timestep.
#'   \item \code{Routing_Method} The algorithm used for routing.
#' }
#' 
#' @export
accumulate_runoff.simple <- function(HSrunoff, velocity=1, boundary = NULL, verbose=FALSE,...) {
    
    lengths <- sf::st_length(HSrunoff$river) %>% unclass()
    IDs <- dplyr::select(HSrunoff$river, riverID) %>% sf::st_set_geometry(NULL) %>% unlist()
    order <- HSrunoff$river %>%
        dplyr::select(riverID, UP_SEGMENTS) %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::arrange(UP_SEGMENTS) %>%
        dplyr::select(riverID) %>%
        unlist() %>%
        match(IDs)
    
    duration <- lengths/velocity
    
    output <- list(river = HSrunoff$river, discharge = list())
    
    # process all of downscaled runoff
    total <- length(HSrunoff$downscaled)
    if (verbose) pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    for (d in seq_along(HSrunoff$downscaled)) {
        
        
        dates <- HSrunoff$downscaled[[d]]$Date
        Q <- HSrunoff$downscaled[[d]]
        
        # check boundary condition
        if(!is.null(boundary)) {
          bind <- which(boundary$riverIDs %in% IDs)
          for(i in seq_along(bind)) {
            rind <- which(colnames(Q) == boundary$riverIDs[bind[i]])
            Q[, rind ] <- Q[, rind ] + boundary$Observations[,bind[i]+1]
          }
              
        }
        nextID <- HSrunoff$river$NEXT %>%
            match(colnames(HSrunoff$downscaled[[d]]))
        
        intervals <- vector("numeric", length(dates))
        for (i in seq_along(intervals)) {
            if (i == length(intervals)) {
                intervals[i] <- lubridate::interval(dates[i], 
                                                    lubridate::ceiling_date(dates[i], unit="month")) / lubridate::seconds(1)
            } else {
                intervals[i] <- lubridate::interval(dates[i], 
                                                    dates[i+1]) / lubridate::seconds(1)
            }
        }
      
        for (i in order) {
            if(is.na(nextID[i])) {
                next
            }
            
            # # check boundary condition
            if(!is.null(boundary)) {
                if(nextID[i] %in% boundary$riverIDs) {
                  next
                }
            }
            
                    
            mat <- matrix(0, nrow(Q)+1, 4)
            mat[1:nrow(Q)+1, 1] <- intervals
            mat[1:nrow(Q)+1, 2] <- Q[,i+1]
            mat[1,] <- mat[2,]
            for (t in 1:(nrow(mat))) {
                outflow <- mat[t,2] * (1-duration[i]/mat[t,1])
                mat[t,3] <- outflow
                storage <- (mat[t,2] - outflow)*mat[t,1]
                if(t != nrow(mat)) mat[t+1,2] <- mat[t+1,2] + (mat[t,2] - outflow)
            }
            Q[, nextID[i] ] <- Q[, nextID[i] ] + mat[2:(nrow(mat)),3]
        }
        
        if(is.null(names(HSrunoff$downscaled))) {
            output$discharge[[d]] <- Q
        } else {
            name <- names(HSrunoff$downscaled)[d]
            output$discharge[[ name ]] <- Q
        }
        
        if (verbose) setTxtProgressBar(pb, ts)
        }
    
    if (verbose) close(pb)
    output[["Routing_Method"]] <- "Simple lag (accumulate_runoff.simple)"
    class(output) <- "HSflow"
    return(output)
}



