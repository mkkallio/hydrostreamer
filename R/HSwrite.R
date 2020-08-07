#' Writes an HS object to disk.
#' 
#' Writes out \code{HS} objects in order to use them outside R. By default
#' the function writes the geometry using \code{\link[sf]{st_write}}, but where
#' list columns are modified since they cannot be written by st_write. 
#' Alternatively the function can output a timeseries specified by the user 
#' (runoff, discharge, or control).
#'
#' @param x A \code{HS} object.
#' @param filename Filename to write to.
#' @param what What to write. Accepts \code{"geometry"} for writing the river
#'   network or the runoff grid, or \code{"discharge_ts", "runoff_ts", 
#'   "control_ts"} for writing out a .csv table of the associated timeseries. 
#' @param ... options passed to \code{\link[sf]{st_write}}.
#' 
#' @export
HSwrite <- function(x, filename, what = "geometry", ...) {
    UseMethod("HSwrite")
}

#' @export
HSwrite.HS <- function(x, filename, what = "geometry", ...) {
    
    Optimisation_info <- NULL
    observation_ts <- NULL
    
    ##################
    # WRITE OUT GEOMETRY
    if (what == "geometry") {
        test <- hasName(x, "NEXT")
        if(test) x$PREVIOUS <- lapply(x$NEXT, 
                                      function(x) {
                                          paste(x, collapse=" ")
                                      }) %>% unlist()
        
        test <- hasName(x, "PREVIOUS")
        if(test) x$PREVIOUS <- lapply(x$PREVIOUS, 
                                      function(x) {
                                          paste(x, collapse=" ")
                                      }) %>% unlist()
        
        test <- hasName(x, "runoff_ts") 
        if (test) {
            x$runoff_ts <- rep(TRUE, nrow(x))
        }
        
        test <- hasName(x, "discharge_ts") 
        if (test) {
            x$discharge_ts <- rep(TRUE, nrow(x))
        }
        
        test <- hasName(x, "Optimisation_info") 
        if (test) {
            x <- dplyr::select(x, -Optimisation_info)
        }
        
        test <- hasName(x, "observation_ts") 
        if (test) {
            x <- dplyr::select(x, -observation_ts)
        }
        
        test <- hasName(x, "control_ts") 
        if (test) {
            replace <- !sapply(x$control_ts, is.null)
            x$control_ts <- replace 
        } 
        
        sf::st_write(x, filename, ...)
        
        
    ######################
    # WRITE OUT A TABLE OF RUNOFF_TS
    } else if (what == "runoff") {
        
        test <- hasName(x, "runoff_ts")
        if(test) {
            data <- collect_listc(x$runoff_ts, acc=TRUE)
            
            for(pred in seq_along(data)) {
                name <- paste0(filename, "_", names(data)[pred], ".csv")
                
                test <- requireNamespace("readr") 
                if(test) {
                    readr::write_csv(data[[pred]], name)
                } else {
                    write.csv(data[[pred]], file = name)
                }
            }
        } else {
            stop("No runoff timeseries in the input - no output written")
        }
        
    
    ######################
    # WRITE OUT A TABLE OF DISCHARGE_TS
    } else if (what == "discharge") {
        
        test <- hasName(x, "discharge_ts")
        if(test) {
            data <- collect_listc(x$discharge_ts, acc=TRUE)
            
            for(pred in seq_along(data)) {
                name <- paste0(filename, "_", names(data)[pred], ".csv")
                
                test <- requireNamespace("readr") 
                if(test) {
                    readr::write_csv(as.data.frame(data[[pred]]), name)
                } else {
                    write.csv(as.data.frame(data[[pred]]), file = name)
                }
            }
        } else {
            stop("No discharge timeseries in the input - no output written")
        }
        
    ######################
    # WRITE OUT A TABLE OF CONTROL_TS
    } else if (what == "control") {
        
        test <- hasName(x, "control_ts")
        if(test) {
            data <- collect_listc(x$control_ts, acc=TRUE)
            
            for(pred in seq_along(data)) {
                name <- paste0(filename, "_", names(data)[pred], ".csv")
                
                test <- requireNamespace("readr") 
                if(test) {
                    readr::write_csv(data[[pred]], name)
                } else {
                    write.csv(data[[pred]], file = name)
                }
            }
        } else {
            stop("No runoff timeseries in the input - no output written")
        }
        
    }
}