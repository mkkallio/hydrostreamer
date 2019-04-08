#' @method print HS 
#' @export 
print.HS <- function(x, ...) {
    
    cat("\nHydrostreamer")
    cat("\n")
    cat(paste0("No. objects: ", nrow(x)))
    cat("\n")
    
    if(hasName(x, "runoff_ts")){
        cat(paste0("No. runoff inputs: ", ncol(x$runoff_ts[[1]]-1)))
        cat("\n")
        cat("  Included runoff timeseries: ")
        print(colnames(x$runoff_ts[[1]][-1]))
        cat("\n")
    }
    
    if(hasName(x, "discharge_ts")){
        cat(paste0("No. discharge timeseries: ", ncol(x$discharge_ts[[1]]-1)))
        cat("\n")
        cat("  Discharge predictions: ")
        print(colnames(x$discharge_ts[[1]][-1]))
        cat("\n")
    }
    
    if(hasName(x, "observation_station")) {
        stations <- unique(x$observation_station)
        stations <- stations[!is.na(stations)]
        
        cat(paste0("No. observation stations: ", length(stations)))
        cat("\n")
        cat("  Stations: ", paste(stations, sep=" "))
        cat("\n")
    }
    
    if(hasName(x, "control_ts")) {
        controls <- table(x$control_type)
        
        cat(paste0("No. of flow controls: ", sum(controls)))
        cat("\n")
        cat("  Control types: ")
        print(controls)
        
    }
    cat("\n")

    NextMethod()
}





