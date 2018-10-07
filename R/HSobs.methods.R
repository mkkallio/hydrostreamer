#' Constructor for \code{HSobs} object
#' 
#' Constructs an \code{HSobs} object from the input. 
#' 
#' @param obs a data.frame or a tibble with observations.
#' @param riverIDs A vector of riverID of the river segments of the columns
#'  in obs.
#'  
#' @return Returns a object of class \code{HSobs} which contains:
#'   \itemize{
#'     \item Observation timeseries
#'     \item riverIDs of the columns inthe observation timeseries
#'   } 
#' 
#' @export
create_HSobs <- function(obs, riverIDs) {
    if (!any(c("Date", "Month") %in% colnames(obs))) stop("Observations do not include 
                                                          column 'Date', or 'Month'.")
    try(names(riverIDs) <- colnames(obs)[-(colnames(obs) =="Date")])
    HSobs <- list(Observations = obs, riverIDs = riverIDs) 
    class(HSobs) <- "HSobs"
    return(HSobs)
}


