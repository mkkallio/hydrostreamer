######
# constructor and methods for HS grid object
######

# constructor function to be added
HSgrid <- function(river, runoff) {
    
}

#' @export
print.HSgrid <- function(HSgrid) {
    print(HSgrid[["grid"]])
}

#' @export
plot.HSgrid <- function(HSgrid, ...) {
    plot(HSgrid[["grid"]], ...)
}

#' @export
summary.HSgrid <- function(HSgrid) {
    print(HSgrid[["grid"]])
    print(summary(HSgrid[["runoff"]][[1]]))
}
