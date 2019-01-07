######
# Methods for HS grid object
######


#' @export
print.HSgrid <- function(x,...) {
    print(HSgrid[["grid"]])
}

#' @export
plot.HSgrid <- function(x, ...) {
    plot(HSgrid[["grid"]], ...)
}

#' @export
summary.HSgrid <- function(object, ...) {
    print(HSgrid[["grid"]])
    print(summary(HSgrid[["runoff"]][[1]]))
}
