######
# Methods for HS grid object
######


#' @export
print.HSgrid <- function(x,...) {
    print(x[["grid"]])
}

#' @export
plot.HSgrid <- function(x, ...) {
    plot(x[["grid"]], ...)
}

#' @export
summary.HSgrid <- function(object, ...) {
    print(object[["grid"]])
    print(summary(object[["runoff"]][[1]]))
}
