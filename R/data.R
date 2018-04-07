#' Example river network
#'
#' A river network of 71 segments used in the vignette of hydrostreamer.
#'
#' @format An 'sf' LINESTRING object with one attribute.
#' \describe{
#'   \item{ID}{unique river segment ID}
#'   \item{geometry}{'sfc' geometry column}
#' }
"river"


#' Area of Interest polygon
#'
#' A polygon containing the area of interest, used in the vignette of hydrostreamer
#'
#' @format An 'sf' POLYGON object with no attributes.
#' \describe{
#'   \item{geometry}{'sfc' geometry column}
#' }
"basin"