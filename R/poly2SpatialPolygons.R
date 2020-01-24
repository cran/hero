#' Convert simple polygon to a \code{SpatialPolygons} object
#'
#' This function takes a simple polygon and attempts to
#' convert it to a \code{\link[sp]{SpatialPolygons}} object.
#' This list is assumed to have components \code{x} and
#' \code{y} that define the boundary of the polygon.
#'
#' @param x A list with components \code{x} and \code{y}.
#' @param ID The name of the resulting polygon.  Default is
#' \code{"border"}.
#'
#' @return A \code{\link[sp]{SpatialPolygons}} object
#' @export
#' @author Joshua French
#' @examples
#' angle = seq(0, 2 * pi, len = 100)
#' poly = list(x = cos(angle), y = sin(angle))
#' plot(poly, type = "l", asp = 1)
#' sppoly = poly2SpatialPolygons(poly)
#' library(sp)
#' plot(sppoly, axes = TRUE, asp = 1)
poly2SpatialPolygons = function(x, ID = "border") {
  if (!is.list(x)) {
    stop("x must be a list with components x and y")
  }
  if(is.null(x$x) | is.null(x$y)) {
    stop("x must have components x and y")
  }
  sppoly = sp::Polygon(cbind(x$x, x$y), hole = FALSE)
  sppoly = sp::Polygons(list(sppoly), ID = ID)
  sp::SpatialPolygons(list(sppoly))
}
