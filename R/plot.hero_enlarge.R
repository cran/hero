#' Plot a \code{hero_enlarge} object
#'
#' Plot the enlarged and original border defined be a
#' set of coordinates.
#' @param x An object of class \code{hero_enlarge}.
#' @param ... Additional graphical parameters passed to the
#' plotting method for \code{\link[sp]{SpatialPolygons-class}}
#' for \code{x$eborder}
#' @param blist A list of additional graphical parameters
#' passed to the plotting method for \code{\link[sp]{SpatialPolygons-class}}
#' for \code{x$border}.
#' @method plot hero_enlarge
#' @rdname plot.hero_enlarge
#' @export
#' @seealso \code{\link[sp]{SpatialPolygons-class}}
#' @importClassesFrom sp Polygon Polygons SpatialPolygons SpatialPoints
#' @importMethodsFrom sp plot
#' @examples
#' b = border.grid(lon, lat)
#' e = enlarge(b)
#' plot(e)
plot.hero_enlarge = function(x, ...,
                             blist = list(col = "grey")) {
  sp::plot(x$eborder, ...)
  blist$x = x$border
  blist$add = TRUE
  do.call("plot", blist)
}

