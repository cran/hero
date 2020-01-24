#' Enlarge spatial domain
#'
#' Enlarge the spatial domain of a
#' \code{\link[sp]{SpatialPolygons-class}} object.  If
#' \code{width} isn't specified, then 10\% of the maximum
#' distance between the points specified by
#' \code{sp::bbox(x)} is used.  The
#' \code{\link[rgeos]{gBuffer}} function is used to enlarge
#' \code{x}.
#'
#' @param x A \code{\link[sp]{SpatialPolygons-class}} object
#'   defining the border(s) of the spatial domain.
#' @param width The width to enlarge the study area.
#'   Distance from original geometry to include in the new
#'   geometry. Negative values are allowed.
#' @param ... Additional arguments to pass to
#'   \code{\link[rgeos]{gBuffer}}.
#'
#' @return An object of class \code{hero_enlarge}.  This is
#'   simply a list with \code{eborder} (the enlarged
#'   border), \code{border} (the border of the original
#'   coordinates), and the width of the englargement.
#'   \code{eborder} and \code{border} are
#'   \code{\link[sp]{SpatialPolygons-class}} objects.
#' @export
#' @author Joshua French
#' @examples
#' # enlarge regular grid
#' # create x and y defining square border
#' x = seq(min(lon), max(lon), length = 60)
#' y = seq(min(lat), max(lat), length = 80)
#' border = border.grid(x, y)
#' e = enlarge(border)
#' plot(e)
#'
#' # create x and y defininging an irregular grid
#' border2 = border.grid(lon, lat)
#' e2 = enlarge(border2)
#' plot(e2)
enlarge = function(x, width, ...) {
  if (class(x) != "SpatialPolygons") {
    stop("x must have class SpatialPolygons from the sp package")
  }
  if (missing(width)) {
    width = max(sp::spDists(t(sp::bbox(x))))/10
  } else {
    if (length(width) != 1 | !is.numeric(width)) {
      stop("width must be a real number")
    }
  }

  epoly = rgeos::gBuffer(x, width  = width, ...)
  out = list(eborder = epoly, border = x, width = width)
  class(out) = "hero_enlarge"
  return(out)
}

