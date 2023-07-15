#' Enlarge spatial domain
#'
#' Enlarge the spatial domain of a
#' \code{\link[sp]{SpatialPolygons-class}} object.  If
#' \code{width} isn't specified, then 10\% of the maximum
#' distance between the points specified by the bounding
#' box is used. The
#' \code{\link[sf]{st_buffer}} function is used to enlarge
#' \code{x}.
#'
#' @param x A \code{\link[sp]{SpatialPolygons-class}} object
#'   defining the border(s) of the spatial domain.
#' @param width The width to enlarge the study area.
#'   Distance from original geometry to include in the new
#'   geometry. Negative values are allowed.
#' @param ... Additional arguments to pass to
#'   \code{\link[sf]{st_buffer}}.
#'
#' @return An object of class \code{hero_enlarge}.  This is
#'   simply a list with \code{eborder} (the enlarged
#'   border), \code{border} (the border of the original
#'   coordinates), and the width of the enlargement.
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
  # is width missing?
  missing_width <- missing(width)
  if (!is.element("SpatialPolygons", class(x))) {
    stop("x must have class SpatialPolygons from the sp package")
  }
  x_sf = sf::st_as_sf(x, "sf")
  if (missing_width) {
    x_bbox <- sf::st_bbox(x_sf)
    x_stp <-
      sf::st_as_sf(
        data.frame(x = x_bbox[c(1, 3)],
                   y = x_bbox[c(2, 4)]),
        crs = sf::st_crs(x_sf),
        coords = 1:2)
    # width = max(sp::spDists(t(sp::bbox(x))))/10
    width = max(sf::st_distance(x_stp))/10
  } else {
    if (length(width) != 1 | !is.numeric(width)) {
      stop("width must be a real number")
    }
  }

  # remove dependency on rgeos
  # epoly = rgeos::gBuffer(x, width  = width, ...)
  epoly_sf = sf::st_buffer(x_sf, dist = width)
  # epoly = sf::as_Spatial(epoly_sf)
  out = list(eborder = sf::as_Spatial(epoly_sf),
             border = x, width = width)
  class(out) = "hero_enlarge"
  return(out)
}

