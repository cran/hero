#' Radial basis spline specification
#'
#' \code{radspline} specifies a set of radial basis splines.
#' \code{nknots} is the approximate number of knots to
#' sample in the (usually) enlarged study area.  If
#' \code{eborder} is not provided, then \code{eborder} is
#' automatically constructed by enlarging the \code{border}
#' object using the \code{\link{enlarge}} function and
#' \code{width}. See Details for additional information
#' about sampling the knot locations.
#'
#' The \code{\link[sp]{spsample}} function is used to
#' "automatically" select the knot locations within
#' \code{eborder}.  \code{nknots} corresponds to the
#' \code{n} argument in that function.  A hexagonal sampling
#' scheme is used by default, but other options are
#' available.
#'
#' Great circle distance IS NOT used in sampling from the
#' regular grid.  This is computationally expensive, so it
#' has not been implemented.  Great circle distance is only
#' used  when the constructed \code{hero_radspline} is
#' evaluated (and \code{longlat = TRUE}).
#'
#' @param nknots The approximate number of knots locations.
#'   Can be a vector of positive integers for successive
#'   samplings. See Details.
#' @param poverlap The proportional amount of overlap (>=1)
#'   beyond the nearest neighbor knots.  Default is 2.
#' @param k The order of the Wendland covariance function.
#' @param border A \code{\link[sp]{SpatialPolygons-class}}
#'   object.  If \code{eborder} is not supplied, this will
#'   be used to determine the sampling region for the knots.
#'   See Details.
#' @param width The width for the border enlargement.
#' @param type The sampling type for
#'   \code{\link[sp]{spsample}}.  The default is
#'   \code{"hexagonal"}.
#' @param longlat A logical value indicating whether Great
#'   Circle distances should be used (\code{TRUE}) or
#'   Euclidean distances (\code{FALSE}).  The default is
#'   \code{FALSE}.
#' @param eborder A \code{\link[sp]{SpatialPolygons-class}}
#'   object.  The enlarged border from which the knots will
#'   be selected.  If not supplied, this is automatically
#'   computed using \code{border} and \code{width}.
#' @param ... Additional arguments passed to
#'   \code{\link[sp]{spsample}}.
#'
#' @return A \code{hero_radspline} object.
#' @export
#' @examples
#' border = border.grid(lon, lat)
#' r = radspline(nknots = c(36, 36 * 4), border = border)
#' # default color scheme
#' plot(r)
#' # change color and point styles of points,
#' # and background of original domain
#' plot(r, blist = list(col = "yellow"),
#'         glist = list(col = c("blue", "orange"),
#'                      pch =  3:4))
radspline = function(nknots, border,
                     poverlap = 2, k = 2,
                     width, type = "hexagonal",
                     longlat = FALSE,
                     eborder, ...) {
  if (missing(border)) {
    if (missing(eborder)) {
      stop("border or eborder must be supplied")
    } else {
      border = eborder
      if (missing(width)) {
        width = 0
      }
    }
  }
  if (missing(eborder)) {
    if (missing(border)) {
      stop("border or eborder must be supplied")
    } else {
      if (missing(width)) {
        e = enlarge(border)
      } else {
        e = enlarge(border, width = width)
      }
    }
    eborder = e$eborder
    width = e$width
  } else {
    if (missing(width)) {
      width = 0
    }
  }
  if (!is.numeric(nknots) | min(nknots) < 1) {
    stop("nknots must be a vector of positive integers")
  }
  if (!is.numeric(poverlap) | length(poverlap) != 1 | poverlap < 1) {
    stop("poverlap must be at least 1")
  }
  if (length(k) == 1) {
    k = rep(k, length(nknots))
  } else if (length(k) != length(nknots)) {
    stop("k must have length 1 or length(nknots)")
  }
  g = vector("list", length(nknots))
  for (i in seq_along(g)) {
    g[[i]] = sp::spsample(eborder, n = nknots[i], type = type, ...)
    nknots[i] = nrow(sp::coordinates(g[[i]]))
  }

  out = list(grid = g, eborder = eborder,
             border = border,
             width = width,
             poverlap = poverlap,
             k = k,
             nknots = nknots,
             nbasis = sum(nknots),
             longlat = longlat)
  class(out) = "hero_radspline"
  return(out)
}

