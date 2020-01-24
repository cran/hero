#' Assemble spline ingredients for sandwich smooth
#'
#' Assemble computations from a spline-related object
#' \code{x} in order to implement the sandwich smoother.
#' This is essentially an internal function, but could be
#' useful for performing a manual implementation of the
#' sandwich smoother.
#'
#' @param object A spline-related object (e.g, a
#'   \code{hero_bspline} from the \code{\link{bspline}}
#'   function), or a list of spline-related objects.
#' @param x Values at which to evaluate the basis functions.
#' This
#' should be a numeric vector if \code{object} is a
#' \code{hero_bspline}.  This should be a numeric matrix of
#' coordinates if \code{object} is a \code{hero_radspline}.
#' If \code{object} is a list comprised of
#' \code{hero_bspline} and \code{hero_radspline} objects,
#' then \code{x} should be a list where each element of the
#' list corresponds to the appropriate \code{x} argument
#' for each element.
#' @inheritParams diffpen
#' @param spdiffpen A logical value indicating whether
#'   \code{\link{spdiffpen}} should be used to compute the
#'   difference penalty.  The default is \code{FALSE}.
#' @inheritParams adjacent
#' @param ... Not implemented
#'
#' @return A list with the necessary components
#'   (ingredients)
#' @export
#' @examples
#' # construct b-spline
#' object1 = bspline(nbasis = 10)
#' # sequence to evaluate
#' x1 = seq(0, 1, len = 11)
#' # assemble b-spline information
#' spline1 = assemble(object1, x1)
#'
#' # assemble radial spline information
#' border = border.grid(lon, lat)
#' object2 = radspline(nknots = 16, border)
#' x2 = cbind(c(lon), c(lat))
#' spline2 = assemble(object2, x = x2)
#'
#' # assemble for list of splines
#' object = list(object1, object2)
#' x = list(x1, x2)
#' splines = assemble(object, x)
assemble = function(object, ...) {
  UseMethod("assemble", object)
}
