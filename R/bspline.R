#' B-spline specification
#'
#' \code{bspline} helps define the parameters necessary for
#' constructing a B-spline but doesn't evaluate it.
#'
#' The knots are assumed to be equidistant and non-repeating
#' (except possibly at the endpoints).
#'
#' The number of knots (\code{nknots}) and the number of
#' basis function (\code{nbasis}) are linked by the relation
#' \code{nknots = nbasis - norder}.
#'
#' If \code{extend = TRUE},  the interior knots are
#' augmented by replicating the \code{rangeval} endpoints
#' \code{norder} times.  Otherwise, the interior knots are
#' augmented by \code{norder} knots at each end
#' corresponding to the spacing of the interior knots.
#'
#' The knot placement mimics the behavior of
#' \code{\link[fda]{create.bspline.basis}} when \code{extend
#' = FALSE}. Note that the number of breaks specified by
#' \code{breaks} in \code{\link[fda]{create.bspline.basis}}
#' corresponds to the number of interior knots plus 2 (the
#' interior knots plus the two endpoints).
#'
#' If \code{knots} is specified, the function does minimial
#' argument checking.  This is provided (mostly) for
#' testing purposes, though it can be used by individuals
#' who want more customizability of knots locations than
#' the equidistant spacing provided by default.
#' @inheritParams knot.design
#' @param knots A numeric vector with all knots (interior
#'   and exterior), including potentially replicated
#'   endpoints.  See Details.
#' @return An object of class \code{hero_bspline}.  It is a
#'   list specifying the necessary B-spline parameters.
#' @seealso \code{\link{knot.design}}
#' @export
#' @author Joshua French
#' @examples
#' bspline(nbasis = 10)
bspline = function(rangeval = 0:1, nbasis, nknots, norder = 4, extend = FALSE, knots) {
  if (missing(knots)) {
    if (missing(nbasis) & missing(nknots)) {
      stop("nbasis or nknots must be specified")
    }
    if (!missing(nbasis) & !missing(nknots)) {
      nbasis2 = nknots + norder
      if (nbasis != nbasis2) stop("nbasis != nknots + norder")
    }
    if (!missing(nbasis)) {
      nknots = nbasis - norder
    }
    if (missing(nbasis)) {
      nbasis = nknots + norder
    }
    arg_check_knot_design(nknots, norder, extend, FALSE)

    knots = knot.design(rangeval = rangeval, nknots = nknots,
                        norder = norder, extend = extend,
                        interior = FALSE)
  } else {
    if (!is.numeric(knots)) {
      stop("knots must be a numeric vector")
    }
    tk = length(knots)
    rangeval = knots[c(norder, tk - norder + 1)]
    nknots = tk - 2 * norder
    nbasis = nknots + norder
    extend = (min(knots) < min(rangeval) |
                max(knots) > max(rangeval))
    arg_check_knot_design(nknots, norder, extend, FALSE)
  }
  obj = list(knots = knots, nbasis = nbasis,
             nknots = nknots, norder = norder,
             rangeval = rangeval)
  class(obj) = "hero_bspline"
  return(obj)
}
