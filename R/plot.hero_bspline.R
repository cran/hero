#' Plot a \code{hero_bspline} object
#'
#' Plots basis functions specified by results of \code{\link{bspline}}.
#'
#' @param x An object of class \code{hero_bspline} to be plotted.
#' @param kcol Color for vertical lines drawn at interior knots.  Default is \code{NULL}, meaning no lines are drawn.
#' @param nderiv An integer value specifying the derivative order of the B-splines.  The default is 0.
#' @inheritParams bspline
#' @inheritParams graphics::matplot
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{matplot}} function.
#' @method plot hero_bspline
#' @export
#' @seealso \code{\link{bspline}}
#' @examples
#' x = bspline(nbasis = 10, extend = FALSE)
#' plot(x)
#' plot(x, nderiv = 1)
#' plot(x, kcol = "grey") # plot vertical lines at knots
#'
#' # extend knots passed rangeval
#' x2 = bspline(nbasis = 10, extend = TRUE)
#' plot(x2, kcol = "grey")
#'
#' # compare to plot.fd
#' x3 = fda::create.bspline.basis(nbasis = 10)
#' par(mfrow = c(2, 1))
#' plot(x, kcol = "grey")
#' title("plot.hero_bspline")
#' plot(x3)
#' title("plot.fd")
plot.hero_bspline = function(x, nderiv = 0L, type = "l", kcol = NULL, ...) {
  if (class(x) != "hero_bspline") {
    stop("x should be a hero_bspline object")
  }
  if (length(nderiv) != 1 |
      !is.numeric(nderiv) |
      nderiv < 0) {
    stop("nderiv should be an non-negative integer")
  }
  s = seq(min(x$rangeval), max(x$rangeval), len = 1000)
  y = splines::splineDesign(knots = x$knots, x = s,
                            ord = x$norder, derivs = nderiv,
                            outer.ok = TRUE)
  tk = length(x$knots)
  v = x$knots[(x$norder + 1):(tk - x$norder)]
  x = s # for labeling purposes
  graphics::matplot(x = x, y = y, type = "l", ...)
  graphics::abline(v = v, col = kcol)
}
