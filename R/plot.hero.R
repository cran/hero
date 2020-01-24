#' Plot a \code{hero} object
#'
#' Plot the smoothed data produced by the
#' \code{\link{hero}}
#' function.  The behavior of the function changes depending
#' on the subclass of the \code{hero} object.  See Details.
#'
#' If \code{x} has subclass \code{hero_numeric}, then
#' the traditional \code{\link[graphics]{plot}} function
#' is used to plot the smoothed data, with \code{type = "l"}.
#'
#' If \code{x} has subclass \code{hero_matrix}, then
#' \code{\link[graphics]{image}} is used to plot the
#' smoothed data, or if the autoimage package is installed,
#' \code{\link[autoimage]{autoimage}} is used to
#' plot the smoothed data.
#'
#' @param x An object of class \code{hero}.
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param ... Additional graphical parameters passed to the
#' relevant plotting function.  See Details.
#' @method plot hero_matrix
#' @rdname plot.hero
#' @export
#' @seealso \code{\link{hero}}
#' @examples
#' # create b-splines
#' x1 = bspline(nbasis = 10)
#' x2 = bspline(nbasis = 12)
#'
#' # observed data locations
#' evalarg1 = seq(0, 1, len = 60)
#' evalarg2 = seq(0, 1, len = 80)
#'
#' # construct "true" data
#' mu = matrix(0, nrow = 60, ncol = 80)
#' for(i in seq_len(60)) {
#'    for(j in seq_len(80)) {
#'       mu[i, j] =  sin(2*pi*(evalarg1[i]-.5)^3)*cos(4*pi*evalarg2[j])
#'    }
#' }
#' # construct noisy data
#' data = mu + rnorm(60 * 80)
#'
#' obj = prepare(data, list(evalarg1, evalarg2), list(x1, x2))
#' obj = enhance(obj)
#' sandmod = hero(obj)
#' plot(sandmod)
plot.hero_matrix = function(x, xlab = "", ylab = "", ...) {
  if (!requireNamespace("autoimage", quietly = TRUE)) {
      message("Package \"autoimage\" will enhance this function. Please install it.",
           call. = FALSE)
    graphics::image(x$x[[1]], x$x[[2]],
                    x$fitted, xlab = xlab, ylab = ylab,
                    ...)
  } else {
    if (length(x$x) == 2 & x$sts == FALSE) {
      autoimage::autoimage(x$x[[1]], x$x[[2]],
                           x$fitted, xlab = xlab, ylab = ylab,
                           ...)
    } else {
      message("plot.hero_matrix not supported for x$sts == TRUE")
    }
  }
}

