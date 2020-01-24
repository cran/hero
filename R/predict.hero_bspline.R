#' Predict method for \code{hero_bspline} object
#'
#' Predicted values based on object created by
#' \code{\link{bspline}}.
#' @param object A \code{hero_bspline} object created by
#'   \code{\link{bspline}}
#' @param newx A numeric vector of values at which to
#' evaluate the B-spline functions or derivatives.
#' @param sparse A logical value indicating if the result
#'   should be a sparse version of the
#'   \code{\link[Matrix]{Matrix-class}}.
#'
#' @inheritParams plot.hero_bspline
#' @param ... Not currently implemented.
#' @importFrom stats predict
#' @method predict hero_bspline
#' @return An \eqn{n \times k} matrix (or
#'   \code{\link[Matrix]{Matrix-class}} object if
#'   \code{sparse = TRUE}), where \eqn{n} is the number of
#'   values in \code{newx} and \eqn{k} is the number of
#'   basis functions in \code{object}.  Each row gives the
#'   predicted values of the basis functions for the
#'   appropriate value of \code{newx}.
#' @export
#' @seealso \code{\link{bspline}}
#' @examples
#' b = bspline(nbasis = 10)
#' p = predict(b, newx = seq(0, 1, len = 101))
predict.hero_bspline = function(object, newx, nderiv = 0L, sparse = TRUE, ...) {
    splines::splineDesign(knots = object$knots,
                          x = newx,
                          ord = object$norder,
                          derivs = nderiv,
                          outer.ok = TRUE,
                          sparse = sparse)
}


