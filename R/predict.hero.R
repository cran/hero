#' Predict method for \code{hero} object
#'
#' Predict new values based on object produced by the
#' \code{\link{hero}} function.
#' @param object A \code{hero_bspline} object created by
#'   \code{\link{bspline}}
#' @param newB A vector or list containing the evaluated
#' basis functions for the observations for
#' which predictions are desired.
#' @param ... Not currently implemented.
#' @importFrom stats predict
#' @method predict hero
#' @return A matrix of the appropriate size
#' @export
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
#' newb1 = predict(x1, newx = seq(0, 1, len = 100))
#' newb2 = predict(x2, newx = seq(0, 1, len = 100))
#' newB = list(newb1, newb2)
#' p = predict(sandmod, newB = list(newb1, newb2))
predict.hero = function(object, newB, ...) {
  rh.seq(newB, object$coefficients)
}


