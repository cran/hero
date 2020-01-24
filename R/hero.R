#' Construct a hero sandwich smoother
#'
#' \code{hero} constructs a hero sandwich smoother based off
#' off a prepared data object coming from the
#' \code{\link{prepare}} function.  \cr
#' Subclasses are added (e.g., \code{hero_numeric},
#' \code{hero_matrix}, \code{hero_array}, etc.) are added to
#' the returned object for plotting purposes.
#' \cr
#' A list is returned (and the data locations are not) for
#' \code{hero.prepared_list}.  Each element of the list
#' contains the coefficients and fitted values (if \code{fitted} is
#' TRUE) for the respective data observation.
#'
#' @param x Data prepared via the \code{\link{prepare}}
#'   function.
#' @param ... Mostly not implemented.  \code{hero.prepared_list} takes
#' the \code{fitted} argument, specifying whether the \code{fitted}
#' values should be returned.
#'
#' @return A \code{hero} object with the smoothed data
#'   (\code{fitted}), the estimated coefficients for the
#'   basis functions (\code{coefficients}), and the
#'   locations of the original data (\code{x}).
#' @author Joshua French.
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
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
hero = function(x, ...) {
  UseMethod("hero", x)
}
