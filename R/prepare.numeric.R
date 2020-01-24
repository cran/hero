#' Prepare data vector for sandwich smooth
#'
#' \code{prepare.vector} prepares a data vector for the
#' sandwich smooth.  Unlike the other \code{prepare.*}
#' functions, \code{x} and \code{splines} do not
#' need to be lists since the data are 1-dimensional.
#'
#' If \code{x} is not supplied and \code{n} is
#' the \code{length(data)}, then the function automatically
#' sets \code{x = seq(0, 1, length = n)}.
#'
#' If \code{splines} is not supplied, and \code{n} is the
#' \code{length(data)}, then the function automatically sets
#' \code{splines = bspline(range(x), nknots = min(ceiling(n/4), 35))}.
#'
#' @param data A numeric data vector
#' @param x A sequence of equidistant values
#'   corresponding to where the data
#'   are observed.  Equidistant spacing between 0 and 1 is
#'   assumed if not supplied.  See Details.
#' @param splines A spline-related object, e.g.,
#'   produced by \code{\link{bspline}}.  A spline is
#'   automatically created if not supplied.  See Details.
#' @param ... Not currently implemented.
#' @inheritParams diffpen
#' @return A \code{prepared_numeric} object.
#' @export
#' @rdname prepare.numeric
#' @seealso \code{\link{bspline}},
#'   \code{\link{default.evalargs}},
#'   \code{\link{default.splines}}
#' @author Joshua French.  Based off code by Luo Xiao (see
#'   References).
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#'
#'   Ruppert, D., Wand, M. P., & Carroll, R. J. (2003).
#'   Semiparametric Regression. Cambridge University Press.
#'   <doi:10.1017/CBO9780511755453>
#' @examples
#' # create data
#' n = 160
#' x = seq(0, 4 * pi, len = n)
#' # "true" data
#' mu = sin(x)
#' # plot true data
#' plot(x, mu, type = "l")
#' # construct noisy data
#' set.seed(4)
#' data = mu + rnorm(n)
#'
#' # construct spline
#' splines = bspline(c(0, 4 * pi), nknots = 20)
#' # prepare/enhance data
#' obj = prepare(data, x, splines)
#' obj = enhance(obj)
#' sandmod = hero(obj)
#' plot(sandmod, ylim = range(data), lty = 2)
#' lines(x, data, col = "lightgrey")
#' lines(x, mu)
#' legend("bottomleft",
#'        legend = c("smoothed", "true", "observed"),
#'        lty = c(2, 1, 1),
#'        col = c("black", "black", "grey"))
prepare.numeric = function(data, x, splines,
                           m = 2, sparse = TRUE, ...) {
  if (!is.vector(data)) stop("data should be a numeric vector")
  n = length(data)
  if (missing(x)) {
    x = seq(0, 1, length = n)
  }
  if (length(data) != length(x)) {
    stop("data and x should have the same length")
  }
  if (missing(splines)) {
    splines = bspline(range(x),
                      nknots = min(ceiling(n/4), 35))
  }

  assembled = vector("list", 1)
  assembled[[1]] =  assemble(object = splines, x = x,
                           m = m, sparse = sparse)

  Ytilde = Matrix::crossprod(assembled[[1]]$A, data)
  structure(list(
    Ytilde = Ytilde,
    sum_ysq = sum(data^2),
    n = length(data),
    s = lapply(assembled, getElement, name = "s"),
    B = lapply(assembled, getElement, name = "B"),
    Q = lapply(assembled, getElement, name = "Q"),
    A = lapply(assembled, getElement, name = "A"),
    U = lapply(assembled, getElement, name = "U"),
    loglambda = 0,
    x = x
  ), class = "prepared_numeric")
}
