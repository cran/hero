#' Prepare data array for sandwich smooth
#'
#' \code{prepare.array} prepares a data matrix for the
#' sandwich smooth.  The dimensionality of \code{data} and
#' the length of \code{x} must match.  Specifically,
#' \code{length(dim(data))} must equal
#' \code{length(x)}.     The dimensionality of
#' \code{data} and  the length of \code{splines} must match.
#' Specifically, \code{length(dim(data))} must equal
#' \code{length(splines)}.
#'
#' For a typical sandwich smooth, for data with \eqn{d}
#' dimensions, \code{Y[i1, i2, ...,id]} is assumed to be
#' observed at position \code{x[[1]][i1]},
#' \code{x[[2]][i2]}, ..., \code{x[[d]][id]}.
#' Consequently, \code{dim(data)[i]} should equal
#' \code{length(x[[i]])} for all \code{i} in
#' \code{seq_len(d)}.
#'
#' If \code{x} is not supplied, then
#' \code{\link{default.evalargs}} is used to create it
#' automatically.
#'
#' If \code{splines} is not supplied, then a B-spline basis
#' is automatically created for each dimension using
#' \code{\link{default.splines}}.
#'
#' @param data A data array
#' @param x A list of univariate, equidistant
#'   sequences.  These should correspond to where the data
#'   are observed.  Equidistant spacing between 0 and 1 is
#'   assumed if not supplied.  See Details.
#' @param splines A list of spline-related objects, e.g.,
#'   produced by \code{\link{bspline}}.  Splines are
#'   automatically created if not supplied.  See Details.
#' @param ... Not currently implemented.
#' @inheritParams diffpen
#' @return A \code{prepared_array} object.
#' @export
#' @rdname prepare.array
#' @seealso \code{\link{bspline}},
#'   \code{\link{default.evalargs}},
#'   \code{\link{default.splines}}
#' @author Joshua French.  Based off code by Luo Xiao (see
#'   References).
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#' @examples
#' # generate and prepare 3d data
#' set.seed(9)
#' dat = generate.data3d()
#' obj = prepare(dat$data3d, x = dat$x)
prepare.array = function(data, x, splines,
                         m = 2, sparse = TRUE, ...) {
  arglist = list(...)
  arg_check_data(data)
  if (missing(x)) {
    evalargs = default.evalargs(data)
  }
  arg_check_data_x(data, x)

  if (missing(splines)) {
    splines = default.splines(x)
  }
  arg_check_x_splines(x, splines)

  d = length(x)
  if (!is.null(arglist$assembled)) {
    assembled = arglist$assembled
  } else {
    assembled = assemble(splines, x = x, m = m,
                       sparse = sparse, ...)
  }

  A = lapply(assembled, getElement, name = "A")
  Ytilde = rh.seq(A, data, transpose = TRUE)
  structure(list(
    Ytilde = Ytilde,
    sum_ysq = sum(data^2),
    n = dim(data),
    s = lapply(assembled, getElement, name = "s"),
    B = lapply(assembled, getElement, name = "B"),
    Q = lapply(assembled, getElement, name = "Q"),
    A = lapply(assembled, getElement, name = "A"),
    U = lapply(assembled, getElement, name = "U"),
    loglambda = rep(0, d),
    x = x
  ), class = "prepared_array")
}
