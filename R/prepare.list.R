#' Prepare data array for sandwich smooth
#'
#' \code{prepare.list} prepares a list of data for the
#' sandwich smooth.  The class of each element of
#' the list must be identical.
#' The dimensionality of \code{data[[i]]} and
#' the length of \code{x} must match.  Specifically,
#' \code{length(dim(data[[i]]))} must equal
#' \code{length(x)}.     The dimensionality of
#' \code{data[[i]]} and  the length of \code{splines} must match.
#' Specifically, \code{length(dim(data[[i]]))} must equal
#' \code{length(splines)}.      Note: If the splines
#' are preassembled, these can be passed using the argument
#' \code{assembled} so that this computation is not reperformed.
#'
#' This function applies the functions
#' \code{\link{prepare.numeric}},
#' \code{\link{prepare.matrix}}, and
#' \code{\link{prepare.array}} to each element of the list,
#' so relevant restrictions in the arguments may be
#' found there.
#'
#' @param data A list of \code{numeric}, \code{matrix}, or
#' \code{array} objects.
#' @inheritParams prepare.matrix
#' @inheritParams diffpen
#' @return A \code{prepared_list} object.
#' @export
#' @author Joshua French.
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#' @seealso \code{\link{prepare.numeric}}, \code{\link{prepare.matrix}},
#' \code{\link{prepare.array}}
#' @examples
#' # generate and prepare 3d data
#' set.seed(9)
#' dat = lapply(1:3, function (i) generate.data3d())
#' x = dat[[1]]$x
#' data = lapply(dat, getElement, name = "data3d")
#' obj = prepare(data, x = x)
#' h = hero(obj)
prepare.list = function(data, x, splines,
                         m = 2, sparse = TRUE, ...) {
  arglist = list(...)
  arg_check_data(data[[1]])
  if (missing(x)) {
    evalargs = default.evalargs(data[[1]])
  }
  arg_check_data_x(data[[1]], x)

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
  Ytilde = lapply(data, function(d) {
    rh.seq(A, d, transpose = TRUE)
  })
  structure(list(
    Ytilde = Ytilde,
    sum_ysq = lapply(data, function(x) sum(x^2)),
    n = dim(data[[1]]),
    s = lapply(assembled, getElement, name = "s"),
    B = lapply(assembled, getElement, name = "B"),
    Q = lapply(assembled, getElement, name = "Q"),
    A = lapply(assembled, getElement, name = "A"),
    U = lapply(assembled, getElement, name = "U"),
    loglambda = rep(0, d),
    x = x
  ), class = "prepared_list")
}
