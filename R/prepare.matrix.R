#' Prepare data matrix for sandwich smooth
#'
#' \code{prepare.matrix} prepares a data matrix for the
#' sandwich smooth.  The dimensionality of \code{data} and
#' the length of \code{x} must match.  Specifically,
#' \code{length(dim(data))} must equal
#' \code{length(x)}.     The dimensionality of
#' \code{data} and the length of \code{splines} must match.
#' Specifically, \code{length(dim(data))} must equal
#' \code{length(splines)}.
#'
#' For a typical sandwich smooth (\code{sts = FALSE}),
#' for two-dimensional data, \code{data[i, j]} is assumed
#' to be observed at position \code{x[[1]][i]},
#' \code{x[[2]][j]}.  If the data are a spatial time series,
#' then the first dimension is assumed to refer to space,
#' and the second dimension to time.  In that case,
#' \code{data[i, j]} is assumed
#' to be observed at location \code{x[[1]][i, ]} and time
#' \code{x[[2]][j]}.
#'
#' If \code{sts = TRUE}, then \code{x[[1]]} should be a
#' matrix of spatial coordinates, with each row
#' corresponding to a location, and \code{x[[2]]} should
#' be a vector with the observation times.
#'
#' If \code{x} is not supplied, then
#' \code{\link{default.evalargs}} is used to create it
#' automatically.  This is only valid when
#' \code{sts = FALSE}.
#'
#' If \code{splines} is not supplied, then a B-spline basis
#' is automatically created for each dimension using
#' \code{\link{default.splines}}.  This is only valid when
#' \code{sts = FALSE}.
#'
#' @param data A data matrix.
#' @param x A list of values at which to evaluate the basis
#' functions.  See Examples and Details.
#' @param splines A list of spline objects
#' (\code{hero_bspline} and \code{hero_radspline}).  See
#' Examples and Details.
#' @param sts A logical value indicating whether \code{data}
#' is a spatial time series, in which each row of
#' \code{data} corresponds to a distinct spatial location
#' and each column corresponds to a distinct time.
#' @param ... Not currently implemented.
#' @inheritParams diffpen
#' @inheritParams assemble
#' @return A \code{prepared_matrix} object.
#' @rdname prepare.matrix
#' @export
#' @seealso \code{\link{bspline}}, \code{\link{radspline}},
#' \code{\link{diffpen}}, \code{\link{spdiffpen}},
#'   \code{\link{default.evalargs}},
#'   \code{\link{default.splines}}
#' @author Joshua French.  Based off code by Luo Xiao (see
#'   References).
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#' @examples
#' # prepare Lu et al. (2012) noisy f1 data
#' data(ludata)
#' obj = prepare(lunoisyf1, x = list(x, z))
#' h = hero(obj)
#'
#' # precompute some stuff
#' splines = default.splines(list(x, z))
#' l = assemble(splines, x = list(x, z))
#' obj2 = prepare(lunoisyf1, x = list(x, z),
#'                splines = splines, assembled = l)
#' h2 = hero(obj2)
#' all.equal(h, h2)
prepare.matrix = function(data, x, splines, m = 2,
                         sparse = TRUE,
                         spdiffpen = TRUE, digits = 1,
                         sts = FALSE, ...) {
  arglist = list(...)
  arg_check_data(data)
  if (missing(x) & !sts) {
    x = default.evalargs(data)
  } else if (missing(x) & sts) {
    stop("x must be supplied when sts = TRUE")
  }
  arg_check_data_x(data, x)

  if (missing(splines) & !sts) {
    splines = default.splines(x)
  } else if (missing(x) & sts) {
    stop("splines must be supplied when sts = TRUE")
  }
  arg_check_x_splines(x, splines)
  if (length(m) == 1) {
    m = rep(m, length(x))
  }
  if (length(m) != length(x)) {
    stop("m must have length 1 or length(x)")
  }
  if (min(m) < 1) {
    stop("m must >= 1")
  }

  d = length(x)
  if (!is.null(arglist$assembled)) {
    assembled = arglist$assembled
  } else {
    assembled = assemble(splines, x = x, m = m,
                       sparse = sparse,
                       spdiffpen = spdiffpen,
                       digits = digits, ...)
  }

  Ytilde = Matrix::crossprod(assembled[[1]]$A, data) %*% assembled[[2]]$A
  structure(list(
    Ytilde = Ytilde,
    sum_ysq = sum(data ^ 2),
    n = dim(data),
    s = lapply(assembled, getElement, name = "s"),
    B = lapply(assembled, getElement, name = "B"),
    A = lapply(assembled, getElement, name = "A"),
    Q = lapply(assembled, getElement, name = "Q"),
    U = lapply(assembled, getElement, name = "U"),
    loglambda = rep(0, d),
    x = x,
    sts = sts), class = "prepared_matrix")
}

arg_check_data = function(data) {
  if (!is.matrix(data) & !is.array(data)) {
    stop("data must be a matrix or array")
  }
  if (nrow(data) < 2 | ncol(data) < 2) {
    stop("data should have more than 1 row or column")
  }
}

arg_check_data_x = function(data, x) {
  if (!is.list(x)) {
    stop("x must be a list describing the observed data positions")
  }
  dimd = dim(data)

  lend = length(dimd)
  lenx = length(x)
  if (lend != lenx) {
    stop("number of data dimensions doesn't match length(x)")
  }
  dimx = sapply(x, length)
  for (i in seq_along(x)) {
    if (!is.null(dim(x[[i]]))) {
      dimx[i] = nrow(x[[i]])
    }
  }
  if (!isTRUE(all.equal(dimd, dimx, check.attributes = FALSE))) {
    stop("dim(data)[i] != length(x[[i]]) for all i")
  }
}

arg_check_x_splines = function(x, splines) {
  if (!is.list(splines)) {
    stop("splines must be a list of spline-related objects")
  }
  lenx = length(x)
  lens = length(splines)

  dimx = sapply(x, length)

  if (lenx != lens) {
    stop("length(x) != length(splines)")
  }
}
