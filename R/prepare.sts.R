#' Prepare \code{starray} for sandwich smooth
#'
#' \code{prepare.starray} prepares a spatio-temporal
#' array for the sandwich smooth.
#'
#' @param data An \code{\link{starray}}
#' @inheritParams border.grid
#' @param times The vector of times at which the data were observed.
#' @param rs A \code{hero_radspline} produced by the \code{\link{radspline}} or
#' \code{\link{connect}} functions.
#' @param bs A \code{hero_bspline} produced by the \code{\link{bspline}} function.
#' @inheritParams assemble.hero_radspline
#' @param ... Not currently implemented.
#' @inheritParams diffpen
#' @inheritParams spdiffpen
#' @inheritParams enlarge
#' @return A \code{prepared_sts} object.
#' @export
#' @rdname prepare.sts
#' @seealso \code{\link{bspline}}, \code{\link{radspline}}
#' @author Joshua French.  Based off code by Luo Xiao (see
#'   References).
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#' @examples
#' # construct basis functions
#' border = border.grid(lon, lat)
#' rs = radspline(nknots = 36, poverlap = 3,
#'                border = border, longlat = TRUE)
#' bs = bspline(c(1, 30), nbasis = 6)
#' splines = list(rs, bs)
#' data = as.sts(tasmax)
#' p = prepare(data, coords = cbind(c(lon), c(lat)),
#'             times = 1:30, rs = rs, bs = bs)
prepare.sts = function(data, coords, times,
                           rs, bs, m = 2,
                           sparse = TRUE,
                           spdiffpen = TRUE,
                           ...) {
  arg_check_prepare_sts(coords, times, rs, bs, m, sparse)
  assembled = vector("list", 2)

  assembled[[1]] = assemble(object = rs,
                          x = as.matrix(coords),
                          m = m, sparse = sparse,
                          longlat = rs$longlat,
                          spdiffpen = spdiffpen)
  assembled[[2]] = assemble(object = bs, x = times,
                          m = m, sparse = sparse)

  Ytilde = Matrix::crossprod(assembled[[1]]$A, data$data) %*% assembled[[2]]$A
  out = list(
    Ytilde = Ytilde,
    sum_ysq = sum(data$data^2),
    n = dim(data$data),
    s = lapply(assembled, getElement, name = "s"),
    B = lapply(assembled, getElement, name = "B"),
    Q = lapply(assembled, getElement, name = "Q"),
    A = lapply(assembled, getElement, name = "A"),
    U = lapply(assembled, getElement, name = "U"),
    loglambda = rep(0, 2),
    times = times
  )
  class(out) = "prepared_sts"
  return(out)
}

arg_check_prepare_sts = function(coords, times,
                                 rs, bs, m, sparse) {
  if (is.null(dim(coords))) {
    stop("coords must have dimensions")
  }
  if (length(dim(coords)) != 2) {
    stop("coords must be two-dimensional")
  }
  if (!is.numeric(times) | !is.vector(times)) {
    stop("times must be a numeric vector")
  }
  if (class(rs) != "hero_radspline") {
    stop("rs is not a hero_radspline")
  }
  if (class(bs) != "hero_bspline") {
    stop("bs is not a hero_bspline")
  }
  if (length(m) != 1 | !is.numeric(m) | m < 1) {
    stop("m must be a positive integer")
  }
  if (length(sparse) != 1 | !is.logical(sparse)) {
    stop("sparse must be a logical value")
  }
}
