#' P-spline difference penalty
#'
#' @param x A \code{hero_bspline} object produced by \code{\link{bspline}}.
#' @param m A positive integer indicating order of the difference penalty.
#' @inheritParams predict.hero_bspline
#'
#' @return A \code{\link[base]{matrix}} or \code{\link[Matrix]{sparseMatrix-class}} object.
#' @export
#' @author Joshua French
#' @examples
#' b = bspline(nbasis = 10)
#' diffpen(b)
diffpen <- function(x, m = 2, sparse = TRUE) {
  if (is.null(x$nbasis)) {
    stop("x$nbasis cannot be NULL")
  }
  arg_check_m(m)
  arg_check_sparse(sparse)
  dpen = base::diff(diag(x$nbasis), differences = m)
  if (sparse) {
    return(Matrix::Matrix(dpen, sparse = sparse))
  } else {
    return(dpen)
  }
}

arg_check_m = function(m) {
  if (length(m) != 1 | m < 1) {
    stop("m should be a positive integer")
  }
}

arg_check_sparse = function(sparse) {
  if (length(sparse) != 1 | !is.logical(sparse)) {
    stop("sparse should be a logical value")
  }
}
