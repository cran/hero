#' Rotated H-transform
#'
#' A rotation of the H-transform of the array \code{a} by a
#' matrix \code{x}.
#'
#' \code{x} should be matrix-like.  This function has been
#' tested when \code{x} is a \code{matrix} object or a
#' \code{\link[Matrix]{Matrix}}.
#'
#' Assuming \code{a} is of size \eqn{c_1 \times c_2 \times
#' \dots \times c_d}, then \code{x} is of size \eqn{r \times
#' c_1}.
#'
#' @param x A matrix-like object.  See Details.
#' @param a An d-dimensional array
#' @param transpose A logical value.  The Default is
#'   \code{FALSE}.  If \code{TRUE}, then the transpose of
#'   \code{A}
#'
#' @return A rotated, h-transformed array
#' @export
#' @author Joshua French.  Based off code by Luo Xiao (see
#'   References).
#' @references Currie, I. D., Durban, M. and Eilers, P. H.
#' (2006), Generalized linear array models with applications
#' to multidimensional smoothing. Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology),
#' 68: 259-280. <doi:10.1111/j.1467-9868.2006.00543.x>
#'
#' Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#' @examples
#' dim = c(10:12)
#' # construct random array
#' a = array(rnorm(prod(dim)), dim = dim)
#' # construct random matrix
#' x = matrix(rnorm(15 * dim[1]), nrow = 15)
#' rhxa = rh(x, a)
rh = function(x, a, transpose = FALSE) {
  dimx = dim(x)
  if (is.null(dimx)) {
    stop("x must be matrix-like (with dimensions)")
  }
  if (length(dimx) != 2) {
    stop("x should only have two dimensions.")
  }

  d = dim(a)
  if (is.null(d)) {
    stop("dim(x) is NULL.  x should be matrix-like.")
  }

  # h-transformation
  m = matrix(a, nrow = d[1])
  if (!transpose) {
    h = array(x %*% m, c(nrow(x), d[-1]))
  } else {
    h = array(Matrix::crossprod(x, m), c(ncol(x), d[-1]))
  }

  # rotation
  aperm(h, circulate(seq_along(d)))
}
