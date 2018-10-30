#' Generate 3d data
#'
#' Generate data related to Section 7.2 of Lu et al. (2012)
#' (code from author).  Define \code{n = c(128,
#'   128, 24)}.  Then \code{x[[i]] = (1:n[i])/n[i] -
#'   1/2/n[i]}.  These are the observed data locations. For
#'   \code{i}, \code{j}, \code{k} spanning the full length
#'   of each element of \code{x}, \code{mu3d[i, j, k] =
#'   x[[1]][i]^2 + x[[2]][j]^2 + x[[3]][k]^2}.  Lastly,
#'   \code{data3d = mu3d + 0.5 * rnorm(n[1] * n[2] * n[3])}.
#' @return A list with components \code{x}, \code{mu3d}, and
#' \code{data3d}.  \code{x} is a list of sequences with
#'   length 128, 128, and 24.  \code{mu3d} and \code{data3d}
#'   are arrays of size 128 by 128 by 24.
#' @author Joshua French.  Based off code by Luo Xiao (see
#'   References).
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#' @export
#' @examples
#' dat = generate.data3d()
generate.data3d = function() {
  d = 3 ### 3D subjects
  n = c(128, 128, 24) ### dimension of data
  x = vector("list", d) ### covariates
  for (i in seq_along(x)) {
    x[[i]] = (1:n[i]) / n[i] - 1 / 2 / n[i]
  }

  MY = array(0, dim = n) ### true function
  for (i in 1:n[1]) {
    for (j in 1:n[2]) {
      for (k in 1:n[3]) {
        MY[i, j, k] <- x[[1]][i] ^ 2 + x[[2]][j] ^ 2 + x[[3]][k] ^ 2
      }
    }
  }
  Y = MY + 0.5 * stats::rnorm(n[1] * n[2] * n[3])

  mu3d = MY
  data3d = Y
  return(list(x = x, mu3d = mu3d, data3d = data3d))
}

#'@rdname generate.data3d
#'@export
generate_data3d = generate.data3d

#'@rdname generate.data3d
#'@export
generateData3d = generate.data3d

#'@rdname generate.data3d
#'@export
GenerateData3d = generate.data3d


