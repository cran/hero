#' Generate 2d data
#'
#' Generate two-dimensional data related to the f1 function
#' of Lu et al. (2012) (code from author).  Define \code{n =
#' c(60, 80)}.  Then \code{x[[i]] = (1:n[i])/n[i] -
#' 1/2/n[i]}.  These are the observed data locations. For
#' \code{i} and \code{j} spanning the full length of each
#' element of \code{x}, \code{mu2d[i, j] = sin(2 * pi *
#' (x[[1]][i] - .5) ^ 3) * cos(4 * pi * x[[2]][j])}.  Lastly,
#' \code{data2d = mu2d + rnorm(prod(n))}.
#' @return A list with components \code{x}, \code{mu2d}, and
#'   \code{data2d}.  \code{x} is a list of sequences with
#'   length 60 and 80.  \code{mu2d} and \code{data2d} are
#'   matrices of size 60 by 80.
#' @author Joshua French.  Based off code by Luo Xiao (see
#'   References).
#' @references Xiao, L. , Li, Y. and Ruppert, D. (2013),
#'   Fast bivariate P-splines: the sandwich smoother. J. R.
#'   Stat. Soc. B, 75: 577-599. <doi:10.1111/rssb.12007>
#' @export
#' @examples
#' dat = generate.data2d()
generate.data2d = function() {
  n = c(60, 80)

  x = vector("list", 2)
  x[[1]] = seq_len(n[1])/n[1] - 1/2/n[1]
  x[[2]] = seq_len(n[2])/n[2] - 1/2/n[2]

  # construct "true" data
  mu2d = matrix(0, nrow = n[1], ncol = n[2])
  for (i in seq_len(n[1])) {
    for (j in seq_len(n[2])) {
      mu2d[i, j] =  sin(2 * pi * (x[[1]][i] - .5) ^ 3) * cos(4 * pi * x[[2]][j])
    }
  }

  # add noise to "true" data
  data2d = mu2d + stats::rnorm(prod(n))
  return(list(x = x, mu2d = mu2d, data2d = data2d))
}

#'@rdname generate.data2d
#'@export
generate_data2d = generate.data2d

#'@rdname generate.data2d
#'@export
generateData2d = generate.data2d

#'@rdname generate.data2d
#'@export
GenerateData2d = generate.data2d


