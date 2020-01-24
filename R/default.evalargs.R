#' Construct default \code{evalargs}
#'
#' Create a default \code{evalargs} object based on
#' \code{data}.  This is just a list of sequences.
#' If \code{ni = dim(data)[i]}, then the sequence for
#' dimension i is \code{seq(0, 1, len = ni)}.
#'
#' @param data A matrix or array-like object
#'
#' @return A list of equidistance sequences between 0 and 1
#' @export
#' @author Joshua French
#' @examples
#' a = array(rnorm(10 * 11 * 12), dim = 10:12)
#' default.evalargs(a)
default.evalargs = function(data) {
  dimd = dim(data)
  if (is.null(dimd)) {
    stop("data must be a matrix-like object, i.e, dim(data) should be non-NULL.")
  }
  evalargs = vector("list", length(dimd))
  for (i in seq_along(evalargs)) {
    evalargs[[i]] = seq(0, 1, len = dimd[i])
  }
  evalargs
}

