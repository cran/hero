#' Circulate values of a vector
#'
#' The first \code{n} values of \code{x} are circulated
#' from the front of \code{x} to the back of \code{x}.
#'
#' @param x vector of values
#' @param n The number of values to circulate
#'
#' @return The circulated vector
#' @export
#' @author Joshua French
#' @examples
#' circulate(1:10, n = 2)
#' circulate(as.list(1:10), n = 2)
circulate = function(x, n = 1) {
  if (!is.vector(x)) stop("x must be a vector")
  if (length(n) != 1 | !is.numeric(n) | n < 1) {
    stop("n should a positive integer")
  }
  c(x[-seq_len(n)], x[seq_len(n)])
}
