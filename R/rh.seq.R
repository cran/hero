#' Apply \code{rh} sequentially
#'
#' \code{rh.seq} sequentially applies the \code{\link{rh}}
#' function to \code{a}.  Specifically, if the length of
#' \code{x} is \code{d}, then \code{rh.seq(x, a)} is
#' equivalent to \code{rh(x[[d]], rh(x[[d - 1]], ..., rh(x[[2]], rh(x[[1]], a))..))}.
#'
#' @param x A list of matrix-like objects
#' @param a A matrix-like object (with dimensions)
#' @inheritParams rh
#' @import Matrix
#' @importMethodsFrom Matrix dim
#' @return A \code{matrix} or \code{\link[Matrix]{Matrix-class}}.
#' @export
#'
#' @examples
#' # generate x, a
#' x = list(matrix(rnorm(100), nrow = 10),
#'          matrix(rnorm(100), nrow = 10))
#' a = matrix(rnorm(100), nrow = 10)
#'
#' # three equivalent forms
#' rhs1 = rh.seq(x, a)
#' rhs2 = rh(x[[2]], rh(x[[1]], a))
#' rhs3 = x[[1]] %*% a %*% t(x[[2]])
#'
#' # check equality
#' all.equal(rhs1, rhs2)
#' all.equal(rhs1, rhs3)
rh.seq = function(x, a, transpose = FALSE) {
  if (!is.list(x)) {
    stop("x should be a list")
  }
  if (is.null(dim(a))) stop("dim(a) is NULL.  a should be matrix-like.")

  for (i in seq_along(x)) {
    a <- rh(x[[i]], a, transpose = transpose)
  }
  return(a)
}

#' @rdname rh.seq
#' @export
rh_seq = rh.seq

#' @rdname rh.seq
#' @export
rhSeq = rh.seq

#' @rdname rh.seq
#' @export
RhSeq = rh.seq

