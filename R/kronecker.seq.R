#' A sequence of kronecker products
#'
#' @param X A list of numeric matrices or arrays
#' @inheritParams base::kronecker
#'
#' @return A matrix or array
#' @export
#'
#' @examples
#' x1 = matrix(rnorm(16), nrow = 4)
#' x2 = matrix(rnorm(25), nrow = 5)
#' x3 = matrix(rnorm(36), nrow = 6)
#' x4 = matrix(rnorm(49), nrow = 7)
#' p1 = x1 %x% x2 %x% x3 %x% x4
#' p2 = kronecker.seq(list(x1, x2, x3, x4))
#' all.equal(p1, p2)
kronecker.seq = function(X, FUN = "*", make.dimnames = FALSE, ...) {
  if (!is.list(X)) {
    if (is.null(dim(X))) {
      stop("X must be matrix-like with non-NULL dimensions")
    }
    return(kronecker(X = X, Y = 1, FUN = FUN, make.dimnames = make.dimnames, ...))
  } else {
    if (length(X) == 1) {
      return(kronecker(X = X[[1]], Y = 1, FUN = FUN, make.dimnames = make.dimnames, ...))
    } else {
      i = 1
      out = X[[1]]
      while (i < length(X)) {
        out = kronecker(out, X[[i + 1]])
        i = i + 1
      }
      return(out)
    }
  }
}

#'@rdname kronecker.seq
#'@export
kronecker_seq = kronecker.seq

#'@rdname kronecker.seq
#'@export
kroneckerSeq = kronecker.seq

#'@rdname kronecker.seq
#'@export
KroneckerSeq = kronecker.seq

