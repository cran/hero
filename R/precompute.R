#' Precompute objects
#'
#' This function is an internal function to compute
#' objects needed for fast implementation of the sandwich
#' smoother.  It is meant to be an internal function,
#' so use this at your own risk.
#'
#' @param B A matrix of basis functions
#' @param P A penalty matrix
#' @param m Difference order of P-spline
#'
#' @return A list of needed objects
#' @export
#'
#' @examples
#' object = bspline(nbasis = 10)
#' # sequence to evaluate
#' evalarg = seq(0, 1, len = 11)
#' # penalty matrix
#' D = diffpen(object)
#' P = Matrix::crossprod(D)
#' B = predict(object, evalarg)
#' stuff = precompute(B, P, m = 2)
precompute = function(B, P, m) {
  nbasis = ncol(B)

#  if (!chol) {
  eQsqi = eigen(Matrix::crossprod(B))
  V = eQsqi$vectors
  e = eQsqi$values
  Q = Matrix::tcrossprod(V/matrix(sqrt(e), nrow = nrow(V), ncol = ncol(V), byrow = TRUE), V)
  # UPUt = Matrix::crossprod(Q, Matrix::tcrossprod(P, Q))
  # UPUt = Q %*% P %*% Q
  eUPUt = eigen(Matrix::crossprod(Q, P %*% Q))
  # } else if (chol) {
  #   Ci = Matrix::solve(Matrix::chol(Matrix::crossprod(B)))
  #   UPUt2 = Matrix::tcrossprod(Ci %*% P, Ci)
  #   all.equal(as.matrix(UPUt), as.matrix(UPUt2), check.attributes = FALSE)
  # }
  U = eUPUt$vectors
  s = eUPUt$values
  if (m > 0) {
    s[(nbasis - m + 1):nbasis] = 0
  }
  # correct potential numerical issue
  s[s < 0] = 0
  A = B %*% Q %*% U
  structure(list(A = A,
              s = s,
              Q = Q,
              U = U,
              B = B,
              n = nrow(B)),
            class = "assembled_splines")
}
