#' @rdname assemble
#' @export
assemble.hero_bspline = function(object, x, m = 2,
                                 sparse = TRUE, ...) {
  nbasis = object$nbasis

  # penalty matrix
  P = Matrix::crossprod(diffpen(x = object, m = m, sparse = sparse))

  # design matrix
  B = predict(object, newx = x, sparse = sparse)

  # precompute stuff
  precompute(B, P, m = m)
}

