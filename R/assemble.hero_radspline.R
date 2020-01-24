#' @rdname assemble
#' @export
assemble.hero_radspline =
  function(object, x, m = 2, sparse = TRUE,
           spdiffpen = TRUE, digits = 1, ...) {
  arg_check_assemble_hero_radspline(x, m, sparse, spdiffpen)

  # design matrix
  B = predict(object, newx = x,
              sparse = sparse,
              longlat = object$longlat)

  # penalty matrix for each set of grid locations
  if (spdiffpen) {
    Pl = lapply(object$grid, function(x)
      spdiffpen(sp::coordinates(x), m = m, sparse = sparse,
                longlat = FALSE, digits = digits)
    )
    P = Matrix::crossprod(Matrix::bdiag(Pl))
    precompute(B, P, m = 0)
  } else {
    P = Matrix::crossprod(diffpen(x = object, m = m, sparse = sparse))
    precompute(B, P, m = m)
  }
}

arg_check_assemble_hero_radspline =
  function(x, m, sparse, spdiffpen) {
    d = dim(x)
    if (is.null(d)) {
      stop("x must be matrix-like with non-NULL dimensions")
    }
    if (d[2] != 2) {
      stop("x must have two columns")
    }
    if (!is.numeric(x)) {
      stop("x must be numeric")
    }
    arg_check_m(m)
    arg_check_sparse(sparse)
    if (length(spdiffpen) != 1 | !is.logical(spdiffpen)) {
      stop("spdiffpen must be a logical value")
    }
}
