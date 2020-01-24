#' @rdname hero
#' @export
hero.prepared_sts = function(x, ...) {
  lambda = exp(x$loglambda)
  parts = lapply(seq_along(lambda), function(i) {
    x$Q[[i]] %*% x$U[[i]] %*% diag(1/(1 + lambda[i]*x$s[[i]]))
  })
  coeffs = rh.seq(parts, x$Ytilde)
  fitted = as.matrix(x$B[[1]] %*% Matrix::tcrossprod(coeffs, x$B[[2]]))
  out = list(fitted = fitted, coefficients = coeffs, x = x$x)
  class(out) = c("hero_sts", "hero")
  return(out)
}


