#' @rdname hero
#' @export
hero.prepared_numeric = function(x, ...) {
  lambda = exp(x$loglambda)
  coeffs = x$Q[[1]] %*% x$U[[1]] %*% diag(1/(1 + lambda*x$s[[1]])) %*% x$Ytilde
  fitted = as.matrix(x$B[[1]] %*% coeffs)
  out = list(fitted = as.vector(fitted),
             coefficients = as.vector(coeffs),
             x = x$x)
  class(out) = c("hero_numeric", "hero")
  return(out)
}


