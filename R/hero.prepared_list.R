#' @rdname hero
#' @export
hero.prepared_list = function(x, ...) {
  arglist = list(...)
  if (is.null(arglist$fitted)) {
    fitted = TRUE
  } else {
    fitted = arglist$fitted
  }
  if (length(fitted) != 1) stop("fitted must be a single logical value")
  if (!is.logical(fitted)) stop("fitted must be a single logical value")

  lambda = exp(x$loglambda)
  parts = lapply(seq_along(lambda), function(i) {
    x$Q[[i]] %*% x$U[[i]] %*% diag(1/(1 + lambda[i]*x$s[[i]]))
  })

  out = lapply(x$Ytilde, function(Y.tilde){
    if (!fitted) {
      return(list(coefficients = rh.seq(parts, Y.tilde)))
    } else {
      coeffs = rh.seq(parts, Y.tilde)
      return(list(coefficients = coeffs,
                  fitted = rh.seq(x$B, coeffs)))
    }
  })
  class(out) = c("hero_list", "hero")
  return(out)
}


