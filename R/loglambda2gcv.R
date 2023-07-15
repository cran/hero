#' Determine GCV statistic
#'
#' \code{loglambda2gcv} uses a vector of penalty values
#' to evaluate the GCV statistic for a
#' \code{prepared_response} object.
#'
#' Though this function can be used by the user, it is
#' basically an internal function used to find the
#' value of \code{loglambda} minimizing the GCV statistic.
#'
#' @param obj A \code{prepared_*} object from a
#' \code{\link{prepare}} function.
#' @param loglambda A vector of penalty values (assumed to
#' be on a natural logarithmic scale) for computing the GCV.
#' @param loggcv A logical value indicating whether the log of the
#' GCV statistic should be returned.
#' The default is \code{FALSE}.
#' @seealso \code{\link{prepare}}
#' @return The scalar GCV statistic
#' @export
#'
#' @examples
#' n1 = 10
#' b1 = bspline(nbasis = 10)
#' x1 = seq(0, 1, len = n1)
#' n2 = 20
#' x2 = seq(0, 1, len = n2)
#' b2 = bspline(nbasis = 12)
#' # construct "true" data
#' mu = matrix(0, nrow = n1, ncol = n2)
#' for(i in seq_len(n1)) {
#'    for(j in seq_len(n2)) {
#'       mu[i, j] =  sin(2*pi*(x1[i]-.5)^3)*cos(4*pi*x2[j])
#'    }
#' }
#' image(mu)
#' # construct noisy data
#' data = mu + rnorm(n1 * n2)
#' x = list(x1, x2)
#' splines = list(b1, b2)
#' obj = prepare(data, x, splines)
#' loglambda2gcv(c(0, 0), obj)
loglambda2gcv = function(loglambda, obj, loggcv = FALSE) {
  # types of prepared objects
  prepared_types = c("prepared_numeric", "prepared_matrix",
                     "prepared_array", "prepared_starray",
                     "prepared_sts", "prepared_list", "prepared_sequential")
  # confirm obj has appropriate type
  if (!any(is.element(class(obj), prepared_types))) {
    stop("obj must be produced by a prepare function")
  }
  if (!is.numeric(loglambda)) {
    stop("loglambda must be numeric")
  }
  if (length(loglambda) != length(obj$n)) {
    stop("length of loglambda doesn't match
         dimensionality of obj")
  }

  lambda = exp(loglambda)

  dtildei = lapply(rev(seq_along(lambda)), function(i) 1/(1 + lambda[i] * obj$s[[i]]))
  dtilde = kronecker.seq(dtildei)
  traced = prod(sapply(dtildei, sum))
  den = (1 - traced/prod(obj$n))^2

  if (!any(is.element(class(obj), c("prepared_list", "prepared_sequential")))) {
    ytilde = as.vector(obj$Ytilde)
    gcv = (obj$sum_ysq + sum((ytilde*dtilde)^2) - 2*sum((ytilde*sqrt(dtilde))^2))/den
  } else {
    gcv = sapply(seq_along(obj$sum_ysq), function(i) {
      # ytilde = as.vector(obj$Ytilde[[i]])
      (obj$sum_ysq[[i]] + sum((as.vector(obj$Ytilde[[i]])*dtilde)^2) - 2*sum((as.vector(obj$Ytilde[[i]])*sqrt(dtilde))^2))/den
    })
    gcv = sum(gcv)
  }

  # should log of gcv be returned instead
  if (loggcv) {
    gcv = log(gcv)
  }
  return(gcv)
}
