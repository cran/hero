#' Enhance penalty value
#'
#' \code{enhance} enhances the sandwich smoother by choosing
#' the optimal penalty value that minimizes the GCV
#' statistic.    The \code{\link[optimx]{optimx}} function
#' is used to do the optimization.
#'
#' Internally, the \code{\link{loglambda2gcv}} is  used as
#' the objective function for the
#' \code{\link[optimx]{optimx}} function. Many different
#' optimization methods are available.  The default is
#' \code{L-BFGS-B}, which allows for constraints on the
#' parameters to optimize.  Another excellent choice is the
#' \code{nlminb} algorithm, which also allows for parameter
#' constraints.
#'
#' @param method The method to be used for optimization. The
#'   default is \code{L-BFGS-B}, which allows for
#'   constraints on the parameters to optimize.  See
#'   \code{\link[optimx]{optimx}} for all available methods.
#' @param prepare A logical value.  The default is \code{TRUE},
#' indicating that a \code{prepared_data} object should be
#' returned.  If \code{FALSE}, then the results of the
#' call to the \code{\link[optimx]{optimx}} function is
#' returned.
#' @inheritParams loglambda2gcv
#' @inheritParams optimx::optimx
#' @param loggcv A logical value indicating whether the log
#' of the GCV statistic should be used.  Useful for very large
#' data sets.  Default is \code{TRUE}.
#' @param ... Additional arguments to pass to to the
#'   \code{\link[optimx]{optimx}} function.
#'
#' @return By default, a \code{prepared_data} object with
#'   the optimal \code{loglambda} values that minimize the
#'   GCV, along with an additional component,
#'   \code{results}, that contains the optimization results.
#' @export
#' @author Joshua French
#' @examples
#' # create b-splines
#' x1 = bspline(nbasis = 10)
#' x2 = bspline(nbasis = 12)
#'
#' # observed data locations
#' evalarg1 = seq(0, 1, len = 60)
#' evalarg2 = seq(0, 1, len = 80)
#'
#' # construct "true" data
#' mu = matrix(0, nrow = 60, ncol = 80)
#' for(i in seq_len(60)) {
#'    for(j in seq_len(80)) {
#'       mu[i, j] =  sin(2*pi*(evalarg1[i]-.5)^3)*cos(4*pi*evalarg2[j])
#'    }
#' }
#' # construct noisy data
#' data = mu + rnorm(60 * 80)
#'
#' obj = prepare(data, list(evalarg1, evalarg2), list(x1, x2))
#' enhance(obj)
enhance = function(obj, par = rep(0, length(obj$n)),
                   lower = rep(-20, length(par)),
                   upper = rep(20, length(par)),
                   method = "L-BFGS-B",
                   control = list(), prepare = TRUE,
                   loggcv = FALSE, ...) {
  if (class(obj) != "prepared_numeric" &
      class(obj) != "prepared_matrix" &
      class(obj) != "prepared_array" &
      class(obj) != "prepared_starray" &
      class(obj) != "prepared_sts" &
      class(obj) != "prepared_list") {
    stop("obj must be of class prepared_numeric, prepared_matrix, prepared_array, prepared_starray, prepared_sts, or prepared_list.  Please use the prepare function to prepare your data.")
  }
  if (length(loggcv) != 1) {
    stop("loggcv must be a single logical value")
  }
  if (!is.logical(loggcv)) {
    stop("loggcv must be a single logical value")
  }

  if (class(obj) == "prepared_numeric") {
    results = stats::optimize(loglambda2gcv,
                              lower = lower, upper = upper,
                              obj = obj, loggcv = loggcv,
                              ...)
    obj$loglambda = results$minimum
    obj$results = results
  } else if (class(obj) == "prepared_matrix" |
             class(obj) == "prepared_array" |
             class(obj) == "prepared_starray" |
             class(obj) == "prepared_sts" |
             class(obj) == "prepared_list") {
    results = optimx::optimx(par = par, fn = loglambda2gcv,
                             method = method,
                             lower = lower, upper = upper,
                             control = control,
                             obj = obj, loggcv = loggcv,
                             ...)
    obj$loglambda = as.numeric(results[seq_along(par)])
    obj$results = results
  }

  # return results or prepared object?
  if (prepare) {
    return(obj)
  } else {
    return(results)
  }
}
