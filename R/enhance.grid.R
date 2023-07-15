#' Enhance penalty value using grid search
#'
#' \code{enhance.grid} enhances the sandwich smoother by
#' choosing a optimal penalty value to lower the GCV
#' statistic.  A grid search algorithm is utilized based on
#' the each row of \code{par}.  The penalty values (assumed
#' to be on the log scale) are passed to the
#' \code{\link{loglambda2gcv}} function.  If \code{prepare}
#' is \code{TRUE}, then \code{obj} is returned with the
#' penalty values that minimize the GCV statistic during the
#' grid search.  Otherwise, the complete results of the grid
#' search are returned.
#' @inheritParams enhance
#' @param par A matrix-like object (i.e.,
#'   \code{!is.null(dim(par)))}).  Each row contains a set
#'   of parameter values for which the GCV statistic should
#'   be computed.  The number of columns of \code{par}
#'   should match the dimensionality of \code{obj}, i.e,
#'   should equal \code{length(obj)$n}.  If missing, the
#'   default choices are a row of -20s, a row of 0s, and a
#'   row of 20s.
#' @param ... Additional arguments to pass to to the
#'   \code{\link{loglambda2gcv}} function.
#'
#' @return By default, a \code{prepared_*} object with the
#'   optimal \code{loglambda} values that minimize the GCV,
#'   along with an additional component, \code{results},
#'   that contains the optimization results. Otherwise, the
#'   complete results of the grid search.
#' @inheritParams pbapply::pbapply
#' @export
#' @author Joshua French
#' @examples
#' # create b-splines
#' b1 = bspline(nbasis = 10)
#' b2 = bspline(nbasis = 12)
#'
#' # observed data locations
#' x1 = seq(0, 1, len = 60)
#' x2 = seq(0, 1, len = 80)
#'
#' # construct "true" data
#' mu = matrix(0, nrow = 60, ncol = 80)
#' for(i in seq_len(60)) {
#'    for(j in seq_len(80)) {
#'       mu[i, j] =  sin(2*pi*(x1[i]-.5)^3)*cos(4*pi*x2[j])
#'    }
#' }
#' # construct noisy data
#' data = mu + rnorm(60 * 80)
#'
#' obj = prepare(data, list(x1, x2), list(b1, b2))
#' enhance.grid(obj, prepare = FALSE)
enhance.grid = function(obj, par, prepare = TRUE,
                        loggcv = FALSE, ..., cl = NULL) {
  if (missing(par)) {
    par = matrix(rep(c(-20, 0, 20), each = length(obj$n)),
                 byrow = TRUE, ncol = length(obj$n))
  }
  if (is.null(par)) {
    stop("par must matrix-like")
  }
  if (ncol(par) != length(obj$n)) {
    stop("ncol(par) does not match the dimensionality of obj, specifically,
         ncol(par) != length(obj$n)")
  }
  # types of prepared objects
  prepared_types = c("prepared_numeric", "prepared_matrix",
                     "prepared_array", "prepared_starray",
                     "prepared_sts", "prepared_list", "prepared_sequential")
  # confirm obj has appropriate type
  if (!any(is.element(class(obj), prepared_types))) {
    stop("obj must be of class 'prepared_numeric', 'prepared_matrix', 'prepared_array', 'prepared_starray', 'prepared_sts', 'prepared_list', or 'prepared_sequential'.  Please use a prepare function to prepare your data.")
  }
  colnames(par) = paste0("par_", seq_len(ncol(par)))
  gcv = pbapply::pbapply(par, 1, loglambda2gcv, obj = obj,
                         loggcv = loggcv, cl = cl)
  wmin = which.min(gcv)

  obj$loglambda = par[wmin, ]
  obj$results = c(loglambda = par[wmin, ], gcv = gcv[wmin])

  # return results or prepared object?
  if (prepare) {
    return(obj)
  } else {
    return(as.data.frame(cbind(par, gcv = gcv)))
  }
}
