#' Prepare data for sandwich smooth
#'
#' A generic function to prepare various types of data.  See
#' the functions linked in See Also.
#'
#' @param data The data to prepare
#' @param ... Not implemented
#'
#' @return A prepared object
#' @seealso \code{\link{prepare.numeric}},
#' \code{\link{prepare.matrix}},
#' \code{\link{prepare.array}},
#' \code{\link{prepare.sts}},
#' \code{\link{prepare.starray}}
#' @export
prepare = function(data, ...) {
  UseMethod("prepare", data)
}


