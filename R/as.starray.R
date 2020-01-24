#' Convert array to \code{starray}
#'
#' Convert a three-dimensional spatio-temporal array into
#' an \code{starray} object.  The first two dimensions are
#' assumed to relate to gridded spatial positions.
#'
#' @param x A three-dimensional array
#' @return An \code{starray} object.
#' @export
#'
#' @examples
#' star = as.starray(tasmax)
#' class(star)
as.starray = function(x) {
  if (!is.array(x)) {
    stop("x must be an array")
  }
  if (length(dim(x)) != 3) {
    stop("x must be a three-dimensional array")
  }
  structure(list(data = matrix(c(x), ncol = dim(x)[3])),
            class = "starray")
}

#' @rdname as.starray
#' @export
starray = as.starray

#' @rdname as.starray
#' @export
as_starray = as.starray
