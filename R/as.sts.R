#' Convert object to \code{sts} class
#'
#' Convert a numeric three-dimensional \code{array} or
#' two-dimensional matrix-like object to an
#' \code{sts} (spatial time series) object.  If \code{x} is
#' a three-dimensional array, the first two dimensions are
#' assumed to relate to gridded spatial positions.  If
#' \code{x} has only two dimensions, each row is a time
#' series for a specific location.  Each column is a
#' realization of a geostatistical process at a specific
#' time.
#'
#' This method has been tested with objects of class
#' \code{\link{matrix}}, \code{\link{data.frame}},
#' \code{\link{array}}, and
#' \code{\link[Matrix]{Matrix-class}}.  It should be
#' possible for \code{x} to have a different class as long
#' as the object has a loaded \code{\link{as.matrix}}
#' method, which is used in this function.
#'
#' @param x A matrix-like object with 2 dimensions or an
#' array with 3 dimensions.
#' @return An \code{sts} object.
#' @export
#'
#' @examples
#' # 3d array to sts
#' sts = as.sts(tasmax)
#' class(sts)
#'
#' # extract a subset of tasmax to produce an sts
#' x = matrix(c(tasmax[50:60, 50:60, ]), ncol = 30)
#' sts = as.sts(x)
#' class(sts)
#'
#' sts = as.sts(as.array(x))
#' class(sts)
#'
#' sts = as.sts(Matrix::Matrix(x))
#' class(sts)
#'
#' sts = as.sts(as.data.frame(x))
#' class(sts)
as.sts = function(x) {
  d = dim(x)
  if (is.null(d)) {
    stop("x must have non-NULL dimensions")
  }
  ld = length(d)

  if (is.array(x)) {
    if (ld != 2 & ld != 3) {
      stop("x may only have two or three-dimensions")
    }
  }
  if (ld == 2) {
    x = as.matrix(x)
  } else {
    x = matrix(c(x), ncol = dim(x)[3])
  }
  if (!is.numeric(x)) {
    stop("x must contain only numerical values")
  }
  if (max(is.na(x)) == 1) {
    stop("x cannot contain any NAs")
  }
  structure(list(data = x), class = "sts")
}

#' @rdname as.sts
#' @export
sts = as.sts

#' @rdname as.sts
#' @export
as_sts = as.sts
