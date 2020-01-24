#' Construct border for grid
#'
#' \code{border.grid} determines the border for
#' data on a grid.  \code{x} and \code{y} must define a
#' regular or irregular grid.  See Details.
#'
#' A regular grid is defined by ascending numeric vectors
#' \code{x} and \code{y}.  A vector \code{x} is ascending if
#' \code{x[i] < x[j]} for \code{i < j}.
#'
#' An irregular grid is defind by ascending matrices.
#' A matrix \code{x} is ascending if \code{x[i, j] < x[i, l]}
#' for \code{j < l} and if \code{x[i, j] < x[k, j]}
#' and \code{j < k}.
#'
#' @param x A vector or matrix of x coordinates.  See
#'   Details.
#' @param y A vector or matrix of y coordinates.  See
#'   Details.
#' @param proj4string A projection string of class
#'   \code{\link[sp]{CRS-class}}.  If not provided, then
#'   default values are used.  This should be changed with
#'   caution.
#' @return A \code{\link[sp]{SpatialPolygons}} object.
#' @export
#' @author Joshua French
#' @examples
#' # create x and y defining square border
#' x = seq(min(lon), max(lon), length = 60)
#' y = seq(min(lat), max(lat), length = 80)
#' border = border.grid(x, y)
#' sp::plot(border)
#'
#' # use lon and lat to define border of an irregular grid
#' border2 = border.grid(lon, lat)
#' sp::plot(border2)
border.grid = function(x, y, proj4string) {
  is_grid(x, y)
  if (missing(proj4string)) {
    proj4string = sp::CRS(as.character(NA))
  }
  if (is.vector(x)) {
    lx = length(x)
    ly = length(y)
    xb = x[c(1, lx, lx, 1, 1)]
    yb = y[c(1, 1, ly, ly, 1)]
  } else {
    nc = ncol(x)
    nr = nrow(x)
    xb = c(x[1,], x[,nc], rev(x[nr,]), rev(x[,1]))
    yb = c(y[1,], y[,nc], rev(y[nr,]), rev(y[,1]))
  }
  sppoly = sp::Polygon(cbind(xb, yb), FALSE)
  sppoly = sp::Polygons(list(sppoly), "border")
  sppoly = sp::SpatialPolygons(Srl = list(sppoly),
                               proj4string = proj4string)
  return(sppoly)

}

is_grid = function(x, y) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  if (!is.numeric(y)) {
    stop("y must be numeric")
  }
  if (!is.vector(x) & !is.matrix(x)) {
    stop("x must be a numeric vector or matrix")
  }
  if (!is.vector(y) & !is.matrix(y)) {
    stop("y must be a numeric vector or matrix")
  }
  if (!is.matrix(x)) {
    if (is.matrix(y)) {
      stop("x and y must both be numeric vectors or both be numeric matrices")
    }
  }
  if (is.vector(x)) {
    if (!isTRUE(all.equal(sort(x), x, check.attributes = FALSE))) {
      stop("x must be an ascending sequence if it is a vector")
    }
    if (!isTRUE(all.equal(sort(y), y, check.attributes = FALSE))) {
      stop("y must be an ascending sequence if it is a vector")
    }
  }
}

#' @rdname border.grid
#' @export
border_grid = border.grid

#' @rdname border.grid
#' @export
borderGrid = border.grid

#' @rdname border.grid
#' @export
BorderGrid = border.grid

