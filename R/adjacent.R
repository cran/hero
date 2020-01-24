#' Determine adjacent points
#'
#' \code{adjacent} attempts to find the point(s) adjacent
#' (closest) to each point.  The data are implicitly assumed
#' to be on a grid, otherwise this function isn't very
#' useful. Distances between each point and other points in
#' \code{coords} are computed and then rounded using the
#' \code{\link[base]{round}} function. Let \code{k} denote
#' the minimum distance between a reference point and all
#' other points.  A point is adjacent to the reference point
#' if (after rounding), it's distance from the reference
#' point matches the minimum distance \code{k}.
#'
#' \code{digits} is the number of digits used by
#' \code{\link[base]{round}} in the rounding process.
#'
#' @param coords A two-dimensional matrix-like object with
#'   non-NULL dimensions.
#' @inheritParams radspline
#' @param digits The number of digits to use when applying
#'   \code{\link[base]{round}} to the distances.
#'
#' @return  A \code{hero_adjacent} object.  This is simply a
#'   list with elements \code{nbrs} and \code{coords}.
#'   \code{nbrs} is a list specifying the adjacent points
#'   for each point.  \code{coords} is simply the original
#'   \code{coords} supplied to the function and is retained
#'   for plotting purposes.
#' @export
#' @examples
#' # basic coordinates
#' coords = expand.grid(1:4, 1:4)
#' # plot coordinates to see relationships
#' plot(coords, type = "n")
#' text(coords)
#' a = adjacent(coords, digits = 1)
#' plot(a)
adjacent = function(coords, longlat = FALSE, digits = 1) {
  arg_check_adjacent(coords, longlat, digits)
  coords = as.matrix(coords)
  dimnames(coords) = NULL
  d = sp::spDists(coords, longlat = longlat)
  do = apply(d, 1, order)[-1,]
  ds = round(apply(d, 1, sort)[-1,], digits = digits)
  first = lapply(seq_len(ncol(ds)), function(i) which(ds[,i] == ds[1, i]))
  nbrs = sapply(seq_along(first), function(i) do[,i][first[[i]]],
                USE.NAMES = FALSE, simplify = FALSE)
  structure(list(nbrs = nbrs,
                 coords = coords),
            class = "hero_adjacent")
}

arg_check_adjacent = function(coords, longlat, digits) {
  if (is.null(dim(coords))) {
    stop("coords must have non-NULL dimensions")
  }
  if (length(dim(coords)) != 2) {
    stop("coords must be two-dimensional")
  }
  if (!is.logical(longlat) | length(longlat) != 1) {
    stop("longlat must be a logical value")
  }
  if (!is.numeric(digits) | length(digits) != 1 | digits < 0) {
    stop("digits must be a non-negative integer")
  }
}
