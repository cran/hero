#' Spatial difference penalty
#'
#' \code{spdiffpen} computes the \code{m}th order spatial
#' difference penalty for a set of coordinates.
#'
#' \code{\link{adjacent}} is used to determine the
#' first-order neighbors of each point in \code{coords}. The
#' difference penalties are then successively determined
#' from that.
#'
#' If \code{sparse = TRUE}, a
#' \code{\link[Matrix]{sparseMatrix-class}} Matrix is
#' returned when the penalty matrix is relatively sparse
#' (typically, at least half the entries are zero).
#' Otherwise, something of the more general
#' \code{\link[Matrix]{Matrix-class}} is returned.
#'
#' @inheritParams adjacent
#' @inheritParams diffpen
#' @inherit diffpen return
#' @export
#' @examples
#' coords = expand.grid(1:4, 1:4)
#' # first order difference penalty
#' d1 = spdiffpen(coords, digits = 1)
#' # second order difference penalty
#' d2 = spdiffpen(coords, m = 2, digits = 1)
#' # third order difference penalty
#' d3 = spdiffpen(coords, m = 3, digits = 1)
spdiffpen = function(coords, m = 1, sparse = TRUE, longlat = FALSE, digits = 1) {
  arg_check_spdiffpen(coords, m, sparse, longlat, digits)
  nbrs = adjacent(coords, longlat = longlat, digits = digits)$nbrs
  nnbrs = sapply(nbrs, length)
  dpen = -Matrix::sparseMatrix(i = rep(seq_along(nnbrs), times = nnbrs),
                           j = unlist(nbrs),
                           x = 1)
  diag(dpen) = -Matrix::rowSums(dpen)
  if (m == 1) {
    return(dpen)
  }
  tnbrs = nbrs
  tpen = dpen
  count = 1
  prev_nbrs = as.list(seq_along(nbrs))
  while (count < m) {
    for (i in seq_along(nbrs)) {
      dpen[i, ] = tpen[i, ] - Matrix::colSums(tpen[tnbrs[[i]], , drop = FALSE])
      prev_nbrs[[i]] = unique(c(prev_nbrs[[i]], tnbrs[[i]]))
      new_nbrs = unique(unlist(nbrs[tnbrs[[i]]]))
      tnbrs[[i]] = setdiff(new_nbrs, prev_nbrs[[i]])
    }
    count = count + 1
    tpen = dpen
  }
  if (sparse) {
    return(Matrix::Matrix(dpen))
  } else {
    return(as.matrix(dpen))
  }
}

arg_check_spdiffpen = function(coords, m, sparse, longlat, digits) {
  arg_check_adjacent(coords, longlat, digits)
  if (length(m) != 1 | m < 1) {
    stop("m should be a positive integer")
  }
  if (length(sparse) != 1 | !is.logical(sparse)) {
    stop("sparse should be a logical value")
  }
}
