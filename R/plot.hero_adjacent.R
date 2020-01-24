#' Plot a \code{hero_adjacent} object
#'
#' Plot a \code{hero_adjacent} object.  \code{x$nbrs} is
#' used to construct a
#' \code{\link[Matrix]{sparseMatrix-class}} object.  The
#' default behavior is to plot the sparse matrix using the
#' \code{\link[graphics]{image}} function.  However, if the
#' igraph package is installed, a graph is made using
#' \code{\link[igraph]{graph_from_adjacency_matrix}} and
#' then plotted using \code{\link[igraph]{plot.igraph}}.
#'
#' @param x A \code{hero_adjacent} object
#' @param ... Additional arguments passed to
#'   \code{\link[graphics]{image}}, or if the igraph package
#'   is installed, \code{\link[igraph]{plot.igraph}}.
#' @method plot hero_adjacent
#' @rdname plot.hero_adjacent
#' @export
#' @examples
#' coords = expand.grid(1:4, 1:4)
#' a = adjacent(coords, digits = 1)
#' plot(a)
plot.hero_adjacent = function(x, ...) {
  nnbrs = sapply(x$nbrs, length)
  a = Matrix::sparseMatrix(i = rep(seq_along(nnbrs), times = nnbrs),
                           j = unlist(x$nbrs),
                           x = 1)

  if (!requireNamespace("igraph", quietly = TRUE)) {
    message("The \"igraph\" package will enhance this function. Please install it.")
    graphics::image(a, ...)

  } else {
    g = igraph::graph_from_adjacency_matrix(a)
    plot(g, layout = as.matrix(x$coords), ...)
  }
}
