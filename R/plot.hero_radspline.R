#' Plot a \code{hero_radspline}
#'
#' Plot a \code{hero_radspline} to compare the knots to the
#' observed data locations.
#'
#' If the default plotting styles for \code{x$grid} are to
#' be changed, the user can either choose a single
#' color/style that is replicated for each element of
#' \code{x$grid} or supply a vector which has length
#' matching \code{length{x$grid}}.  See Examples.
#'
#' @rdname plot.hero_radspline
#' @param x A \code{hero_radspline} object.
#' @param ... Additional arguments to pass the plot method
#'   associated with \code{\link[sp]{SpatialPolygons-class}}
#'   when plotting \code{x$eborder}.
#' @param blist A list to pass the plot method associated
#'   with \code{\link[sp]{SpatialPolygons-class}} when
#'   plotting \code{x$border}.  The default is a
#'   grey-colored polygon.
#' @param glist A list to pass the plot method associated
#'   with \code{\link[sp]{SpatialPoints-class}} when
#'   plotting each element of the list \code{x$grid}.  A
#'   basic color scheme and point style is automatically
#'   chosen if none is supplied.
#' @method plot hero_radspline
#' @seealso \code{\link{radspline}}
#' @author Joshua French
#' @export
#' @examples
#' border = border.grid(lon, lat)
#' r = radspline(nknots = c(36, 36 * 4), border = border)
#' # default color scheme
#' plot(r)
#' # change color and point styles of points,
#' # and background of original domain
#' plot(r, blist = list(col = "yellow"),
#'         glist = list(col = c("blue", "orange"),
#'                      pch =  3:4))
plot.hero_radspline =
  function(x, blist = list(col = "grey"),
              glist = list(col = seq_along(x$grid) + 1,
              pch = seq_along(x$grid)),
              ...) {
  if (!is.list(blist))
    stop("blist must be a list")
  if (!is.list(glist))
    stop("glist must be a list")
  if (is.null(glist$col)) {
    glist$col = seq_along(x$grid) + 1
  }
  ng = length(x$grid)
  if (length(glist$col) == 1) {
    glist$col = rep(glist$col, ng)
  } else if (length(glist$col) != ng) {
    stop("glist$col must be a single color or match the length of x$grid")
  }
  if (is.null(glist$pch)) {
    glist$pch = seq_along(x$grid)
  }
  if (length(glist$pch) == 1) {
    glist$pch = rep(glist$pch, ng)
  } else if (length(glist$pch) != ng) {
    stop("glist$pch must be a single choice or match the length of x$grid")
  }

  sp::plot(x$eborder, ...)
  blist$x = x$border
  blist$add = TRUE
  do.call(sp::plot, blist)

  for (i in rev(seq_along(x$grid))) {
    tlist = list(
      x = x$grid[[i]],
      add = TRUE,
      col = glist$col[i],
      pch = glist$pch[i]
    )
    do.call(sp::plot, tlist)
  }
}
