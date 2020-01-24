#' Connect \code{hero_radsplines}
#'
#' \code{connect} joins multiple \code{hero_radspline}
#' objects into a single \code{hero_radspline}.
#' The \code{e}
#'
#' @param ... A sequence of \code{hero_radspline} objects from the
#' \code{\link{radspline}} function.
#' @seealso \code{\link{radspline}}
#' @return A combined \code{hero_radspline}
#' @export
#'
#' @examples
#' border = border.grid(lon, lat)
#' s1 = radspline(nknots = 36, border = border)
#' plot(s1)
#' s2 = radspline(nknots = 36 * 4, border = border,
#'                width = 6)
#' plot(s2)
#' par(mfrow = c(1, 2))
#' plot(s1)
#' plot(s2)
#' par(mfrow = c(1, 1))
#' s = connect(s1, s2)
#' plot(s)
connect = function(...) {
  arg.list = list(...)
  for (i in seq_along(arg.list)) {
    if (class(arg.list[[i]]) != "hero_radspline") {
      stop("All objects supplied to connect must be hero_radspline objects")
    }
  }
  grid = sapply(arg.list, getElement, name = "grid")
  eborder = arg.list[[1]]$eborder
  border = arg.list[[1]]$border
  width = sapply(arg.list, getElement, name = "width")
  poverlap = arg.list[[1]]$poverlap
  nknots = sapply(arg.list, getElement, name = "nknots")
  nbasis = sum(nknots)
  k = sapply(arg.list, getElement, name = "k")

  return(structure(
    list(grid = grid, eborder = eborder, border = border,
         width = width, poverlap = poverlap,
         nknots = nknots,
         k = k,
         nbasis = nbasis),
    class = "hero_radspline"
  ))
}
