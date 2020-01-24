#' @rdname plot.hero
#' @param type The plot type (when \code{x} is of subclass
#' \code{hero_numeric}).  Default is \code{type = "l"}.
#' @method plot hero_numeric
#' @export
plot.hero_numeric = function(x, xlab = "", ylab = "",
                             type = "l", ...) {
  graphics::plot(x = x$x, y = x$fitted,
                 xlab = xlab, ylab = ylab,
                 type = type, ...)
}

