#' Predict method for a \code{hero_radspline}
#'
#' Predicted values based on object created by
#' \code{\link{radspline}}.
#' @param object A \code{hero_radspline} object created by
#'   \code{\link{radspline}}.
#' @param newx A numeric matrix at which to evaluate the
#'   radial basis splines functions.
#' @param longlat Use Euclidean (\code{FALSE}) or Great Circle
#'   (WGS84 ellipsoid) distance (\code{TRUE}).  Default is
#'   \code{FALSE}.
#' @param join A logical value.  \code{TRUE}, the default,
#' indicates that the predictions from each set of radial
#' basis functions should be joined column-wise.  Otherwise,
#' a list with the predictions from each set of basis functions
#' is returned.
#' @inheritParams predict.hero_bspline
#'
#' @param ... Not currently implemented.
#' @method predict hero_radspline
#' @return An \eqn{n \times k} matrix (or
#'   \code{\link[Matrix]{Matrix-class}} object if
#'   \code{sparse = TRUE}), where \eqn{n} is the number of
#'   rows in \code{newx} and \eqn{k} is the number of
#'   basis functions in \code{object}.  Each row gives the
#'   predicted values of each \code{newx} value evaluated
#'   at each of the basis functions.
#' @export
#' @seealso \code{\link{radspline}}
#' @examples
#' border = border.grid(lon, lat)
#' r = radspline(nknots = c(36, 36 * 4), border = border)
#' newx = cbind(c(lon), c(lat))
#' p = predict(r, newx)
predict.hero_radspline = function(object,
                                  newx,
                                  sparse = TRUE,
                                  longlat = FALSE,
                                  join = TRUE, ...) {
  B = vector("list", length(object$grid))
  for (i in seq_along(object$grid)) {
    gcoords = sp::coordinates(object$grid[[i]])
    gsd = apply(sp::spDists(gcoords, longlat = longlat), 1, sort)
    md = object$poverlap * max(gsd[2,])
    d = sp::spDists(newx, gcoords, longlat = longlat)
    B[[i]] = Matrix::Matrix(fields::Wendland(d,
                                             theta = md,
                                             dimension = 2,
                                             k = object$k[i]),
                            sparse = sparse)
  }
  if (!join) {
    return(B)
  } else {
    return(do.call(cbind, B))
  }
}


