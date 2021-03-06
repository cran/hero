% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.hero_radspline.R
\name{predict.hero_radspline}
\alias{predict.hero_radspline}
\title{Predict method for a \code{hero_radspline}}
\usage{
\method{predict}{hero_radspline}(object, newx, sparse = TRUE, longlat = FALSE, join = TRUE, ...)
}
\arguments{
\item{object}{A \code{hero_radspline} object created by
\code{\link{radspline}}.}

\item{newx}{A numeric matrix at which to evaluate the
radial basis splines functions.}

\item{sparse}{A logical value indicating if the result
should be a sparse version of the
\code{\link[Matrix]{Matrix-class}}.}

\item{longlat}{Use Euclidean (\code{FALSE}) or Great Circle
(WGS84 ellipsoid) distance (\code{TRUE}).  Default is
\code{FALSE}.}

\item{join}{A logical value.  \code{TRUE}, the default,
indicates that the predictions from each set of radial
basis functions should be joined column-wise.  Otherwise,
a list with the predictions from each set of basis functions
is returned.}

\item{...}{Not currently implemented.}
}
\value{
An \eqn{n \times k} matrix (or
  \code{\link[Matrix]{Matrix-class}} object if
  \code{sparse = TRUE}), where \eqn{n} is the number of
  rows in \code{newx} and \eqn{k} is the number of
  basis functions in \code{object}.  Each row gives the
  predicted values of each \code{newx} value evaluated
  at each of the basis functions.
}
\description{
Predicted values based on object created by
\code{\link{radspline}}.
}
\examples{
border = border.grid(lon, lat)
r = radspline(nknots = c(36, 36 * 4), border = border)
newx = cbind(c(lon), c(lat))
p = predict(r, newx)
}
\seealso{
\code{\link{radspline}}
}
