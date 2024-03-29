% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enlarge.R
\name{enlarge}
\alias{enlarge}
\title{Enlarge spatial domain}
\usage{
enlarge(x, width, ...)
}
\arguments{
\item{x}{A \code{\link[sp]{SpatialPolygons-class}} object
defining the border(s) of the spatial domain.}

\item{width}{The width to enlarge the study area.
Distance from original geometry to include in the new
geometry. Negative values are allowed.}

\item{...}{Additional arguments to pass to
\code{\link[sf]{st_buffer}}.}
}
\value{
An object of class \code{hero_enlarge}.  This is
  simply a list with \code{eborder} (the enlarged
  border), \code{border} (the border of the original
  coordinates), and the width of the enlargement.
  \code{eborder} and \code{border} are
  \code{\link[sp]{SpatialPolygons-class}} objects.
}
\description{
Enlarge the spatial domain of a
\code{\link[sp]{SpatialPolygons-class}} object.  If
\code{width} isn't specified, then 10\% of the maximum
distance between the points specified by the bounding
box is used. The
\code{\link[sf]{st_buffer}} function is used to enlarge
\code{x}.
}
\examples{
# enlarge regular grid
# create x and y defining square border
x = seq(min(lon), max(lon), length = 60)
y = seq(min(lat), max(lat), length = 80)
border = border.grid(x, y)
e = enlarge(border)
plot(e)

# create x and y defininging an irregular grid
border2 = border.grid(lon, lat)
e2 = enlarge(border2)
plot(e2)
}
\author{
Joshua French
}
