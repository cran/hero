% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default.splines.R
\name{default.splines}
\alias{default.splines}
\title{Construct default splines}
\usage{
default.splines(evalargs)
}
\arguments{
\item{evalargs}{A list of equidistant sequences.}
}
\value{
A list of \code{hero_bsplines}.
}
\description{
Construct a list of \code{hero_bsplines} using the
default values suggested by Ruppert, Wand, and Carroll
(2003).  Specifically, if
\code{r = range(evalargs[[i]])} and
\code{l = length(evalargs[[i]])}, then Ruppert, Wand,
and Carroll (2003) suggest
\code{nknots = min(ceiling(l/4), 35)} and the function
returns the \code{hero_bspline} for that dimension as
\code{bspline(r, nknots = nknots)}.
}
\examples{
s1 = seq(0, 1, len = 10)
s2 = seq(0, 1, len = 20)
default.splines(list(s1, s2))
}
\references{
Ruppert, D., Wand, M. P., & Carroll, R. J. (2003).
  Semiparametric Regression. Cambridge University Press.
  <doi:10.1017/CBO9780511755453>
}
\author{
Joshua French
}
