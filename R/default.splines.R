#' Construct default splines
#'
#' Construct a list of \code{hero_bsplines} using the
#' default values suggested by Ruppert, Wand, and Carroll
#' (2003).  Specifically, if
#' \code{r = range(evalargs[[i]])} and
#' \code{l = length(evalargs[[i]])}, then Ruppert, Wand,
#' and Carroll (2003) suggest
#' \code{nknots = min(ceiling(l/4), 35)} and the function
#' returns the \code{hero_bspline} for that dimension as
#' \code{bspline(r, nknots = nknots)}.
#'
#' @param evalargs A list of equidistant sequences.
#' @return A list of \code{hero_bsplines}.
#' @references Ruppert, D., Wand, M. P., & Carroll, R. J. (2003).
#'   Semiparametric Regression. Cambridge University Press.
#'   <doi:10.1017/CBO9780511755453>
#' @export
#' @author Joshua French
#' @examples
#' s1 = seq(0, 1, len = 10)
#' s2 = seq(0, 1, len = 20)
#' default.splines(list(s1, s2))
default.splines = function(evalargs) {
  if (!is.list(evalargs)) {
    stop("evalargs must be a list")
  }
  dimx = sapply(evalargs, length)
  splines = vector("list", length(dimx))
  for (i in seq_along(splines)) {
    r = range(evalargs[[i]])
    n = min(ceiling(dimx[i]/4), 35)
    splines[[i]] = bspline(r, nknots = n)
  }
  splines
}

