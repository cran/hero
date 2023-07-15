#' Design knot/breakpoint spacing
#'
#' See Details of \code{\link{bspline}} for additional
#' information about arguments.
#' @param rangeval A numeric vector of length 2 defining the
#' interval over which the functional data object can be
#' evaulated.  The default value is \code{0:1}.
#' @param nbasis An integer specifying the number of
#' basis functions to construct.  This is closely linked to
#' the number of knots (\code{nknots}), and
#' \code{nknots = nbasis - norder}.
#' @param nknots The number of *interior* knots.  See Details.
#' @param norder An integer specifying the order of the B-splines, which is one higher than their degree.  The default is 4, which corresponds to cubic splines.
#' @param extend Should the knots stop at the endpoints specified by \code{rangeval}?  Default is \code{FALSE}. See Details.
#' @param interior A logical value specifying whether only interior knots should be returned.  Default is \code{FALSE}.
#' @aliases knot_design knotDesign KnotDesign
#' @return An ascending sequence of univarite knot locations.
#' @export
#' @examples
#' if (requireNamespace("fda", quietly = TRUE)) {
#' b = fda::create.bspline.basis(nbasis = 10)
#' # interior knots only
#' bknots = b$params
#' # should match
#' knots = knot.design(nbasis = 10, interior = TRUE)
#' all.equal(bknots, knots)
#' }
knot.design <- function(rangeval = 0:1,
                        nbasis,
                        nknots, norder = 4, extend = FALSE,
                        interior = FALSE) {
  if (missing(nbasis) & missing(nknots)) stop("nbasis or nknots must be specified")
  if (!missing(nbasis) & !missing(nknots)) {
    nbasis2 = nknots + norder
    if (nbasis != nbasis2) stop("nbasis != nknots + norder")
  }
  if (!missing(nbasis)) {
    nknots = nbasis - norder
  }
  arg_check_knot_design(nknots, norder, extend, interior)

  knots = seq(-norder + 1, nknots + norder, length = nknots + 2 * norder)/(nknots + 1)
  if (!extend) {
    knots[seq_len(norder)] = 0
    knots[length(knots) - seq_len(norder) + 1] = 1
  }
  knots = knots * (rangeval[2] - rangeval[1]) + rangeval[1]
  if (interior) {
    tk = length(knots)
    return(knots[(norder + 1):(tk - norder)])
  } else {
    return(knots)
  }
}

#' @rdname knot.design
#' @export
knot_design = knot.design

#' @rdname knot.design
#' @export
knotDesign = knot.design

#' @rdname knot.design
#' @export
KnotDesign = knot.design


arg_check_knot_design = function(nknots, norder, extend, interior) {
  if (length(nknots) != 1 | !is.numeric(nknots) | (nknots < 1)) {
    stop("nknots should be a positive integer value")
  }
  if (length(norder) != 1 | !is.numeric(norder) | (norder < 1)) {
    stop("norder should be a positive integer value")
  }
  if (length(extend) != 1 | !is.logical(extend)) {
    stop("extend should be a logical value")
  }
  if (length(interior) != 1 | !is.logical(interior)) {
    stop("extend should be a logical value")
  }
}



