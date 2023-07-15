#' Manually create a \code{prepared_list}
#'
#' \code{create.prepared_list} creates a
#' \code{prepared_list} manually.  Typically, one would
#' simply use the \code{\link{prepare.list}}, but there are
#' situations where the \code{data} argument would be too
#' large to read into memory. \cr This function assumes that
#' the user has used the \code{\link{assemble}} function to
#' construct a list of the relevant \code{assembled_splines}
#' and manually computed \code{Ytilde} for a number of
#' relevant \code{data} observations and stored them in a
#' list. The user should also manually compute the sum of
#' the squared \code{data} for each \code{data} observation.
#' The user must also specify the dimensions of each data
#' set (which are assumed to be the same) as a vector and
#' provide the relevant set of values at which each
#' \code{data} object is observed.  See Examples.
#'
#' @param assembled A list of \code{assembled_splines}.  See
#'   Examples.
#' @param x The list of arguments at which to evaluate each
#'   of the splines used to construct \code{assembled}.
#' @param Ytilde A list of \code{prepared_*} objects.
#' @param sum_ysq A vector with the sum of squared \code{data}
#'   objects used to construct \code{Ytilde}.
#' @param n The dimensions of the \code{data} objects used
#'   to construct \code{Ytilde}.
#'
#' @return A prepared list.
#' @export
#'
#' @examples
#' # generate and prepare 3d data
#' set.seed(9)
#' dat = generate.data3d()
#'
#' # list giving the locations to evaluate the basis functions
#' x = dat$x
#' # construct a set of basic B-splines for each dimension
#' splines = default.splines(x)
#'
#' # construct assembled splines from splines list
#' a = assemble(splines, x)
#'
#' # imagine there are 4 data obsevations we want to smooth
#' # but that they can't be loaded into memory
#' Ytilde = vector("list", 4)
#' sum_ysq = numeric(4)
#'
#' # prepare each data set manually
#' # notice the use of the assembled arguments so that
#' # the splines are not "assembled" again for each data set
#' for(i in seq_along(Ytilde)) {
#'     data = generate.data3d()$data3d
#'     Ytilde[[i]] = prepare(data, x = x, splines = splines,
#'                           assembled = a)
#'     sum_ysq[i] = sum(data^2)
#' }
#' n = dim(data)
#' p = create.prepared_list(assembled = a, x = x,
#'                          Ytilde = Ytilde, sum_ysq = sum_ysq,
#'                          n = n)
create.prepared_list = function(assembled, x, Ytilde, sum_ysq, n) {
  structure(list(
    Ytilde = Ytilde,
    sum_ysq = sum_ysq,
    n = n,
    s = lapply(assembled, getElement, name = "s"),
    B = lapply(assembled, getElement, name = "B"),
    Q = lapply(assembled, getElement, name = "Q"),
    A = lapply(assembled, getElement, name = "A"),
    U = lapply(assembled, getElement, name = "U"),
    loglambda = rep(0, length(n)),
    x = x
  ), class = "prepared_list")
}
