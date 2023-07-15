#' Sequentially prepare data for sandwich smooth
#'
#' Sequentially prepare each observation for smoothing. It is assumed that each
#' observation resides in its own file and that \code{do.call(import_fun,
#' list(import_list[i]))} will import the data associated with observation
#' \code{i} into memory. The \code{import_fun} argument should be a function
#' after the style of \code{\link[base]{readRDS}}, where the object can be
#' assigned a name once it is read in. The \code{import_fun} argument should NOT
#' be like \code{\link[base]{load}}, where the object loaded has a preassigned
#' name.
#'
#' @param import_list A vector or list whose elements tell \code{import_fun}
#'   which files to import.
#' @param import_fun A function that will read each observation into memory
#'   based on the elements of \code{import_list}.
#' @param splines A list of spline-related objects. Each element of
#'   \code{splines} corresponds to the set of splines for the corresponding
#'   element of \code{x}.
#' @param ... Not implemented
#' @inheritParams create.prepared_list
#' @param package A character string indicating the package to use for the
#'   computations. The choices are \code{"base"}, \code{"parallel"},
#'   \code{"pbapply"}, \code{"future.apply"}, and \code{"Rmpi"}. The default is
#'   \code{"base"}, in which case a standard \code{for} loop is used. If
#'   \code{package == "parallel"}, then \code{\link[parallel]{mclapply}} is
#'   used, which is only appropriate when \code{mc.cores} is integer-valued or
#'   \code{NULL}. If \code{package == "pbapply"}, then
#'   \code{\link[pbapply]{pblapply}} is used, which automatically provides a
#'   progress bar. If \code{package == "future.apply"}, then
#'   \code{\link[future.apply]{future_lapply}} is used. If \code{package ==
#'   "Rmpi"}, then \code{\link[Rmpi]{mpi.applyLB}} is used.
#' @param call_args A named list providing relevant arguments to the
#'   \code{\link[parallel]{mclapply}}, \code{\link[pbapply]{pblapply}},
#'   \code{\link[future.apply]{future_lapply}}, or
#'   \code{\link[Rmpi]{mpi.applyLB}} depending on the value of \code{package}.
#' @return A \code{prepared_sequential} object
#' @author Joshua P. French
#' @seealso \code{\link{prepare}}, \code{\link[parallel]{mclapply}},
#'   \code{\link[pbapply]{pblapply}}, \code{\link[future.apply]{future_lapply}},
#'   \code{\link[Rmpi]{mpi.applyLB}}
#' @export
prepare_sequential = function(import_list, import_fun = base::readRDS,
                        x, splines, assembled, package = "base",
                        call_args = list(), ...) {
  # argument checking
  if (!is.vector(import_list) & !is.list(import_list)) {
    stop("import_list must be a vector list")
  }
  if (!is.function(import_fun)) { stop("import_fun must be a function")}
  if (length(package) != 1) { stop("package must be a single character string")}
  if (!is.character(package)) { stop("package must be a character string")}
  if (!is.element(package, c("base", "parallel", "pbapply", "future.apply", "Rmpi"))) {
    stop("package must be one of 'base', 'parallel', 'pbapply', 'future.apply', or 'Rmpi'")
  }

  # determine which function to call
  if (package == "parallel") {
    call_fun = parallel::mclapply
  } else if (package == "pbapply") {
    call_fun = pbapply::pblapply
  } else if (package == "future.apply") {
    call_fun = future.apply::future_lapply
    if(!requireNamespace("future.apply")) {
      message("future.apply must be installed to use this functionality. Using 'pbapply' package.")
    call_fun = pbapply::pblapply
    }
  } else if (package == "Rmpi") {
    call_fun = Rmpi::mpi.applyLB
    if(!requireNamespace("Rmpi")) {
      message("Rmpi must be installed to use this functionality. Using 'pbapply' package.")
      call_fun = pbapply::pblapply
    }
  } else if (package == "base"){
    call_fun = base::lapply
  } else {
    stop("package not implemented")
  }

  # save for future computations
  A = lapply(assembled, getElement, name = "A")

  # to return output
  out = vector("list", length(import_list))
  if (package == "base") {
    for(i in seq_along(import_list)) {
      out[[i]] = lprepare_slim(import_list[[i]],
                               import_fun = import_fun,
                               A = A)
    }
  } else {
    out = do.call(call_fun,
                  append(list(X = import_list,
                              FUN = lprepare_slim,
                              import_fun = import_fun,
                              A = A),
                         call_args))
  }
  # determine dimensions
  n = determine_n(x)
  # return formatted output
  structure(list(
    Ytilde = lapply(out, getElement, name = "Ytilde"),
    sum_ysq = unlist(lapply(out, getElement, name = "sum_ysq"), use.names = FALSE),
    n = n,
    s = lapply(assembled, getElement, name = "s"),
    B = lapply(assembled, getElement, name = "B"),
    Q = lapply(assembled, getElement, name = "Q"),
    A = A,
    U = lapply(assembled, getElement, name = "U"),
    loglambda = rep(0, length(n)),
    x = x
  ), class = "prepared_sequential")
}

# a function that takes each element of import_list,
# imports files based on import_fun, then prepares
# each observation
# lprepare =
#   function(i,
#            import_fun,
#            herox,
#            splines,
#            assembled) {
#     prepare(
#       do.call(import_fun, list(i)),
#       x = herox,
#       splines = splines,
#       assembled = assembled
#     )[c("Ytilde", "sum_ysq")]
#   }

# determine n for x when it is a list
determine_n = function(x) {
  n = numeric(length(x))
  for (i in seq_along(n)) {
    if (is.matrix(x[[i]])) {
      n[i] = nrow(x[[i]])
    } else {
      n[i]= length(x[[i]])
    }
  }
  return(n)
}

# a function that takes each element of import_list,
# imports data based on import_fun, then prepares
# each observation
lprepare_slim = function(i, import_fun, A) {
    prepare_slim(
      do.call(import_fun, list(i)),
      A = A)
}

prepare_slim = function(data, A) {
  list(
    Ytilde = rh.seq(A, data, transpose = TRUE),
    sum_ysq = sum(data^2)
  )
}
