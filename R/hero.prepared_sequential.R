#' @rdname hero
#' @param export_list A vector or list whose elements tell \code{export_fun}
#'   what files to export. The length must match the number of observations,
#'   i.e., the number of elements in \code{x$Ytilde}
#' @param export_fun A function that will write the results for each observation
#'   to file using the names in \code{export_list}. Must only have the arguments
#'   \code{object}, which is what will be saved and computed internally, and
#'   \code{file}, which is the name of the file that will be saved. \code{file}
#'   will be one of the elements of \code{export_list}.
#' @param package A character string indicating the approach to use for the
#'   computations. The choices are \code{"base"}, \code{"parallel"},
#'   \code{"pbapply"}, \code{"future.apply"}, or \code{"Rmpi"}. The default is
#'   \code{"base"}. If \code{package == "base"}, then \code{\link[base]{mapply}}
#'   is used. If \code{package == "parallel"}, then
#'   \code{\link[parallel]{mcmapply}} is used. If \code{package == "pbapply"},
#'   then \code{\link[pbapply]{pblapply}} is used. If code{package ==
#'   "future.apply"}, then \code{\link[future.apply]{future_mapply}} is used. If
#'   code{package == "Rmpi"}, then \code{\link[Rmpi]{mpi.applyLB}} is used.
#' @param call_args A named list providing relevant arguments to
#'   \code{\link[parallel]{mcmapply}}, \code{\link[pbapply]{pblapply}},
#'   \code{\link[future.apply]{future_mapply}}, or
#'   \code{\link[Rmpi]{mpi.applyLB}}, depending on the \code{package} choice.
#' @export
hero.prepared_sequential = function(x, ...,
                                    export_list,
                                    export_fun = base::saveRDS,
                                    package = "base",
                                    call_args = list()) {
  arglist = list(...)
  if (length(package) != 1) { stop("package must be a single character string")}
  if (!is.character(package)) { stop("package must be a character string") }
  if (!is.element(package, c("base", "parallel", "pbapply", "future.apply", "Rmpi"))) {
    stop("package must be one of 'base', 'parallel', 'pbapply', 'future.apply', or 'Rmpi'")
  }
  if (!is.list(call_args)) stop("call_args must be a list")
  if (length(x$Ytilde) != length(export_list)) {
    stop("The number of elements in x$Ytilde does not match the number of elements in export_list.")
  }

  lambda = exp(x$loglambda)
  parts = lapply(seq_along(lambda), function(i) {
    x$Q[[i]] %*% x$U[[i]] %*% diag(1/(1 + lambda[i]*x$s[[i]]))
  })

  # determine which function to call
  if (package == "base") {
    # function to call for parallelization
    call_fun = base::mapply
  } else if (package == "parallel") {
    call_fun = parallel::mcmapply
  } else if (package == "pbapply") {
    call_fun = pbapply::pblapply
  } else if (package == "future.apply") {
    call_fun = future.apply::future_mapply
    if(!requireNamespace("future.apply")) {
      message("future.apply must be installed to use this functionality. Using 'pbapply' package.")
      package = "pbapply"
    }
  } else if (package == "Rmpi") {
    call_fun = Rmpi::mpi.applyLB
    if(!requireNamespace("Rmpi")) {
      message("Rmpi must be installed to use this functionality. Using 'pbapply' package.")
      package = "pbapply"
    }
  } else {
    stop("That package isn't implemented")
  }

  # function to call for computations
  myFUN = ifelse(!is.element(package, c("pbapply", "Rmpi")),
                 coeff_fitted_save_mapply,
                 coeff_fitted_save_lapply)
  if (is.element(package, c("pbapply", "Rmpi"))) {
    # need to add filename as attribute for lapply style
    for(i in seq_along(x$Ytilde)) {
      attr(x$Ytilde[[i]], "file_out") = export_list[[i]]
    }
  }

  # call desired implementation of mapply or lapply
  if (!is.element(package, c("pbapply", "Rmpi"))) {
    do.call(call_fun,
            append(list(FUN = myFUN,
              Y.tilde = x$Ytilde,
              file_out = export_list,
              MoreArgs = list(export_fun = export_fun, parts = parts, B = x$B)),
              call_args)
            )
  } else {
    do.call(call_fun,
            append(list(X = x$Ytilde,
                        FUN = myFUN,
                        export_fun = export_fun,
                        parts = parts,
                        B = x$B),
                   call_args)
    )
  }
}

# helper function to compute coeffs and fitted values and export results to file
coeff_fitted_save_mapply = function(Y.tilde, file_out, export_fun, parts, B) {
  coeffs = rh.seq(parts, Y.tilde)
  do.call(what = export_fun,
          args = list(object = list(coeffs = coeffs, fitted = rh.seq(B, coeffs)),
                      file = file_out)
  )
}

coeff_fitted_save_lapply = function(Y.tilde, export_fun, parts, B) {
  coeffs = rh.seq(parts, Y.tilde)
  do.call(what = export_fun,
          args = list(object = list(coeffs = coeffs, fitted = rh.seq(B, coeffs)),
                      file = attr(Y.tilde, "file_out"))
  )
}
