#' #' Prepare data for sandwich smooth with mpi
#' #'
#' #' A generic function to prepare various types of data.  See
#' #' the functions linked in See Also.
#' #'
#' #' @param data The data to prepare
#' #' @param ... Not implemented
#' #'
#' #' @return A prepared object
#' #' @seealso \code{\link{prepare.numeric}},
#' #' \code{\link{prepare.matrix}},
#' #' \code{\link{prepare.array}},
#' #' \code{\link{prepare.sts}},
#' #' \code{\link{prepare.starray}}
#' #' @export
#' prepare_mpi = function(import_list, import_function = base::readRDS,
#'                         x, splines, assembled, args.mpi.spawn.Rslaves = NULL, ...) {
#'   if (!require(Rmpi)) {
#'     stop("Rmpi must be installed to use this function")
#'   }
#'   if (is.null(args.mpi.spawn.Rslaves)) {
#'     Rmpi::mpi.spawn.Rslaves(nslaves = mpi.universe.size() - 1)
#'   } else {
#'     do.call(Rmpi::mpi.spawn.Rslaves, args.mpi.spawn.Rslaves)
#'   }
#'
#'   prepare_online =
#'     function(i,
#'             import_function,
#'             x,
#'             splines,
#'             assembled) {
#'     tdata = do.call(import_function, list(i))
#'     prepared_object = prepare(tdata, x = x, splines = splines,
#'                               assembled = assembled)[c("Ytilde", "sum_ysq")]
#'     prepared_object$n = dim(tdata)
#'     return(prepared_object)
#'   }
#'
#'   out = Rmpi::mpi.iapplyLB(import_list, prepare_online,
#'                             import_function = import_function,
#'                             x = x,
#'                             splines = splines,
#'                             assembled = assembled,
#'                             job.num = length(import_list))
#'   Rmpi::mpi.close.Rslaves()
#'   # Ytilde[[i]] = prepare(stdata, x = x, splines = splines, assembled = a)$Ytilde
#'   # sum_ysq[i] = sum(stdata^2)
#' }
#'
#'
