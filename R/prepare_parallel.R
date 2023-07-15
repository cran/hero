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
#' #'
#'
#' # function to read and process data
#' # get_data <- function(i, nco, wna) {
#' #   data = ncvar_get(nco, varid = "tasmax",
#' #                    start = c(1, 1, 365 * (i - ifelse(i < 57, 1, 57)) + 1),
#' #                    count = c(600, 258, 365), 365 * (i - 1) + 1,
#' #                    verbose = FALSE)
#' #   stdata = matrix(c(data), ncol = dim(data)[3])
#' #   stdata[-wna, ]
#' # }
#' get_data2 <- function(i) {
#'   readRDS(paste0("cordex_year_", i, ".rds"))
#' }
#'
#' prepare_parallel = function(import_list, herox, splines, assembled,
#'                             args.mpi.spawn.Rslaves = NULL, ...) {
#'   no_cores <- parallel::detectCores(logical = TRUE)
#'   cl <- parallel::makeCluster(no_cores - 1)
#'
#'   prepare_online =
#'     function(i,
#'              herox,
#'              splines,
#'              assembled) {
#'     prepare(get_data2(i), x = herox, splines = splines,
#'             assembled = assembled)[c("Ytilde", "sum_ysq")]
#'   }
#'
#'   k = prepare_online(1, herox, splines, assembled)
#'
#'   ta = Sys.time()
#'   out = lapply(seq_len(4),
#'                prepare_online,
#'                herox = herox,
#'                splines = splines,
#'
#'    tb = Sys.time()
#'    out2 = parallel::mclapply(seq_len(4),
#'                             prepare_online,
#'                             herox = herox,
#'                             splines = splines,
#'                             assembled = assembled,
#'                             mc.cores = 4)
#'    tb2 = Sys.time() - tb
#'    print(tb2)
#'
#'    cl <- parallel::makeCluster(4L)
#'    tc = Sys.time()
#'    out2 = parallel::parLapply(cl, X = seq_len(4),
#'                              fun = prepare_online,
#'                              herox = herox,
#'                              splines = splines,
#'                              assembled = assembled)
#'    tc2 = Sys.time() - tc
#'    print(tc2)
#'
#'    library(parallel)
#'    t4 = Sys.time()
#'    cl <- makeCluster(4L)
#'    clusterEvalQ(cl, {
#'      library(hero)
#'      get_data2 <- function(i) {
#'        readRDS(paste0("cordex_year_", i, ".rds"))
#'      }
#'      prepare_online =
#'        function(i,
#'                 herox,
#'                 splines,
#'                 assembled) {
#'          prepare(get_data2(i), x = herox, splines = splines,
#'                  assembled = assembled)[c("Ytilde", "sum_ysq")]
#'        }
#'      })
#'    out4 = parallel::parLapply(cl, X = seq_len(4),
#'                               fun = prepare_online,
#'                               herox = herox,
#'                               splines = splines,
#'                               assembled = assembled)
#'    stopCluster(cl)
#'   t4b = Sys.time() - t4
#'   print(t4b)
#'
#'   library(future)
#'   tc = Sys.time()
#'   future::plan(multicore)
#'   out3 = future.apply::future_lapply(seq_len(4),
#'                             prepare_online,
#'                             herox = herox,
#'                             splines = splines,
#'                             assembled = assembled)
#'   Sys.time() - tc
#'   length(out3)
#'
#'   options(future.globals.maxSize = +Inf)
#'   cl <- parallel::makeCluster(4L)
#'   plan(cluster, workers = cl)
#'   tc = Sys.time()
#'   out3 = future.apply::future_lapply(seq_len(4),
#'                                      prepare_online,
#'                                      herox = herox,
#'                                      splines = splines,
#'                                      assembled = assembled)
#'   tc2 = Sys.time() - tc
#'   print(tc2)
#'
#'   # Rmpi::mpi.close.Rslaves()
#'   # Ytilde[[i]] = prepare(stdata, x = x, splines = splines, assembled = a)$Ytilde
#'   # sum_ysq[i] = sum(stdata^2)
#' }
#'
#'
