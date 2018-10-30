#' @name ludata
#' @aliases x z lutruef1 lunoisyf1
#' @title Data for f1 function from Lu et al. (2012)
#' @description Data related to the f1 function in
#' Lu et al. (2012).  Define \code{n1 = 60} and
#' \code{x = seq_len(n1)/n1 - 1/2/n1}.  Similarly,
#' define \code{n2 = 80} and
#' \code{z = seq_len(n2)/n2 - 1/2/n2}.  The f1 function is defined as
#' \code{sin(2 * pi * (x[i] - .5) ^ 3) * cos(4 * pi * z[j])},
#' where \code{i in seq_along(x)} and
#' \code{j in seq_along(z)}.  The result of this function
#' is stored in \code{lutruef1}.  Using \code{set.seed(3)}
#' and adding \code{rnorm(60 * 80)} to \code{lutruef1}
#' results in \code{lunoisyf1}.
#' @docType data
#' @usage data(ludata)
#' @format The sequences \code{x} and \code{z}, the
#' \code{lutruef1} data matrix with 60 rows and 80 columns,
#' and the \code{lunoisyf1} data matrix with 60 rows and 80
#' columns.
NULL

