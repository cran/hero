#' @rdname assemble
#' @export
assemble.list =
  function(object, x, m = 2, sparse = TRUE,
           spdiffpen = TRUE, digits = 1, ...) {
    if (length(m) == 1) {
      m = rep(m, length(object))
    }
    d = length(x)
    assembled = lapply(seq_len(d), function(i) {
      assemble(
        object = object[[i]],
        x = x[[i]],
        m = m[i],
        sparse = sparse,
        spdiffpen = spdiffpen,
        digits = digits
      )
    })
  }
