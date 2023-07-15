context("test knot design")

if(requireNamespace("fda", quietly = TRUE)) {
test_that("knot.design matches create.bspline.basis", {
  o = 1:5
  nb = seq(10, 50, len = 6)
  for (i in seq_along(o)) {
    for (j in seq_along(nb)) {
      # range c(0, 1)
      a = knot.design(nbasis = nb[j], norder = o[i], extend = FALSE, interior = TRUE)
      b = fda::create.bspline.basis(nbasis = nb[j], norder = o[i])$params
      expect_equal(a, b)
      # if (!all.equal(a, b)) stop("a != b")

      # random range
      rangeval = sort(runif(2, 3, 12))
      a = knot.design(rangeval, nbasis = nb[j], norder = o[i], extend = FALSE, interior = TRUE)
      b = fda::create.bspline.basis(rangeval, nbasis = nb[j], norder = o[i])$params
      # if (!all.equal(a, b)) stop("a != b")
      expect_equal(a, b)
    }
  }
})
}


