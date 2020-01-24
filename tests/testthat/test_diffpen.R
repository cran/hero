context("check diffpen")

test_that("diffpen matches diff.pen (or negative of it)", {
  nb = seq(10, 25, len = 4)
  mc = 1:5
  for (i in seq_along(nb)) {
    b = bspline(nbasis = nb[i])
    for (j in seq_along(mc)) {
      p1 = Matrix::crossprod(diffpen(b, m = mc[j], sparse = FALSE))
      p2 = crossprod(difference.penalty(m = mc[j], K = b$nbasis))
      expect_equivalent(as.matrix(p1), p2)
    }
  }
})
