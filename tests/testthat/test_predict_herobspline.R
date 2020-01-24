context("predict.hero_bspline")
nb = seq(10, 25, len = 4)
o = 1:6
test_that("compare predict.hero_bspline to predict.fd", {
  for (i in seq_along(nb)) {
    for (j in seq_along(i)) {
      b1 = bspline(nbasis = nb[i], norder = o[j])
      b2 = fda::create.bspline.basis(nbasis = nb[i], norder = o[j])
      s = seq(0, 1, len = 13)
      p1 = predict(b1, newx = s)
      p2 = predict(b2, newdata = s)
      expect_equivalent(as.matrix(p1), p2)
#
#       pd1 = predict(b1, newdata = s, nderiv = o[j] - 1)
#       pd2 = predict(b2, newdata = s, nderiv = o[j] - 1)
#       expect_equivalent(as.matrix(pd1), pd2)
    }
  }}
)
