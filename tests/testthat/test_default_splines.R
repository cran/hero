context("test default.splines")

n1 = 25
n2 = 40 * 4
n3 = 15

s1 = bspline(nknots = ceiling(n1/4))
s2 = bspline(nknots = 35)
s3 = bspline(nknots = ceiling(n3/4))

data = array(rnorm(n1 * n2 * n3), dim = c(n1, n2, n3))

evalargs = default.evalargs(data)
splines = default.splines(evalargs)
test_that("default.splines works correctly", {
  expect_equivalent(s1, splines[[1]])
  expect_equivalent(s2, splines[[2]])
  expect_equivalent(s3, splines[[3]])
})

