context("check rh")

test_that("rh matches RH", {
  c1 = 10
  c2 = 11
  c3 = 12
  a = array(rnorm(c1 * c2 * c3), dim = c(c1, c2, c3))
  r = 15
  x = matrix(rnorm(r * c1), nrow = r)
  expect_equal(rh(x, a), RH(x, a))

  c1 = 6
  c2 = 5
  c3 = 5
  c4 = 7
  a = array(rnorm(c1 * c2 * c3 * c4), dim = c(c1, c2, c3, c4))
  r = 14
  x = matrix(rnorm(r * c1), nrow = r)
  expect_equal(rh(x, a), RH(x, a))
})
