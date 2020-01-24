context("check rh.seq")

# generate x, a
x = list(matrix(rnorm(100), nrow = 10),
         matrix(rnorm(100), nrow = 10))
a = matrix(rnorm(100), nrow = 10)

# three equivalent forms
rhs1 = rh.seq(x, a)
rhs2 = rh(x[[2]], rh(x[[1]], a))
rhs3 = x[[1]] %*% a %*% t(x[[2]])

test_that("rh.seq correct", {
  expect_equal(rhs1, rhs2)
  expect_equal(rhs1, rhs3)
})
