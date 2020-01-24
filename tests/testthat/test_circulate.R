context("check circulate")

test_that("circulate works propertly", {
  x = 1:10
  expect_equivalent(circulate(x), c(2:10, 1))
  expect_equivalent(circulate(x, n = 2), c(3:10, 1:2))
  expect_equivalent(circulate(x, n = 3), c(4:10, 1:3))
})
