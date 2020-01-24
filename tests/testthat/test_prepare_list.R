set.seed(9)
dat = lapply(1:3, function (i) generate.data3d())
x = dat[[1]]$x
data = lapply(dat, getElement, name = "data3d")
obj = prepare(data, x = x)
h = hero(obj)
e = loglambda2gcv(rep(0, length(x)), obj)

obj.list = lapply(data, prepare, x = x)
h.list = lapply(obj.list, hero)
e.list = sapply(seq_along(dat), function(i) {
  loglambda2gcv(rep(0, length(x)), obj.list[[i]])
})

context("match results for prepare.list, hero.list")
test_that("hero.list match, array", {
  for (i in seq_along(h.list)) {
    expect_equal(h.list[[i]]$coeff, h[[i]]$coeff)
    expect_equal(h.list[[i]]$coeff, h[[i]]$coeff)
  }
})

context("match results for loglambda2gcv.list")
test_that("loglambda2gcv.list match loglambda2gcv", {
  expect_equal(sum(e.list), e)
})

e2 = enhance(obj)

context("enhance works on prepared_list")
test_that("enhance better than loglambda2gcv for prepared_list", {
  expect_true(e2$results$value <= e)
})

