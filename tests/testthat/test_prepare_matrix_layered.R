# prepare Lu et al. (2012) noisy f1 data
data(ludata)

# prepare stuff
splines = default.splines(list(x, z))
l = assemble(splines, x = list(x, z))

obj = prepare(lunoisyf1, x = list(x, z), splines = splines)
obj2 = prepare(lunoisyf1, x = list(x, z),
               splines = splines, assembled = l)
h = hero(obj)
h2 = hero(obj2)

context("test prepare.matrix with and without assembled")
test_that("prepare.matrix with and without assembled", {
  expect_equal(h, h2)
})
#
# library(microbenchmark)
# microbenchmark(
#   obj <- prepare(lunoisyf1, x = list(x, z),
#                  splines = splines),
#   obj2 <- prepare(lunoisyf1, x = list(x, z),
#                  splines = splines, assembled = l)
# )
