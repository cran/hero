# prepare data
set.seed(10)
n1 = 10
n2 = 20

evalarg1 = seq(0, 1, len = n1)
evalarg2 = seq(0, 1, len = n2)

# construct "true" data
mu = matrix(0, nrow = n1, ncol = n2)
for (i in seq_len(n1)) {
  for (j in seq_len(n2)) {
    mu[i, j] =  sin(2 * pi * (evalarg1[i] - .5) ^ 3) * cos(4 * pi * evalarg2[j])
  }
}
# construct noisy data (matrix, then array)
datam <- mu + rnorm(n1 * n2)
dataa <- as.array(datam)

prepm = prepare(datam)
prepa = prepare(dataa)

context("does prepare.array match prepare.matrix")

test_that("prepare.array matches prepare.matrix", {
  expect_equivalent(prepm, prepa)
})

