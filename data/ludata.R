# generate ludata

# observed data locations
n1 = 60
n2 = 80

x = seq_len(n1)/n1 - 1/2/n1
z = seq_len(n2)/n2 - 1/2/n2

# construct "true" data
lutruef1 = matrix(0, nrow = 60, ncol = 80)
for (i in seq_len(60)) {
  for (j in seq_len(80)) {
    lutruef1[i, j] =  sin(2 * pi * (x[i] - .5) ^ 3) *
      cos(4 * pi * z[j])
  }
}

# add noise to "true" data
set.seed(3)
lunoisyf1 = lutruef1 + stats::rnorm(60 * 80)
rm(n1)
rm(n2)
rm(i)
rm(j)


