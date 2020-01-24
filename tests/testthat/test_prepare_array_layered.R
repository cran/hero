## The following is an example of
## using FMPS to estimate a multivariate function
## with regularly spaced observations on three
## dimensional grid
## Sep 1, 2012
## Author: Luo Xiao at Cornell Univ.
## Article: Xiao, Li and Ruppert (2013 JRSSB)

context("test 3d functions")
set.seed(9)
#source("fmps.r")
#source("glam.r")
#source("pspline.setting.R")
##########################
#### True function   #####
##########################
d <- 3 ### 3D subjects
n <- c(128,128,24) ### dimension of data
x <- list(length = d) ### covariates
for(i in 1:d) x[[i]] <- (1:n[i])/n[i] - 1/2/n[i]

MY <- array(0,dim=n) ### true function
for(i in 1:n[1])
  for(j in 1:n[2])
    for(k in 1:n[3])
      MY[i,j,k] <- x[[1]][i]^2 + x[[2]][j]^2 + x[[3]][k]^2

##########################
#### Observed data   #####
##########################
sigma <- 0.5
Y <- MY + sigma * rnorm(n[1]*n[2]*n[3],0,1)

# create splines
splines = default.splines(x)
l = assemble(splines, x)

obj = prepare(Y, x = x, splines = splines)
obj2 = prepare(Y, x = x, splines = splines, assembed = l)
h1 = hero(obj)
h2 = hero(obj2)

context("test prepare.array with assembled precomputed")
test_that("test prepare.array with assembled precomputed", {
  expect_equal(h1, h2)
})

# library(microbenchmark)
# microbenchmark(obj1 <- prepare(Y, x = x, splines = splines),
#                obj2 <- prepare(Y, x = x, splines = splines,
#                                assembled = l)
# )

