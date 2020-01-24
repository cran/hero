## The following is an example of
## using fbps to estimate a bivariate function
## with regularly spaced observations
## choose proper directory where the code is located
set.seed(2)

##########################
#### True function   #####
##########################
n1 <- 60
n2 <- 80
x <- (1: n1)/n1-1/2/n1
z <- (1: n2)/n2-1/2/n2
MY <- array(0,c(length(x),length(z)))

sigx <- .3
sigz <- .4
for(i in 1: length(x))
  for(j in 1: length(z))
  {
    #MY[i,j] <- .75/(pi*sigx*sigz) *exp(-(x[i]-.2)^2/sigx^2-(z[j]-.3)^2/sigz^2)
    #MY[i,j] <- MY[i,j] + .45/(pi*sigx*sigz) *exp(-(x[i]-.7)^2/sigx^2-(z[j]-.8)^2/sigz^2)
    MY[i,j] = sin(2*pi*(x[i]-.5)^3)*cos(4*pi*z[j])
  }
##########################
#### Observed data   #####
##########################
sigma <- 1
Y <- MY + sigma * rnorm(n1*n2,0,1)
##########################
#### FBPS estimation  #####
##########################

est <- fbps(Y, list(x = x, z = z))

b1 = bspline(knots = est$xknots)
b2 = bspline(knots = est$zknots)
obj = prepare(Y, list(est$x, est$z), list(b1, b2))
gcv = loglambda2gcv(log(est$lambda), obj)

context("check loglambda2gcv")

test_that("loglambda2gcv correct", {
  expect_equal(est$gcv, gcv)
})

context("check enhance")

obj2 = enhance(obj, method = "nlminb")
gcv2 = loglambda2gcv(obj2$loglambda, obj)

test_that("enhance improves gcv", {
  expect_true(est$gcv >= gcv2)
})

context("check hero")

obj$loglambda = log(est$lambda)
sandmod = hero(obj)
test_that("smooth matches canonical", {
  expect_equivalent(sandmod$fitted, est$hatY)
  expect_equivalent(as.matrix(sandmod$coeff), est$Theta)
})
