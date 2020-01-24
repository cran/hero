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
time = proc.time()
##########################
#### FBPS  Settings   #####
##########################
## number of knots
K <- rep(0,d) ### number of knots
for(i in 1:d)
  K[i] <- min(n[i]/2,35)

#### cubic splines and second order penalty
p <- rep(3,d)
m <- rep(2,d)

List <- list(length = d)
for(i in 1:d)
  List[[i]] <- pspline.setting(x[[i]],K[i],p[i],m[i])

s <- list(length = d)
U <- list(length = d)
Sigi.sqrt <- list(length = d)
Ytilde <- Y
for(i in 1:d) {
  s[[i]] <- List[[i]]$s
  U[[i]] <- List[[i]]$U
  Sigi.sqrt[[i]] <- List[[i]]$Sigi.sqrt
  Ytilde <- RH(t(as.matrix(List[[i]]$A)), Ytilde)
}
Y_sum = sum(Y^2)
ytilde = as.vector(Ytilde)


##########################
#### FMPS estimation  #####
##########################

### step 1: search for smoothing parameters
Lambda = seq(-10,10,length.out=20)
lambda_length = length(Lambda)

lambda_length = rep(lambda_length,d)
Lambda = list("1"=Lambda, "2"=Lambda, "3"=Lambda)
# GCV = array(0,dim = lambda_length)
# for(i in 1:lambda_length[1])
# {
#   print(i)
#   for(j in 1:lambda_length[2])
#     for(k in 1:lambda_length[3])
#     {
#       lambda = c(Lambda[[1]][i],Lambda[[2]][j],Lambda[[3]][k])
#       GCV[i,j,k] = fmps.gcv(lambda,s,n,ytilde,Y_sum)
#     }
# }
# location = which(GCV==min(GCV))
# print(location)
# proc.time()-time
#
# ### step 2: find the location of the smoothing parameters
# temp <- location%%(lambda_length[1]*lambda_length[2])
# if(temp==0){ temp <- lambda_length[1]*lambda_length[2]}
# k <- (location - temp)/(lambda_length[1]*lambda_length[2]) + 1
# ##
# temp1 <- temp%%lambda_length[1]
# if(temp1==0) { temp1 <- lambda_length[1]}
# j <- (temp - temp1)/lambda_length[1] + 1
# ##
# i <- temp1
# print(c(i,j,k))
i = 15; j = 16; k = 11
lambda <- c(Lambda[[1]][i],Lambda[[2]][j],Lambda[[3]][k])
## step 2: obtain estimate
est <- fmps.est(lambda,s,n,ytilde,Y_sum,Ytilde,Sigi.sqrt,U,Y, List)
Yhat <- est$Yhat

# create splines
splines = vector("list", length(List))
for (i in seq_along(splines)) {
  splines[[i]] = bspline(knots = List[[i]]$knots)
}

# # for some reason this has to be called for the code to work
# context("dumb")
# test_that("dumb test", {
#   expect_true(d < 4)
# })

obj = prepare(Y, x = x, splines = splines)

obj2 = enhance(obj)
gcv2 = loglambda2gcv(log(est$lambda), obj)
obj$loglambda = log(est$lambda)
sandmod = hero(obj)

test_that("functions work for 3d array", {
  expect_equal(est$gcv, gcv2)
  expect_true(est$gcv >= obj2$results$value)
  expect_equal(est$Yhat, sandmod$fitted)
})
