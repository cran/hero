## using FMPS (Fast multivariate P-splines) to estimate a multivariate function
## with regularly spaced observations on d-dimensional grid (d>=3)
## Sep 1, 2012
## Author: Luo Xiao at Cornell Univ.
## Article: Xiao, Li and Ruppert (2013 JRSSB)

fmps.gcv <- function(x,s,n,ytilde,Y_sum){
## this function computes the value of GCV
## x: the values of smoothing parameters (log scale)
## s: the list of eigenvalues
## n: the dimension of data
## ytilde: the transformed data (see page 580, right before equation (10)  of the paper)
## Y_sum: ||Y||_F^2
d <- length(x)
lambda <- exp(x)

sigma <- 1/(1+lambda[1]*s[[1]])
for(i in 2:d)
sigma <- kronecker(1/(1+lambda[i]*s[[i]]),sigma)

sigma.2 <- sqrt(sigma)

gcv <- Y_sum + sum((ytilde*sigma)^2) - 2*sum((ytilde*sigma.2)^2)
Trace <- sum(1/(1+lambda[1]*s[[1]]))/n[1]
for(i in 2:d)
Trace <- Trace*sum(1/(1+lambda[i]*s[[i]]))/n[i]
gcv <- gcv/(1-Trace)^2
return(gcv)
}

## The following function gives the estimates for the
## FMPS estimator.
## returns: (1) smoothing parameters; (2) fitted values;
## (3) trace for the hat matrix; (4) GCV;
## (5) the parameter surface: Theta;

fmps.est =function(x,s,n,ytilde,Y_sum,Ytilde,Sigi.sqrt,U,Y, List){

d <- length(x)
lambda <- exp(x)

sigma <- 1/(1+lambda[1]*s[[1]])
for(i in 2:d)
sigma <- kronecker(1/(1+lambda[i]*s[[i]]),sigma)

sigma.2 <- sqrt(sigma)

gcv <- Y_sum + sum((ytilde*sigma)^2) - 2*sum((ytilde*sigma.2)^2)
Trace <- sum(1/(1+lambda[1]*s[[1]]))/n[1]
for(i in 2:d)
Trace <- Trace*sum(1/(1+lambda[i]*s[[i]]))/n[i]
gcv <- gcv/(1-Trace)^2

C <- list(length=d)
for(i in 1:d)
C[[i]] <- Sigi.sqrt[[i]]%*%U[[i]]%*% diag(1/(1+lambda[i]*s[[i]]))
Theta <- RH(C[[1]],Ytilde)
for(i in 2:d)
Theta <- RH(C[[i]],Theta)

Yhat <- Theta
for(i in 1:d)
Yhat <- RH(List[[i]]$B,Yhat)

result <- list(lambda=lambda,Yhat = Yhat, trace=Trace,gcv=gcv,Theta=Theta)
return(result)
}
