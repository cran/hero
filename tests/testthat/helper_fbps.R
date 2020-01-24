fbps <-function(data, covariates = NULL, knots=35, p=3,m=2,lambda=NULL, search.grid = T, search.length = 100, method="L-BFGS-B",lower= -20, upper=20, control=NULL){

  # return a smoothed matrix using fbps

  # data: a matrix
  # covariates: the list of data points for each dimension
  # knots: to specify either the number/numbers of  knots  or the vector/vectors of knots for each dimension; defaults to 35
  # p: the degrees of B-splines
  # m: the order of difference penalty
  # lambda: the user-selected smoothing parameters
  # method: see "optim"
  # lower, upper, control: see "optim"

  # library(splines)
  #library(fBasics)
  # source("pspline.setting.R")

  ## data dimension
  data_dim = dim(data)
  n1 = data_dim[1]
  n2 = data_dim[2]

  ## covariates for the two axis
  if(!is.list(covariates)) {

    x=(1:n1)/n1-1/2/n1; ## if NULL, assume equally distributed
    z = (1:n2)/n2-1/2/n2
  }
  if(is.list(covariates)){

    x = covariates[[1]]
    z = covariates[[2]]
  }

  ## B-spline basis setting
  p1 = rep(p,2)[1]
  p2 = rep(p,2)[2]
  m1 = rep(m,2)[1]
  m2 = rep(m,2)[2]

  ## knots
  if(!is.list(knots)){

    K1 = rep(knots,2)[1]
    xknots=seq(-p1,K1+p1,length=K1+1+2*p1)/K1
    xknots = xknots*(max(x)-min(x)) + min(x)
    K2 = rep(knots,2)[2]
    zknots=seq(-p2,K2+p2,length=K2+1+2*p2)/K2
    zknots = xknots*(max(z)-min(z)) + min(z)

  }

  if(is.list(knots)){

    xknots = knots[[1]]
    K1 = length(xknots)-2*p1-1
    zknots= knots[[2]]
    K2 = length(zknots)-2*p2-1
  }
  #######################################################################################
  Y = data

  ###################   precalculation for fbps smoothing  ##########################################66

  List = pspline.setting(x,xknots,p1,m1)
  A1 = List$A
  s1 = List$s
  Sigi1_sqrt = List$Sigi.sqrt
  U1 = List$U

  ### Added for checking
  tUPU1 = List$tUPU
  B1 = List$B
  P1 = List$P
  Esig1 = List$Esig

  ###

  List = pspline.setting(z,zknots,p2,m2)
  A2 = List$A
  s2 = List$s
  Sigi2_sqrt = List$Sigi.sqrt
  U2 = List$U

  ### Added for checking
  tUPU2 = List$tUPU
  B2 = List$B
  P2 = List$P
  Esig2 = List$Esig

  ###


  ##B1 and B2 are the B-spline design matrices
  B1 = splines::spline.des(knots=xknots, x=x, ord = p1+1,outer.ok = TRUE)$design
  B2 = splines::spline.des(knots=zknots, x=z, ord = p2+1,outer.ok = TRUE)$design
  #################select optimal penalty ####################################

  tr <-function(A){ return(sum(diag(A)))} ## the trace of a square matrix

  Ytilde = t(as.matrix(A1))%*%Y%*%as.matrix(A2)
  Y_sum = sum(Y^2)
  ytilde = as.vector(Ytilde)

  fbps_gcv =function(x){

    lambda=exp(x)
    ## two lambda's are the same
    if(length(lambda)==1)
    {
      lambda1 = lambda
      lambda2 = lambda
    }
    ## two lambda's are different
    if(length(lambda)==2){
      lambda1=lambda[1]
      lambda2=lambda[2]
    }


    sigma = kronecker(1/(1+lambda2*s2),1/(1+lambda1*s1))
    sigma.2 = sqrt(sigma)

    gcv = Y_sum + sum((ytilde*sigma)^2) - 2*sum((ytilde*sigma.2)^2)
    trace = sum(1/(1+lambda1*s1))*sum(1/(1+lambda2*s2))
    gcv = gcv/(1-trace/(n1*n2))^2
    return(gcv)
  }

  fbps_est =function(x){

    lambda=exp(x)
    ## two lambda's are the same
    if(length(lambda)==1)
    {
      lambda1 = lambda
      lambda2 = lambda
    }
    ## two lambda's are different
    if(length(lambda)==2){
      lambda1=lambda[1]
      lambda2=lambda[2]
    }

    sigma = kronecker(1/(1+lambda2*s2),1/(1+lambda1*s1))
    sigma.2 = sqrt(sigma)

    gcv = Y_sum + sum((ytilde*sigma)^2) - 2*sum((ytilde*sigma.2)^2)
    trace = sum(1/(1+lambda1*s1))*sum(1/(1+lambda2*s2))
    gcv = gcv/(1-trace/(n1*n2))^2

    Theta = Sigi1_sqrt%*%U1%*%diag(1/(1+lambda1*s1))%*%Ytilde
    Theta = Theta%*%diag(1/(1+lambda2*s2))%*%t(U2)%*%Sigi2_sqrt
    hatY = B1%*%Theta%*%t(B2)
    result=list(lambda=c(lambda1,lambda2),hatY=hatY,trace=trace,gcv=gcv,Theta=Theta)
    return(result)
  }

  if(is.null(lambda)){

    if(search.grid ==T){

      Lambda = seq(lower,upper,length = search.length)
      lambda.length = length(Lambda)
      GCV = matrix(0,lambda.length,lambda.length)
      for(j in 1:lambda.length)
        for(k in 1:lambda.length){
          GCV[j,k] = fbps_gcv(c(Lambda[j],Lambda[k]))
        }
      location = which(GCV==min(GCV))
      j0 = location%%lambda.length
      if(j0==0) j0 = lambda.length
      k0 = (location-j0)/lambda.length+1
      lambda = exp(c(Lambda[j0],Lambda[k0]))
    } ## end of search.grid

    if(search.grid == F){
      fit = stats::optim(0,fbps_gcv,method=method,control=control,
                  lower=rep(lower,2)[1],upper=rep(upper,2)[1])

      fit = stats::optim(c(fit$par,fit$par),fbps_gcv,method=method,control=control,
                  lower=rep(lower,2)[1:2],upper=rep(upper,2)[1:2])
      if(fit$convergence>0) {
        expression = paste("Smoothing failed! The code is:",fit$convergence)
        print(expression)
      }
      lambda = exp(fit$par)
    } ## end of optim

  } ## end of finding smoothing parameters
  lambda = rep(lambda,2)[1:2]

  # return(fbps_est(log(lambda)))
  out = fbps_est(log(lambda))
  out$x = x
  out$xknots = xknots
  out$m1 = m1
  out$p1 = p1
  out$z = z
  out$zknots = zknots
  out$m2 = m2
  out$p2 = p2
  out$Ytilde = Ytilde
  out$ytilde = ytilde
  out$Y_sum = Y_sum
  out$s1 = s1
  out$s2 = s2
  out$Sigi1_sqrt = Sigi1_sqrt
  out$Sigi2_sqrt = Sigi2_sqrt
  out$A1 = A1
  out$A2 = A2
  out$tUPU1 = tUPU1
  out$B1 = B1
  out$P1 = P1
  out$tUPU2 = tUPU2
  out$B2 = B2
  out$P2 = P2
  out$U1 = U1
  out$U2 = U2
  out$Esig1 = Esig1
  out$Esig2 = Esig2
  ###

  return(out)
}
