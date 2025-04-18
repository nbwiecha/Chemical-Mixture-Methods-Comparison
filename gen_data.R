# function to generate data for exposure methods simulations
# Author: Nate Wiecha, NCSU

gen_data <- function(n, p, eff_type, beta1, beta2=.3, rho, var1=1, random_fn=qexp, rho1=rho,
                     sd.y=2){
  
  # function to generate data for simulations
  # n is sample size
  # p is number of exposure variables
  # eff_type should be "linear", "nonlinear", or "interactive"
  # beta1 is reg. coef. of 1st exposure on y
  # beta2 is reg. coef. of 2nd exposure on y
  # rho is pairwise correlation parameter for exposures
  # var1 is variance of x1
  # random_fn is function to draw i.i.d. random variates from
  # rho1 is correlation between x1 and other exposure variables
  
  # define variance matrix
  R <- matrix(rho, p, p) + diag(rep(1-rho), p)
  R[2:p,1] <- rho1
  R[1, 2:p] <- rho1
  R[,1] <- R[,1]*sqrt(var1)
  R[1,] <- R[1,]*sqrt(var1)
  
  # generate exposure data
  # L <- chol(R)
  # X1 <- matrix(random_fn(n*p), nrow=n)
  # X <- X1 %*% L
  
  # using copula
  L <- chol(R)
  X1 <- matrix(rnorm(n*p), nrow=n)
  X2 <- X1 %*% L
  X.unif <- apply(X=X2, MARGIN=2, FUN=pnorm)
  X <- apply(X=X.unif, MARGIN=2, FUN=random_fn)
  
  # generate outcome data according to effect type
  if(eff_type=="linear"){
    y <- beta1 * X[,1] + beta2 * X[,2] + rnorm(n, sd=sd.y)
  }
  
  if(eff_type=="nonlinear"){
    y <- beta1 * X[,1]^2 + beta2 * X[,2]^2 + rnorm(n, sd=sd.y)
  }
  
  if(eff_type=="interactive"){
    y <- beta1 * X[,1]*X[,2] + rnorm(n, sd=sd.y)
  }
  
  if(eff_type=="sine"){
    y <- beta1 * sin(sqrt((X[,1])^2 + (X[,2])^2)*5) + rnorm(n, sd=sd.y)
  }
  
  out <- data.frame(y, X)
  colnames(out) <- c("y", paste("x", 1:p, sep=""))
  
  return(out)
  
}

gen_data_dense <- function(n, p, eff_type="linear", beta, rho, var1=1, random_fn=rexp, rho1=rho,
                     sd.y=2){
  
  # Unlike previous function, all elements of beta vector are the same and may be nonzero
  # Meant for linear effect only
  
  # function to generate data for simulations
  # n is sample size
  # p is number of exposure variables
  # eff_type should be "linear", "nonlinear", or "interactive"
  # beta1 is reg. coef. of 1st exposure on y
  # beta2 is reg. coef. of 2nd exposure on y
  # rho is pairwise correlation parameter for exposures
  # var1 is variance of x1
  # random_fn is function to draw i.i.d. random variates from
  # rho1 is correlation between x1 and other exposure variables
  
  # define variance matrix
  R <- matrix(rho, p, p) + diag(rep(1-rho), p)
  R[2:p,1] <- rho1
  R[1, 2:p] <- rho1
  R[,1] <- R[,1]*sqrt(var1)
  R[1,] <- R[1,]*sqrt(var1)
  
  # generate exposure data
  # L <- chol(R)
  # X1 <- matrix(random_fn(n*p), nrow=n)
  # X <- X1 %*% L
  
  # using copula
  L <- chol(R)
  X1 <- matrix(rnorm(n*p), nrow=n)
  X2 <- X1 %*% L
  X.unif <- apply(X=X2, MARGIN=2, FUN=pnorm)
  X <- apply(X=X.unif, MARGIN=2, FUN=qexp)
  
  # generate outcome data according to effect type
  beta_vec <- rep(beta, p)
  if(eff_type=="linear"){
    y <- X %*% beta_vec + rnorm(n, sd=sd.y)
  }
  
  if(eff_type=="nonlinear"){
    # y <- beta1 * X[,1]^2 + beta2 * X[,2]^2 + rnorm(n, sd=sd.y)
    stop("Function meant for linear effect only")
  }
  
  if(eff_type=="interactive"){
    # y <- beta1 * X[,1]*X[,2] + rnorm(n, sd=sd.y)
    stop("Function meant for linear effect only")
  }
  
  out <- data.frame(y, X)
  colnames(out) <- c("y", paste("x", 1:p, sep=""))
  
  return(out)
  
}
