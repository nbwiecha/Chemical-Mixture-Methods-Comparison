# Joint test functions for mixture simulations

# Need joint tests for several methods that may not really have it built in
# Author: Nate Wiecha, NCSU

library(mgcv)
library(spikeSlabGAM)
library(caret)
library(glmnet)

joint_gam_pip <- function(fit, burn=100){
  require(dplyr)
  # compute PIP for a variable in a spikeSlabGAM model
  # i.e., the PIP of ALL variables together
  # estimated as proportion of MCMC samples after burn-in where at least one 
  # variable is included
  
  # extract selection flags and put in dataframe
  gammas <- (fit$samples$gamma)
  nsamp <- nrow(gammas[[1]])
  gammas.noburn <- list()
  for(i in 1:length(gammas)){
    gammas.noburn[[i]] <- gammas[[i]][(burn+1):nsamp,]
    
  }
  gammas.df <- as.data.frame(do.call(rbind,gammas.noburn))
  # determine which MCMC samples select at least one variable
  inclusions <- apply(gammas.df, FUN=max, MARGIN=1)
  inclusions[inclusions<1] <- 0
  pip <- mean(inclusions)
  return(pip)
}

joint_bkmr_pip <- function(fit, burn){
  # joint PIP for BKMR: pip for ANY variable being included in model
  # extract variable selection flags and compute proportion where at least one
  # is > 0
  r.samples <- fit$r[burn:nrow(fit$r),]
  r.rowsums <- rowSums(r.samples)
  return(mean(r.rowsums > 0))
}

bkmr_contrast_test <- function(fit, burn){
  
  risks.overall <- OverallRiskSummaries(fit = fit, y = fit$y, Z = fit$Z, X = fit$X, 
                                        qs = c(0.75), 
                                        q.fixed = 0.25, method = "exact")
  ci <- as.numeric(c(risks.overall[2] - 1.96*risks.overall[3], risks.overall[2] + 1.96*risks.overall[3]))
  if(0 < ci[1] | 0 > ci[2]){
    return(1)
  }else{
      return(0)
    }
}

term_pip <- function(fit, var, burn=100){
  # estimate PIP for one exposure variable from a spike and slab GAM
  # necessary since spike and slab GAM decomposes function of a variable
  # into linear and smooth terms, so need to test those jointly to test
  # whether the term should be included
  require(dplyr)
  # compute PIP for a variable in a spikeSlabGAM model
  # i.e., the PIP of ANY term including the specified variable
  
  # extract selection flags into dataframe
  gammas <- (fit$samples$gamma)
  nsamp <- nrow(gammas[[1]])
  gammas.noburn <- list()
  for(i in 1:length(gammas)){
    gammas.noburn[[i]] <- gammas[[i]][(burn+1):nsamp,]
    
  }
  gammas.df <- as.data.frame(do.call(rbind,gammas.noburn))
  # select columns involving the term of interest
  var.cols <- select(gammas.df, contains(var))
  # compute proportion of samples where one of those columns has selection flag
  inclusions <- apply(var.cols, FUN=max, MARGIN=1)
  pip <- mean(inclusions)
  return(pip)
}

