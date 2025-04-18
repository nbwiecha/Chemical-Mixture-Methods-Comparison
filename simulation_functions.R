# Simulation functions
# Functions to run a simulation, based on parameters of scenario
# parameters are:
# n: sample size
# p: number of exposure variables
# rho: pairwise correlation between all pairs of exposures
# eff_type: data generating process, linear, nonlinear, or interactive
# beta_seq: sequence of beta values to use in data generating process
# B: number of monte carlo iterations
# hpc: whether running on HPC
# errorhandling: whether to stop entire simulation if encounter an error ("stop")
# or remove that result from the output ("remove")
# gam.big: whether to include a joint test with frequentist GAM, including all exposures in one smooth term
# bkmr.iter: since BKMR is slowest method, how many MCMC iterations to use

# Author: Nate Wiecha, NCSU

# edited 11/28/2023 for poster. Combining simulations into one function, run both sets of method on same data set.
# also add MSE estimation.
packages <-  c("mgcv", "bkmr", "spikeSlabGAM", 'caret', 'glmnet', 'gWQS', "qgcomp", "dplyr")

run_simulation_combined <- function(n=100, np=n, p, rho, eff_type, beta1_seq, beta2_seq, B, hpc, 
                                    errorhandling="stop", gam.big, bkmr.iter=500, bkmr.knots=NULL,
                                    bkmr.exact=TRUE, random_fn=qexp){
  require(doSNOW)
  require(doParallel)
  require(foreach)
  
  # initialize output
  out <- matrix(nrow=1, ncol=32+1)
  colnames(out) <- c("beta", "vs.GAM_HT", "vs.GAM_VS", "vs.BKMR_50", "vs.BKMR_95", "vs.SSGAM_50", "vs.SSGAM_95", "vs.ENET", "vs.ENET_alpha", "vs.OLS", "idx.GAM_HT_term", "idx.GAM_HT_big", "idx.GAM_HT_F", "idx.GAM_VS",
                           "idx.BKMR_CON", "idx.BKMR_H_50", "idx.BKMR_H_95", "idx.SSGAM_50", 
                           "idx.SSGAM_95", "idx.ENET", "idx.ENET_alpha",
                           "idx.PCR", "idx.WQS_POS", "idx.WQS_NEG", "idx.QGC", "idx.TEV", "idx.OLS",
                           "mse.GAM.add", "mse.GAM.big", "mse.BKMR", "mse.SSGAM", "mse.ENET", "mse.OLS")
  
  
  # define function to run one simulation iteration
  run_sim_iter <- function(n, p, eff_type, beta1, beta2, rho, hpc, gam.big, bkmr.iter, bkmr.knots,
                           bkmr.exact, seed, random_fn=qexp){
    if(!hpc){
      source("code/fit_all.R", local = TRUE)
      source("code/gen_data.R", local=TRUE)
      source("code/joint_tests.R", local=TRUE)
      source("code/RVeels.R") # function to run TEV
      
    }else{
      source("fit_all.R", local = TRUE)
      source("gen_data.R", local=TRUE)
      source("joint_tests.R", local=TRUE)
      source("RVeels.R") # function to run TEV
      
    }
    set.seed(seed)
    # generate data
    data <- gen_data(n=n, p=p, eff_type=eff_type, beta1=beta1, beta2=beta2, rho=rho)
    
    # generate data used to evaluate MSE
    data.pred <- gen_data(n=np, p=p, eff_type=eff_type, beta1=beta1, beta2=beta2, rho=rho,
                          sd.y=0)
    
    # plug in generated data to fitting function that returns results
    out.iter <- fit_all(data, data.pred, gam.big, bkmr.iter, bkmr.knots, bkmr.exact)
    
    return(out.iter)
  }
  
  # run B monte carlo iterations for each value of beta in the sequence provided
  for(i in 1:length(beta1_seq)){
    beta1 <- beta1_seq[i]
    beta2 <- beta2_seq[i]
    
    # run simulation in parallel for this value of beta
    out_beta <- foreach(b=1:B,
                          .packages=packages,
                          .combine=rbind,
                          .errorhandling=errorhandling) %dopar% 
      run_sim_iter(n, p, eff_type, beta1, beta2, rho, hpc, gam.big=gam.big, bkmr.iter=bkmr.iter, 
                   bkmr.knots=bkmr.knots, bkmr.exact=bkmr.exact, seed=i*b, random_fn=random_fn)
    
    
    # output matrix of all binary indicators of whether x1 was selected by 
    # each method in each monte carlo iteration
    out <- rbind(out, cbind(beta1, out_beta))
  }
  return(out[2:nrow(out),])
}


run_simulation_combined_dense <- function(n=100, np=n, p, rho, eff_type, beta_seq, B, hpc, 
                                    errorhandling="stop", gam.big, bkmr.iter=500, bkmr.knots=NULL,
                                    bkmr.exact=TRUE){
  require(doSNOW)
  require(doParallel)
  require(foreach)
  
  # initialize output
  out <- matrix(nrow=1, ncol=32+1)
  colnames(out) <- c("beta", "vs.GAM_HT", "vs.GAM_VS", "vs.BKMR_50", "vs.BKMR_95", "vs.SSGAM_50", "vs.SSGAM_95", "vs.ENET", "vs.ENET_alpha", "vs.OLS", "idx.GAM_HT_term", "idx.GAM_HT_big", "idx.GAM_HT_F", "idx.GAM_VS",
                     "idx.BKMR_CON", "idx.BKMR_H_50", "idx.BKMR_H_95", "idx.SSGAM_50", 
                     "idx.SSGAM_95", "idx.ENET", "idx.ENET_alpha",
                     "idx.PCR", "idx.WQS_POS", "idx.WQS_NEG", "idx.QGC", "idx.TEV", "idx.OLS",
                     "mse.GAM.add", "mse.GAM.big", "mse.BKMR", "mse.SSGAM", "mse.ENET", "mse.OLS")
  
  
  # define function to run one simulation iteration
  run_sim_iter <- function(n, p, eff_type, beta, rho, hpc, gam.big, bkmr.iter, bkmr.knots,
                           bkmr.exact, seed){
    if(!hpc){
      source("code/fit_all.R", local = TRUE)
      source("code/gen_data.R", local=TRUE)
      source("code/joint_tests.R", local=TRUE)
      source("code/RVeels.R") # function to run TEV
      
    }else{
      source("fit_all.R", local = TRUE)
      source("gen_data.R", local=TRUE)
      source("joint_tests.R", local=TRUE)
      source("RVeels.R") # function to run TEV
      
    }
    set.seed(seed)
    # generate data
    data <- gen_data_dense(n=n, p=p, eff_type=eff_type, beta=beta, rho=rho)
    
    # generate data used to evaluate MSE
    data.pred <- gen_data_dense(n=np, p=p, eff_type=eff_type, beta=beta, rho=rho,
                          sd.y=0)
    
    # plug in generated data to fitting function that returns results
    out.iter <- fit_all(data, data.pred, gam.big, bkmr.iter, bkmr.knots, bkmr.exact)
    
    return(out.iter)
  }
  
  # run B monte carlo iterations for each value of beta in the sequence provided
  for(i in 1:length(beta_seq)){
    beta <- beta_seq[i]
    
    # run simulation in parallel for this value of beta
    out_beta <- foreach(b=1:B,
                        .packages=packages,
                        .combine=rbind,
                        .errorhandling=errorhandling) %dopar% 
      run_sim_iter(n, p, eff_type, beta, rho, hpc, gam.big=gam.big, bkmr.iter=bkmr.iter, 
                   bkmr.knots=bkmr.knots, bkmr.exact=bkmr.exact, seed=i*b)
    
    # output matrix of all binary indicators of whether x1 was selected by 
    # each method in each monte carlo iteration
    out <- rbind(out, cbind(beta, out_beta))
  }
  return(out[2:nrow(out),])
}

run_null_simulation <- function(n=100, p, rho, B, hpc, 
                                errorhandling="stop", gam.big, bkmr.iter=500, bkmr.knots=NULL){
  
  # This function is for specific simulations which save the PIPs/p-values from simulations under the null
  # to compare the null distributions of those statistics
  # it's kept separate because the other code works well and it seems bad to start to change it
  # And this is only needed for a subset of methods to convey the point
  
  run_sim_iter <- function(n, p,rho, hpc, gam.big, bkmr.iter, seed){
    if(!hpc){
      source("code/fit_all.R", local = TRUE)
      source("code/gen_data.R", local=TRUE)
      source("code/joint_tests.R", local=TRUE)
      source("code/RVeels.R") # function to run TEV
      
    }else{
      source("fit_all.R", local = TRUE)
      source("gen_data.R", local=TRUE)
      source("joint_tests.R", local=TRUE)
      source("RVeels.R") # function to run TEV
      
    }
    
    # generate data under the null (eff_type does not matter)
    data <- gen_data(n=n, p=p, eff_type="linear", beta1=0, beta2=0, rho=rho)
    
    # packages
    require(bkmr)
    require(mgcv)
    require(spikeSlabGAM)
    require(caret)
    require(glmnet)
    require(pls)
    require(gWQS)
    require(qgcomp)
    
    # initialize array to store results for var sel methods
    result.vs <- rep(0, length=4)
    names(result.vs) <- c("vs.GAM_HT", "vs.BKMR", "vs.SSGAM", "vs.OLS")
    
    # scale data for BKMR
    data.scaled <- scale(data)
    n <- nrow(data.scaled)
    # p <- ncol(data.scaled) - 1
    
    # Do stuff
    
    # Fit frequentist gam and use frequentist H0 on x1 term
    gam.formula <- paste("y ~ ", paste("s(", "x", 1:p, ",k=5)", sep="", collapse="+"))
    fit.gam.ht <- gam(formula(gam.formula), data=data.frame(data.scaled), method="REML")
    pval.gam.ht <- summary(fit.gam.ht)$s.table[1,"p-value"]
    result.vs["vs.GAM_HT"] <- pval.gam.ht
    
    # Fit BKMR and save PIP of x1
    y <- data.scaled[,1]
    Z <- data.scaled[,2:ncol(data.scaled)]
    
    if(is.null(bkmr.knots)){
      fit.bkmr <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE)
    }else{
      knots.Z <- fields::cover.design(Z, nd = bkmr.knots)$design
      fit.bkmr <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE,
                          knots=knots.Z)
    }
    
    # fit.bkmr <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE)
    pip.1.km <- ExtractPIPs(fit.bkmr)[1,2]
    result.vs["vs.BKMR"] <- pip.1.km
    
    # SSGAM and save term pip for X1
    ssgam.formula <- paste("y ~ ", paste("x", 1:p, sep="", collapse="+"))
    fit.ssgam <- spikeSlabGAM(formula(ssgam.formula), data=data.frame(data.scaled),
                              mcmc=list(nChains=1,
                                        chainLength=bkmr.iter,
                                        burnin=as.integer(bkmr.iter/2)))
    pip.x1.gam <- term_pip(fit.ssgam, "x1", 100)
    result.vs["vs.SSGAM"] <- pip.x1.gam
    
    # OLS
    lm.formula <- paste("y ~ ", paste("x", 1:p, sep="", collapse="+"))
    fit.lm <- lm(formula(lm.formula), data=data.frame(data.scaled))
    lm.pval <- summary(fit.lm)$coefficients[2,4]
    result.vs["vs.OLS"] <- lm.pval
    
    # initialize array to store results for index methods
    result.idx <- rep(0, length=8)
    names(result.idx) <- c("idx.GAM_HT_big", "idx.GAM_HT_F",
                           "idx.BKMR_H", "idx.PCR", "idx.WQS_POS", "idx.WQS_NEG", "idx.QGC", "idx.OLS")
    
    if(gam.big){
      # GAM - frequentist HT with one big smooth
      gam.formula.big <- paste("y ~ ", "s(", paste("x", 1:p, sep="", collapse=","), ",k=", n/2, ")")
      fit.gam.big <- gam(formula(gam.formula.big), data=data.frame(data.scaled), method="REML")
      gam.big.pval <- summary(fit.gam.big)$s.table[,"p-value"]
      result.idx["idx.GAM_HT_big"] <- gam.big.pval
    }else{
      result.idx["idx.GAM_HT_big"] <- NA
    }
    
    # using F-test against null GAM model
    fit.gam.null <- gam(y ~ 1, data=data.frame(data.scaled), method="REML")
    # names(anova(fit.gam.null, fit.gam.ht, test="F"))
    gam.ht.f.pval <- anova(fit.gam.null, fit.gam.ht, test="F")$`Pr(>F)`[2]
    result.idx["idx.GAM_HT_F"] <- gam.ht.f.pval
    
    # BKMR hierarchical
    fit.bkmr.h <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE, groups=rep(1,p))
    group.pip.bkmr <- ExtractPIPs(fit.bkmr.h)[1,3]
    result.idx["idx.BKMR_H"] <- group.pip.bkmr
    
    # PCR using enough PCs to explain 75% of variance in exposure matrix
    
    # compute PCA
    pca <- prcomp(Z, center=TRUE, scale=TRUE)
    
    # select PCs
    var.explained <- pca$sdev^2 / sum(pca$sdev^2)
    A1 <- matrix(1, nrow=p, ncol=p)
    A1[upper.tri(A1)] <- 0
    cum.var.explained <- A1 %*% var.explained
    enough.var.explained <- cum.var.explained > .75
    ncomp <- min(which(enough.var.explained))
    Q <- Z %*% pca$rotation
    
    # fit model and do F test
    fit.pcr <- lm(y ~ Q[,1:ncomp]-1)
    pcr.pval <- anova(fit.pcr)$`Pr(>F)`[1]
    result.idx["idx.PCR"] <- pcr.pval
    
    # WQS - twice, with assumptions of negative and positive effect of mixture
    mix_name <- paste("x", 1:p, sep="")
    
    result.wqs.pos <- tryCatch({
      fit.wqs.pos <- gwqs(y ~ wqs, mix_name = mix_name, data = data.frame(data.scaled), 
                          q = 10, validation = 0.6, b = 100, b1_pos = TRUE, 
                          b1_constr = FALSE, family = "gaussian")
      wqs.pval.pos <- summary(fit.wqs.pos)$coefficients["wqs","Pr(>|t|)"]
      reject.wqs.pos <- wqs.pval.pos < .05
      as.integer(reject.wqs.pos)
    },
    error=function(e){return(NA)}
    )
    result.idx["idx.WQS_POS"] <- wqs.pval.pos
    
    result.wqs.neg <- tryCatch({
      fit.wqs.neg <- gwqs(y ~ wqs, mix_name = mix_name, data = data.frame(data.scaled), 
                          q = 10, validation = 0.6, b = 100, b1_pos = FALSE, 
                          b1_constr = FALSE, family = "gaussian")
      wqs.pval.neg <- summary(fit.wqs.neg)$coefficients["wqs","Pr(>|t|)"]
      reject.wqs.neg <- wqs.pval.neg < .05
      as.integer(reject.wqs.neg)
    },
    error=function(e){return(NA)}
    )
    result.idx["idx.WQS_NEG"] <- wqs.pval.neg
    
    # QGC
    qgc.formula <- paste("y ~ ", paste("x", 1:p, sep="", collapse="+"))
    fit.qgc <- qgcomp(formula(qgc.formula), data=data.frame(data.scaled))
    pval.qgc <- fit.qgc$pval[2]
    result.idx["idx.QGC"] <- pval.qgc
    
    # OLS
    lm.f <- summary(fit.lm)$fstatistic
    pval.lm.f <- pf(lm.f[1], df1=lm.f[2], df2=lm.f[3], lower.tail = FALSE)
    result.idx["idx.OLS"] <- pval.lm.f
    
    return(c(result.vs, result.idx))
    
  }
  
  out_null <- foreach(b=1:B,
                      .packages=packages,
                      .combine=rbind,
                      .errorhandling=errorhandling) %dopar% 
    run_sim_iter(n, p, rho, hpc, gam.big=gam.big, bkmr.iter=bkmr.iter, seed=seed)
  
  return(out_null)
}