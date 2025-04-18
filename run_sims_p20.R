# Run simulation for exposure methods
# Linear data generating process
# Author: Nate Wiecha, NCSU

library(doSNOW)
library(doParallel)
library(foreach)

# packages needed
packages <-  c("mgcv", "bkmr", "spikeSlabGAM", 'caret', 'glmnet', 'gWQS', "qgcomp", "dplyr")

# set p (dimension of exposures) before running; p=5, 10 used
p <- 20

# set n (sample size) before running; n=100, 400 used
n <- 100

filename <- paste("outputs//all_sims_p", p, "_n", n, "_mcmc20000.Rdata", sep="")

# set up depending on whether running on HPC
hpc <- 1
if(!hpc){
  setwd("~/GitHub/Chemical-Mixture-Methods-Comparison")
  source("code/fit_all.R")
  source("code/gen_data.R")
  source("code/joint_tests.R")
  source("code/simulation_functions.R")
  source("code/RVeels.R")
  
  nCores <- detectCores()
  cl <- makeCluster(nCores-1)
  registerDoSNOW(cl)
  options(mc.cores=1)
  
}else{
  source("fit_all.R")
  source("gen_data.R")
  source("joint_tests.R")
  source("simulation_functions.R")
  source("RVeels.R")
  
  
  nCores <- strtoi(Sys.getenv(c("LSB_DJOB_NUMPROC")))
  cl <- makeCluster(nCores)
  registerDoSNOW(cl)
  options(mc.cores=1)
}


B <- 400 # MC iterations
# B <- 4

out.p20.linear <- run_simulation_combined(n=n, p=p, rho=0.5, eff_type="linear", 
                                           beta1_seq=seq(0, 1, length=10), beta2_seq=seq(0, 1, length=10), B=B, 
                                           hpc=hpc, errorhandling="remove", gam.big=FALSE,
                                           bkmr.iter=20000)

out.p20.nonlinear <- run_simulation_combined(n=n, p=p, rho=.5, eff_type="nonlinear", 
                                                beta1_seq=seq(0, .5, length=10), beta2_seq=seq(0, .5, length=10), B=B, 
                                                hpc=hpc, errorhandling="remove", gam.big=FALSE,
                                              bkmr.iter=20000)

out.p20.interactive <- run_simulation_combined(n=n, p=p, rho=.5, eff_type="interactive", 
                                           beta1_seq=seq(0, .5, length=10), beta2_seq=seq(0, .5, length=10), B=B, 
                                           hpc=hpc, errorhandling="remove", gam.big=FALSE,
                                           bkmr.iter=20000)

# save results
save(out.p20.linear,
     out.p20.nonlinear,
     out.p20.interactive,
     file=filename)