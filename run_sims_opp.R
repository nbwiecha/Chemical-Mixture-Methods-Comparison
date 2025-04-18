# Run simulation for exposure methods
# Opposite data generating process
# Author: Nate Wiecha, NCSU

library(doSNOW)
library(doParallel)
library(foreach)

# packages needed
packages <-  c("mgcv", "bkmr", "spikeSlabGAM", 'caret', 'glmnet', 'gWQS', "qgcomp", "dplyr")

# set p (dimension of exposures) before running; only p=5 used for opposite
p <- 10
filename <- paste("outputs//opposite_sims_p", p, "_mcmc20000.Rdata", sep="")

# set up depending on whether running on HPC
hpc <- 1
if(!hpc){
  setwd("~/GitHub/Chemical-Mixture-Methods-Comparison")
  source("code/simulation/fit_all.R")
  source("code/simulation/gen_data.R")
  source("code/simulation/joint_tests.R")
  source("code/simulation/simulation_functions.R")
  source("code/simulation/RVeels.R")
  
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

out.rho5.opposite <- run_simulation_combined(n=100, p=p, rho=.5, eff_type="linear",
                                                beta1_seq=seq(-1, 1, length=10), beta2_seq=rep(1, 10), B=B,
                                                hpc=hpc, errorhandling="stop", gam.big=FALSE,
                                                bkmr.iter=20000)
# save results
save(out.rho5.opposite,
     file=filename)