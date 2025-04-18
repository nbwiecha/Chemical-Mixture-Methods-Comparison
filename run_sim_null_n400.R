# Run simulation for exposure methods
# Linear data generating process
# Author: Nate Wiecha, NCSU

library(doSNOW)
library(doParallel)
library(foreach)

# packages needed
packages <-  c("mgcv", "bkmr", "spikeSlabGAM", 'caret', 'glmnet', 'gWQS', "qgcomp", "dplyr")

# set p (dimension of exposures) before running; p=5 used when n=400
p <- 5

# set n (sample size) before running; n=100, 400 used
n <- 400
bkmr.iter <- 20000

filename <- paste("outputs//null_sims_p", p, "_n", n, "_mcmc", bkmr.iter, ".Rdata", sep="")

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

out.rho5.null.n400 <- run_null_simulation(n=n, p=p, rho=0.5, B=B,
                                           hpc=hpc, errorhandling="stop", gam.big=TRUE,
                                           bkmr.iter=bkmr.iter)

# save results
save(out.rho5.null.n400,
     file=filename)