# Run simulation for exposure methods
# Interactive data generating process
# Author: Nate Wiecha, NCSU

library(doSNOW)
library(doParallel)
library(foreach)

# packages needed
packages <-  c("mgcv", "bkmr", "spikeSlabGAM", 'caret', 'glmnet', 'gWQS', "qgcomp", "dplyr")

# set p (dimension of exposures) before running; p=5, 10 used
p <- 5

# set n (sample size) before running; n=100, 400 used
n <- 400

filename <- paste("outputs//sine_sims_p", p, "_n", n, "_mcmc20000.Rdata", sep="")

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

B <- 200 # MC iterations

out.rho0.sine <- run_simulation_combined(n=n, p=p, np=100, rho=0, eff_type="sine",
                                                beta1_seq=seq(0, 5, length=5), beta2_seq=seq(0, .5, length=5),
                                                B=B,
                                                hpc=hpc, errorhandling="stop", gam.big=TRUE,
                                                bkmr.iter=20000, bkmr.exact=FALSE)

out.rho5.sine <- run_simulation_combined(n=n, p=p, np=100, rho=.5, eff_type="sine",
                                                   beta1_seq=seq(0, 5, length=5), beta2_seq=seq(0, .5, length=5),
                                                   B=B,
                                                   hpc=hpc, errorhandling="stop", gam.big=TRUE,
                                                   bkmr.iter=20000, bkmr.exact=FALSE)
# 
# out.rho9.interactive <- run_simulation_combined(n=100, p=p, rho=.9, eff_type="interactive",
#                                                 beta1_seq=seq(0, .5, length=10), beta2_seq=seq(0, .5, length=10),
#                                                 B=B,
#                                                 hpc=hpc, errorhandling="remove", gam.big=FALSE,
#                                                 bkmr.iter=20000)
# save results
save(out.rho0.sine,
     out.rho5.sine,
     # out.rho9.interactive,
     file=filename)