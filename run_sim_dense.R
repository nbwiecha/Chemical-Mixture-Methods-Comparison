# Run simulation for exposure methods
# Linear data generating process with dense (no non-zero entries except for null case) beta vector
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
bkmr.iter <- 20000

filename <- paste("outputs//dense_sims_p", p, "_n", n, "_mcmc", bkmr.iter, ".Rdata", sep="")

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

out.rho5.dense <- run_simulation_combined_dense(n=n, p=p, eff_type="linear", 
                                                rho=0.5, B=B, beta_seq=seq(0, .25, length=10), 
                                                hpc=hpc, errorhandling="remove", gam.big=FALSE,
                                           bkmr.iter=bkmr.iter)

# save results
save(out.rho5.dense,
     file=filename)