# Simulation of BKMR under the null with Hierarchical variable selection
rm(list=ls())
library(bkmr)
library(doSNOW)
library(doParallel)
library(foreach)

nCores <- detectCores()
cl <- makeCluster(nCores-1)
registerDoSNOW(cl)

B <- 100
# B <- 2
ps <- 1:10
iters <- 1000
n <- 100
rho <- .5

bkmr_sim_iter <- function(n, p, iters, L.Sigma){
  X <-  matrix(rnorm(n*p), ncol=p) %*% (L.Sigma)
  y <- rnorm(n)
  fit <- kmbayes(y, X, groups=rep(1, p), varsel=TRUE, verbose=FALSE, iter=iters)
  if(p > 1){
    pip <- ExtractPIPs(fit)[1,3]
  }else{
    pip <- mean(fit$delta[ceiling(iters/2):iters])
  }
  
  return(pip)
  # TracePlot(fit, par="sigsq.eps")
  # TracePlot(fit, par="r", comp=1)
  # TracePlot(fit, par="r", comp=2)
}

pips <- matrix(nrow=B, ncol=length(ps))
colnames(pips) <- ps

# ps <- 1
for(i in 1:length(ps)){
  p <- ps[i]
  print(paste("starting p=", p))
  Sigma <- matrix(rho, nrow=p, ncol=p) + diag(1-rho, nrow=p, ncol=p)
  L.Sigma <- chol(Sigma)
  pips.p <- foreach(b=1:B,
                  .packages=c('bkmr'),
                  .combine=rbind) %dopar% 
    bkmr_sim_iter(n, p, iters, L.Sigma)
  pips[,i] <- pips.p
  
}
pips.1 <- pips[,1]
hist(pips.1)
mean(pips.1)
load("~/GitHub/Chemical-Mixture-Methods-Comparison/outputs/BKMR_null_hier_sim.Rdata")
pips <- cbind(pips)
boxplot(pips, main="Null distributions of BKMR group PIPs for p=2, ..., 10", ylim=c(0,1))

png("GitHub/Chemical-Mixture-Methods-Comparison/outputs//bkmr_null_distn.png", width = 500, height = 400)
boxplot(pips, main="Null distributions of BKMR group PIPs for p=2, ..., 10", ylim=c(0,1))
dev.off()

pips.1.nongroup <- rep(NA, B)
p <- 1
# Sigma <- matrix(rho, nrow=p, ncol=p) + diag(1-rho, nrow=p, ncol=p)
# L.Sigma <- chol(Sigma)
bkmr_sim_iter_nongroup <- function(n, iters){
  X <-  matrix(rnorm(n), ncol=1)
  y <- rnorm(n)
  fit <- kmbayes(y, X, varsel=TRUE, verbose=FALSE, iter=iters)
  
  pip <- ExtractPIPs(fit)[1,2]
  
  return(pip)
  # TracePlot(fit, par="sigsq.eps")
  # TracePlot(fit, par="r", comp=1)
  # TracePlot(fit, par="r", comp=2)
}
pips.nongroup <- foreach(b=1:B,
                  .packages=c('bkmr'),
                  .combine=rbind) %dopar% 
  bkmr_sim_iter_nongroup(n, iters)

hist(pips.nongroup)
mean(pips.nongroup)

n <- 10
ps <- 2:5
pips.10 <- matrix(nrow=B, ncol=length(ps))
colnames(pips.10) <- ps
for(i in 1:length(ps)){
  p <- ps[i]
  print(paste("starting p=", p))
  Sigma <- matrix(rho, nrow=p, ncol=p) + diag(1-rho, nrow=p, ncol=p)
  L.Sigma <- chol(Sigma)
  pips.p <- foreach(b=1:B,
                    .packages=c('bkmr'),
                    .combine=rbind) %dopar% 
    bkmr_sim_iter(n, p, iters, L.Sigma)
  pips.10[,i] <- pips.p
  
}
boxplot(pips.10)

save.image("~/GitHub/Chemical-Mixture-Methods-Comparison/outputs/BKMR_null_hier_sim.Rdata")
