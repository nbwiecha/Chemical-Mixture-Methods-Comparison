# Function to fit the index models on data and test for index association
# Run within the simulation function 
# to output results of whole-mixture tests applied to simulated dataset
# Author: Nate Wiecha, NCSU

# requires joint_tests.R to be loaded to conduct some tests after fitting models

# Combining fit_vs and fit_index from _v2 of those R scripts

fit_all <- function(data, data.pred, gam.big=FALSE, bkmr.iter=500, bkmr.knots=NULL, 
                    bkmr.exact=TRUE){
  
  # function to fit index methods on simulated dataset
  # output result of hypothesis test for no association bw index and response
  
  # packages used
  require(bkmr)
  require(mgcv)
  require(spikeSlabGAM)
  require(caret)
  require(glmnet)
  require(pls)
  require(gWQS)
  require(qgcomp)
  # require(TEV)
  
  # initialize array to store results
  result.vs <- rep(0, length=9)
  names(result.vs) <- c("vs.GAM_HT", "vs.GAM_VS", "vs.BKMR_50", "vs.BKMR_95", "vs.SSGAM_50", "vs.SSGAM_95", "vs.ENET", "vs.ENET_alpha", "vs.OLS")
  
  # scale data for BKMR: only scale y variable; Z's have no need
  # data.scaled <- scale(data)
  data.scaled <- data
  data.scaled[,1] <- scale(data.scaled[,1])
  
  n <- nrow(data.scaled)
  p <- ncol(data.scaled) - 1
  
  # No need to scale prediction data set: eval predictions on original scale
  # And scale up fitted values from fit to scaled Y
  
  # data.pred.scaled <- scale(data.pred)
  # data.pred.scaled <- data.pred
  # data.pred.scaled[,1] <- scale(data.pred.scaled[,1])
  # 
  # y.pred <- if(min(data.pred$y)==max(data.pred$y)){
  #   y.pred <- data.pred[,1]
  # }else{
  #   ###########################################################################
  #   #                             Change here                                 #
  #   ###########################################################################
  #   # y.pred <- data.pred.scaled[,1]
  #   y.pred <- data.pred[,1]
  # }
  # y.pred <- data.pred.scaled[,1]
  # Z.pred <- data.pred.scaled[,2:ncol(data.pred.scaled)]

  y.pred <- data.pred[,1]
  Z.pred <- data.pred[,2:ncol(data.pred)]
    
  mean.y <- mean(data[,1])
  sd.y <- sd(data[,1])
  # Fit frequentist gam and use frequentist H0 on x1 term
  gam.formula <- paste("y ~ ", paste("s(", "x", 1:p, ",k=5)", sep="", collapse="+"))
  fit.gam.ht <- gam(formula(gam.formula), data=data.frame(data.scaled), method="REML")
  reject.gam.ht <- summary(fit.gam.ht)$s.table[1,"p-value"] < .05
  result.vs["vs.GAM_HT"] <- as.integer(reject.gam.ht)
  
  # Fit frequentist gam with var. sel. penalty
  fit.gam.vs <- gam(formula(gam.formula), data=data.frame(data.scaled), select=TRUE)
  reject.gam.vs <- summary(fit.gam.vs)$s.table[1,"edf"] > 10^(-4)
  result.vs["vs.GAM_VS"] <- as.integer(reject.gam.vs)
  
  # Fit BKMR and test PIP of x1
  y <- data.scaled[,1]
  Z <- data.scaled[,2:ncol(data.scaled)]

  if(is.null(bkmr.knots)){
    fit.bkmr <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE)
  }else{
    knots.Z <- fields::cover.design(Z, nd = bkmr.knots)$design
    fit.bkmr <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE,
                        knots=knots.Z)
  }
  pip.1.km <- ExtractPIPs(fit.bkmr)[1,2]
  
  # PIP cutoff of .5 and .95 used
  reject.bkmr.50 <- pip.1.km > .5
  reject.bkmr.95 <- pip.1.km > .95
  result.vs["vs.BKMR_50"] <- as.integer(reject.bkmr.50)
  result.vs["vs.BKMR_95"] <- as.integer(reject.bkmr.95)
  
  # Fit spike and slab GAM and test term PIP of x1
  # since this package decomposes the function associated with one variable
  # to several different effects, need term_pip function from joint_tests.R
  
  ssgam.formula <- paste("y ~ ", paste("x", 1:p, sep="", collapse="+"))
  fit.ssgam <- spikeSlabGAM(formula(ssgam.formula), data=data.frame(data.scaled),
                            mcmc=list(nChains=1,
                                      chainLength=bkmr.iter,
                                      burnin=floor(bkmr.iter/2)))
  pip.x1.gam <- term_pip(fit.ssgam, "x1", 100)
  
  # PIP cutoff of .5 and .95 used
  reject.ssgam.50 <- pip.x1.gam > .5
  reject.ssgam.95 <- pip.x1.gam > .95
  result.vs["vs.SSGAM_50"] <- as.integer(reject.ssgam.50)
  result.vs["vs.SSGAM_95"] <- as.integer(reject.ssgam.95)
  
  # Elastic net
  cv_5 = trainControl(method = "cv", number = 5)
  train.1 <- train(
    y ~ ., data = data.scaled,
    method = "glmnet",
    trControl = cv_5
  )
  
  get_best_result = function(caret_fit) {
    best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
    best_result = caret_fit$results[best, ]
    rownames(best_result) = NULL
    best_result
  }
  hyperparams <- as.numeric(get_best_result(train.1)[1:2])
  
  fit.glmnet <- glmnet(Z, y, family="gaussian", alpha = hyperparams[1], lambda=hyperparams[2])
  x1.coef <- coefficients(fit.glmnet)["x1",1]
  reject.enet <- x1.coef > 0
  result.vs["vs.ENET"] <- as.integer(reject.enet)
  result.vs["vs.ENET_alpha"] <- hyperparams[1] # store choice of alpha as well
  
  # gam.formula <- paste("y ~ ", paste("s(", "x", 1:p, ",k=5)", sep="", collapse="+"))
  # fit.gam.ht <- gam(formula(gam.formula), data=data.frame(data.scaled), method="REML")
  # reject.gam.ht <- summary(fit.gam.ht)$s.table[1,"p-value"] < .05
  # result.vs["vs.GAM_HT"] <- as.integer(reject.gam.ht)
  
  # OLS
  lm.formula <- paste("y ~ ", paste("x", 1:p, sep="", collapse="+"))
  fit.lm <- lm(formula(lm.formula), data=data.frame(data.scaled))
  reject.lm.ht <- summary(fit.lm)$coefficients[2,4] < .05
  result.vs["vs.OLS"] <- as.integer(reject.lm.ht)
  
  ###############################################################################
  #                         Begin Index Methods                                 #
  ###############################################################################
  
  # initialize array to store outputs
  result.idx <- rep(0, length=17)
  names(result.idx) <- c("idx.GAM_HT_term", "idx.GAM_HT_big", "idx.GAM_HT_F", "idx.GAM_VS",
                     "idx.BKMR_CON", "idx.BKMR_H_50", "idx.BKMR_H_95", "idx.SSGAM_50", "idx.SSGAM_95", "idx.ENET",
                     "idx.ENET_alpha",
                     "idx.PCR", "idx.WQS_POS", "idx.WQS_NEG", "idx.QGC", "idx.TEV", "idx.OLS")
  
  # scale dataset - required for BKMR
  data.scaled <- scale(data)
  n <- nrow(data.scaled)
  p <- ncol(data.scaled) - 1
  
  # GAM - frequentist HT w bonferroni
  gam.formula <- paste("y ~ ", paste("s(", "x", 1:p, ",k=5)", sep="", collapse="+"))
  # fit.gam.ht <- gam(formula(gam.formula), data=data.frame(data.scaled), method="REML")
  
  # using term pvals with bonferroni
  gam.ht.pvals <- summary(fit.gam.ht)$s.table[,"p-value"]
  gam.ht.pvals.adj <- gam.ht.pvals * p
  min.gam.ht.pval.adj <- min(gam.ht.pvals.adj)
  reject.gam.ht.term <-  min.gam.ht.pval.adj < .05
  result.idx["idx.GAM_HT_term"] <- as.integer(reject.gam.ht.term)

  if(gam.big){
    # GAM - frequentist HT with one big smooth
    gam.formula.big <- paste("y ~ ", "s(", paste("x", 1:p, sep="", collapse=","), ",k=", n/2, ")")
    fit.gam.big <- gam(formula(gam.formula.big), data=data.frame(data.scaled), method="REML")
    gam.big.pval <- summary(fit.gam.big)$s.table[,"p-value"]
    reject.gam.big <- gam.big.pval < 0.05
    result.idx["idx.GAM_HT_big"] <- as.integer(reject.gam.big)
  }else{
    result.idx["idx.GAM_HT_big"] <- NA
  }
  
  # using F-test against null GAM model
  fit.gam.null <- gam(y ~ 1, data=data.frame(data.scaled), method="REML")
  # names(anova(fit.gam.null, fit.gam.ht, test="F"))
  gam.ht.f.pval <- anova(fit.gam.null, fit.gam.ht, test="F")$`Pr(>F)`[2]
  reject.gam.ht.f <- gam.ht.f.pval < .05
  result.idx["idx.GAM_HT_F"] <- as.integer(reject.gam.ht.f)
  
  # GAM - frequentist variable selection
  # fit.gam.vs <- gam(formula(gam.formula), data=data.frame(data.scaled), select=TRUE)
  reject.gam.vs <- max(summary(fit.gam.vs)$s.table[,"edf"]) > 10^(-4)
  result.idx["idx.GAM_VS"] <- as.integer(reject.gam.vs)
  
  # BKMR using joint pip (from joint_tests.R)
  y <- data.scaled[,1]
  Z <- data.scaled[,2:ncol(data.scaled)]
  
  # fit.bkmr <- kmbayes(y=y, Z=Z, iter=500, varsel=TRUE, verbose=FALSE)

  contrast.test.bkmr <- bkmr_contrast_test(fit.bkmr, burn=bkmr.iter/2)
  result.idx["idx.BKMR_CON"] <- contrast.test.bkmr
  
  if(is.null(bkmr.knots)){
    fit.bkmr.h <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE, groups=rep(1,p))
  }else{
    knots.Z <- fields::cover.design(Z, nd = bkmr.knots)$design
    fit.bkmr.h <- kmbayes(y=y, Z=Z, iter=bkmr.iter, varsel=TRUE, verbose=FALSE,
                        knots=knots.Z, groups=rep(1,p))
  }
  group.pip.bkmr <- ExtractPIPs(fit.bkmr.h)[1,3]
  reject.bkmr.h.50 <- group.pip.bkmr > .5
  reject.bkmr.h.95 <- group.pip.bkmr > .95
  
  # results of HTs conducted using PIP > .5 and PIP > .95
  result.idx["idx.BKMR_H_50"] <- as.integer(reject.bkmr.h.50)
  result.idx["idx.BKMR_H_95"] <- as.integer(reject.bkmr.h.95)
  
  # Spike and slab GAM using joint pip (from joint_tests.R)
  ssgam.formula <- paste("y ~ ", paste("x", 1:p, sep="", collapse="+"))
  # fit.ssgam <- spikeSlabGAM(formula(ssgam.formula), data=data.frame(data),
  #                           mcmc=list(nChains=1))
  joint.pip.ssgam <- joint_gam_pip(fit.ssgam)
  
  # results of HTs conducted using PIP > .5 and PIP > .95
  reject.ssgam.50 <- joint.pip.ssgam > .5
  reject.ssgam.95 <- joint.pip.ssgam > .95
  result.idx["idx.SSGAM_50"] <- as.integer(reject.ssgam.50)
  result.idx["idx.SSGAM_95"] <- as.integer(reject.ssgam.95)
  
  # Elastic net: reject H0 if any var selected. Not used in simulation study
  # (too sensitive)
  # cv_5 = trainControl(method = "cv", number = 5)
  # train.1 <- train(
  #   y ~ ., data = data.scaled,
  #   method = "glmnet",
  #   trControl = cv_5
  # )
  # 
  # get_best_result = function(caret_fit) {
  #   best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  #   best_result = caret_fit$results[best, ]
  #   rownames(best_result) = NULL
  #   best_result
  # }
  # hyperparams <- as.numeric(get_best_result(train.1)[1:2])
  result.idx["idx.ENET_alpha"] <- hyperparams[1]
  # fit.glmnet <- glmnet(Z, y, family="gaussian", alpha = hyperparams[1], lambda=hyperparams[2])
  max.coef <- max(coefficients(fit.glmnet)[2:(p+1),1])
  reject.enet <- max.coef > 0
  result.idx["idx.ENET"] <- as.integer(reject.enet)
  
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
  reject.pcr <- pcr.pval < .05
  result.idx["idx.PCR"] <- as.integer(reject.pcr)
  
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
  result.idx["idx.WQS_POS"] <- result.wqs.pos
  
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
  result.idx["idx.WQS_NEG"] <- result.wqs.neg
  
  # QGC
  qgc.formula <- paste("y ~ ", paste("x", 1:p, sep="", collapse="+"))
  fit.qgc <- qgcomp(formula(qgc.formula), data=data.frame(data.scaled))
  pval.qgc <- fit.qgc$pval[2]
  reject.qgc <- pval.qgc < .05
  result.idx["idx.QGC"] <- as.integer(reject.qgc)
  
  # TEV
  tev.fit <- RVeels(y, Z, alpha=.05)
  tev.lb <- tev.fit[[2]][1]
  result.idx["idx.TEV"] <- as.integer(tev.lb > 0)
  
  # OLS
  lm.f <- summary(fit.lm)$fstatistic
  pval.lm.f <- pf(lm.f[1], df1=lm.f[2], df2=lm.f[3], lower.tail = FALSE)
  reject.lm.f <- pval.lm.f < .05
  result.idx["idx.OLS"] <- as.integer(reject.lm.f)
  ##############################################################################
  #                          Estimate MSE                                      #
  ##############################################################################
  
  ##############################################################################
  #                                Changes here                                #
  ##############################################################################
  
  result.mse <- rep(NA, 6)
  names(result.mse) <- c("mse.GAM.add", "mse.GAM.big", "mse.BKMR", "mse.SSGAM", "mse.ENET", "mse.OLS")
  # data.pred.scaled <- scale(data.pred)
  # y.pred <- data.pred.scaled[,1]
  # Z.pred <- data.pred.scaled[,2:ncol(data.pred.scaled)]
  
  # yhat.gam.add.1 <- predict(fit.gam.ht, newdata=data.frame(data.pred.scaled))
  yhat.gam.add.1 <- predict(fit.gam.ht, newdata=data.frame(data.pred))
  yhat.gam.add <- yhat.gam.add.1 * sd.y + mean.y
  
  mse.gam.add <- mean((y.pred - yhat.gam.add)^2)
  result.mse["mse.GAM.add"] <- mse.gam.add
  
  if(gam.big){
    # yhat.gam.big.1 <- predict(fit.gam.big, newdata=data.frame(data.pred.scaled))
    yhat.gam.big.1 <- predict(fit.gam.big, newdata=data.frame(data.pred))
    yhat.gam.big <- yhat.gam.big.1 * sd.y + mean.y
    
    mse.gam.big <- mean((y.pred - yhat.gam.big)^2)
    result.mse["mse.GAM.big"] <- mse.gam.big
  }else{
    result.mse["mse.GAM.big"] <- NA
  }
  
  if(bkmr.exact){
    bkmr.method <- "exact"
  }else{
    bkmr.method <- "approx"
  }
  yhat.bkmr.1 <- ComputePostmeanHnew(fit.bkmr, Znew = Z.pred, method = bkmr.method)
  yhat.bkmr <- yhat.bkmr.1$postmean * sd.y + mean.y
  mse.bkmr <- mean((y.pred - yhat.bkmr)^2)
  result.mse["mse.BKMR"] <- mse.bkmr
  
  # yhat.ssgam.1 <- predict(fit.ssgam, newdata=data.frame(data.pred.scaled))
  yhat.ssgam.1 <- predict(fit.ssgam, newdata=data.frame(data.pred))
  yhat.ssgam <- yhat.ssgam.1 * sd.y + mean.y
  mse.ssgam <- mean((y.pred - yhat.ssgam)^2)
  result.mse["mse.SSGAM"] <- mse.ssgam
  
  yhat.enet.1 <- predict(fit.glmnet, newx = as.matrix(Z.pred))
  yhat.enet <- yhat.enet.1 * sd.y + mean.y
  mse.enet <- mean((y.pred - yhat.enet)^2)
  result.mse["mse.ENET"] <- mse.enet
  
  # yhat.ols.1 <- predict(fit.lm, newdata=data.frame(data.pred.scaled))
  yhat.ols.1 <- predict(fit.lm, newdata=data.frame(data.pred))
  yhat.ols <- yhat.ols.1 * sd.y + mean.y
  mse.ols <- mean((y.pred - yhat.ols)^2)
  result.mse["mse.OLS"] <- mse.ols
  
  return(c(result.vs, result.idx, result.mse))
}
