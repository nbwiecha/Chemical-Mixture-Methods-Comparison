# Script to create plots from simulation study output files
# Nate Wiecha

# LEAVING OFF 5/31: SELECTING TABLE ROW BY PICKING BIGGEST DIFFS BETWEEN POWERS
# SOME COMMENTED OUT CODE IN TABLES FUNCTION

# 8/16 notes:
# Relabeled data to include p and n. The plot functions beginning on line 434 
# need to be updated.

# 8/22: got up to line 717. Tables need to be updated but code should be fine.

# 3/6/2025: updated to produce MSE plots since tables were actually misleading.
rm(list=ls())
# Load libraries
library(tidyverse)
library(ggpubr)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
setwd("~/GitHub/PFAS-methods")

# Function to summarize output datasets
summ_data <- function(sim_out){
  sim.summ <- sim_out %>% as.data.frame() %>%
    # select(-ENET_alpha) %>%
    rename(beta1=beta) %>%
    group_by(beta1)  %>%
    summarize(across(everything(), \(x) mean(x, na.rm=TRUE)))
  return(sim.summ)
}
# load("outputs//poster//linear_sims_p5.Rdata")
# load("outputs//poster//nonlinear_sims_p5.Rdata")
# load("outputs//poster//interactive_sims_p5.Rdata")
# load("outputs//poster//opposite_sims_p5.Rdata")


load("outputs//HPC//linear_sims_p10_n100_mcmc20000.Rdata")

out.rho0.linear.p10.n100 <- out.rho0.linear
out.rho5.linear.p10.n100 <- out.rho5.linear
out.rho9.linear.p10.n100 <- out.rho9.linear

rm(out.rho0.linear, out.rho5.linear, out.rho9.linear)
load("outputs//HPC//nonlinear_sims_p10_n100_mcmc20000.Rdata")


out.rho0.nonlinear.p10.n100 <- out.rho0.nonlinear
out.rho5.nonlinear.p10.n100 <- out.rho5.nonlinear
out.rho9.nonlinear.p10.n100 <- out.rho9.nonlinear

rm(out.rho0.nonlinear, out.rho5.nonlinear, out.rho9.nonlinear)

load("outputs//HPC//interactive_sims_p10_n100_mcmc20000.Rdata")


out.rho0.interactive.p10.n100 <- out.rho0.interactive
out.rho5.interactive.p10.n100 <- out.rho5.interactive
out.rho9.interactive.p10.n100 <- out.rho9.interactive

rm(out.rho0.interactive, out.rho5.interactive, out.rho9.interactive)

load("outputs//HPC//opposite_sims_p10_mcmc20000.Rdata")
out.rho5.opposite.p10.n100 <- out.rho5.opposite
rm(out.rho5.opposite)

load("outputs//HPC//linear_sims_p5_n100_mcmc20000.Rdata")

out.rho0.linear.p5.n100 <- out.rho0.linear
out.rho5.linear.p5.n100 <- out.rho5.linear
out.rho9.linear.p5.n100 <- out.rho9.linear

rm(out.rho0.linear, out.rho5.linear, out.rho9.linear)

load("outputs//HPC//nonlinear_sims_p5_n100_mcmc20000.Rdata")


out.rho0.nonlinear.p5.n100 <- out.rho0.nonlinear
out.rho5.nonlinear.p5.n100 <- out.rho5.nonlinear
out.rho9.nonlinear.p5.n100 <- out.rho9.nonlinear

rm(out.rho0.nonlinear, out.rho5.nonlinear, out.rho9.nonlinear)


load("outputs//HPC//interactive_sims_p5_n100_mcmc20000.Rdata")


out.rho0.interactive.p5.n100 <- out.rho0.interactive
out.rho5.interactive.p5.n100 <- out.rho5.interactive
out.rho9.interactive.p5.n100 <- out.rho9.interactive

rm(out.rho0.interactive, out.rho5.interactive, out.rho9.interactive)

# n=400
load("outputs//HPC//linear_sims_p5_n400_mcmc20000.Rdata")

out.rho0.linear.p5.n400 <- out.rho0.linear
out.rho5.linear.p5.n400 <- out.rho5.linear

rm(out.rho0.linear, out.rho5.linear)

load("outputs//HPC//nonlinear_sims_p5_n400_mcmc20000.Rdata")


out.rho0.nonlinear.p5.n400 <- out.rho0.nonlinear
out.rho5.nonlinear.p5.n400 <- out.rho5.nonlinear

rm(out.rho0.nonlinear, out.rho5.nonlinear)


load("outputs//HPC//interactive_sims_p5_n400_mcmc20000.Rdata")


out.rho0.interactive.p5.n400 <- out.rho0.interactive
out.rho5.interactive.p5.n400 <- out.rho5.interactive

rm(out.rho0.interactive, out.rho5.interactive)

load("outputs//HPC//sine_sims_p5_n100_mcmc20000.Rdata")
out.rho0.sin.p5.n100 <- out.rho0.sine
out.rho5.sin.p5.n100 <- out.rho5.sine
out.rho9.sin.p5.n100 <- out.rho9.sine

rm(out.rho0.sine, out.rho5.sine, out.rho9.sine)

load("outputs//HPC//sine_sims_p10_n100_mcmc20000.Rdata")
out.rho0.sin.p10.n100 <- out.rho0.sine
out.rho5.sin.p10.n100 <- out.rho5.sine
out.rho9.sin.p10.n100 <- out.rho9.sine

rm(out.rho0.sine, out.rho5.sine, out.rho9.sine)

load("outputs//HPC//sine_sims_p5_n400_mcmc20000.Rdata")
out.rho0.sin.p5.n400 <- out.rho0.sine
out.rho5.sin.p5.n400 <- out.rho5.sine

rm(out.rho0.sine, out.rho5.sine)
# load("outputs//HPC//opposite_sims_p5_mcmc20000.Rdata")

load("outputs//HPC//null_sims_p5_n100_mcmc20000.Rdata")
out.rho5.null.p5.n100 <- out.rho5.null
rm(out.rho5.null)

load("outputs//HPC//dense_sims_p10_n100_mcmc20000.Rdata")
out.rho5.dense.p10.n100 <- out.rho5.dense
rm(out.rho5.dense)

load("outputs//HPC//dense_sims_p20_n100_mcmc20000.Rdata")
out.rho5.dense.p20.n100 <- out.rho5.dense
rm(out.rho5.dense)

# summ_data(select(as.data.frame(out.rho5.dense.p10.n100), starts_with(c("beta", "idx")))) %>%
#   view()
################################################################################
#                          General function to create set of plots             #
################################################################################
make_power_plot_index <- function(sim_out, title, smooth=FALSE){
  # function to make power curve plot for index simulation output
  
  # summarize raw sim output
  sim.summ <- summ_data(sim_out)
  
  # transform into data for plotting 
  plotdat <- sim.summ %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value)
  
  powerdiffs <- plotdat %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row <- which.max(powerdiffs$diff)
  
  # make plots
  if(smooth){
    # loess smoothed power curve
    
    p <- ggplot(plotdat, aes(x=beta1, y=Power, color=Method)) +
      geom_smooth(se=FALSE) +
      ggtitle(title) +
      scale_y_continuous(breaks=c(0, 0.05, 0.5, 0.80, 1), limits = c(0,1)) +
      theme_classic(base_size = 30) +
      theme(text = element_text(family = "serif"),
            legend.key.size=unit(1,'cm'),
            legend.position="none") +
      labs(x=bquote(beta),
           y=bquote(Pr(Reject~H[0]:~no~effect~of~mixture)))+
      geom_label_repel(data=plotdat %>% filter(beta1==unique(plotdat$beta1)[label.row]),aes(label=Method),
                       max.overlaps=Inf, size=10)
  }
  if(!smooth){
    # line plot power curve
    
    p <- ggplot(plotdat, aes(x=beta1, y=Power, color=Method)) +
      geom_line() +
      ggtitle(title) +
      scale_y_continuous(breaks=c(0, 0.05, 0.5, 0.80, 1), limits = c(0,1)) +
      theme_classic(base_size = 30) +
      theme(text = element_text(family = "serif"),
            legend.key.size=unit(1,'cm'),
            legend.position="none") +
      labs(x=bquote(beta),
           y=bquote(Pr(Reject~H[0]:~no~effect~of~mixture)))+
      geom_label_repel(data=plotdat %>% filter(beta1==unique(plotdat$beta1)[label.row]),aes(label=Method),
                       max.overlaps=Inf, size=10)
  }
  
  return(p)
}

make_power_plot_vs <- function(sim_out, title, smooth=FALSE){
  # Function to make individual power plot for one scenario
  # sim_out is simulation output dataset
  # title is plot title
  # smooth is whether to smooth the power curves
  
  # Create summarized data from raw output
  sim.summ <- summ_data(sim_out)
  
  # Create data set for plotting
  plotdat <- sim.summ %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value)
  
  powerdiffs <- plotdat %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row <- which.max(powerdiffs$diff)
  
  # Create plots: smoothed or unsmoothed power curves
  if(smooth){
    
    # if smoothed, use loess smoother to make curve
    p <- ggplot(plotdat, aes(x=beta1, y=Power, color=Method)) +
      geom_smooth(se=FALSE) +
      ggtitle(title) +
      scale_y_continuous(breaks=c(0, 0.05, 0.5, 0.80, 1), limits = c(0,1)) +
      theme_classic(base_size = 30) +
      theme(text = element_text(family = "serif"),
            legend.key.size=unit(1,'cm'),
            legend.position = "none") +
      labs(x=bquote(beta),
           y=bquote(Pr(Reject~H[0]:~no~effect~of~a[1]))) +
      geom_label_repel(data=plotdat %>% filter(beta1==unique(plotdat$beta1)[label.row]),aes(label=Method),
                       max.overlaps=Inf, size=10)
  }
  if(!smooth){
    
    # otherwise use geom_line
    p <- ggplot(plotdat, aes(x=beta1, y=Power, color=Method)) +
      geom_line() +
      ggtitle(title) +
      scale_y_continuous(breaks=c(0, 0.05, 0.5, 0.80, 1), limits = c(0,1)) +
      theme_classic(base_size = 30) +
      theme(text = element_text(family = "serif"),
            legend.key.size=unit(1,'cm'),
            legend.position = 'none') +
      labs(x=bquote(beta),
           y=bquote(Pr(Reject~H[0]:~no~effect~of~a[1]))) +
      geom_label_repel(data=plotdat %>% filter(beta1==unique(plotdat$beta1)[label.row]),aes(label=Method),
                       max.overlaps=Inf, size=10)
    
  }
  
  return(p)
}

make_mse_plot <- function(sim_out, title, smooth=FALSE){
  # Function to make MSE plot for one scenario
  # sim_out is simulation output dataset
  # title is plot title
  # smooth is whether to smooth the power curves
  
  # Create summarized data from raw output
  sim.summ <- summ_data(sim_out)
  
  # Create data set for plotting
  plotdat <- sim.summ %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, MSE=value) %>%
    drop_na()
  
  msediffs <- plotdat %>%
    group_by(beta1) %>%
    summarize(diff = max(MSE) - min(MSE))
  
  label.row <- which.max(msediffs$diff)
  
  # Create plots: smoothed or unsmoothed power curves
  if(smooth){
    
    # if smoothed, use loess smoother to make curve
    p <- ggplot(plotdat, aes(x=beta1, y=MSE, color=Method)) +
      geom_smooth(se=FALSE) +
      ggtitle(title) +
      # scale_y_continuous(breaks=c(0, 0.05, 0.5, 0.80, 1)) +
      theme_classic(base_size = 30) +
      theme(text = element_text(family = "serif"),
            legend.key.size=unit(1,'cm'),
            legend.position = "none") +
      labs(x=bquote(beta),
           y=bquote(MSE)) +
      geom_label_repel(data=plotdat %>% filter(beta1==unique(plotdat$beta1)[label.row]),aes(label=Method),
                       max.overlaps=Inf, size=10)
  }
  if(!smooth){
    
    # otherwise use geom_line
    p <- ggplot(plotdat, aes(x=beta1, y=MSE, color=Method)) +
      geom_line() +
      ggtitle(title) +
      # scale_y_continuous(breaks=c(0, 0.05, 0.5, 0.80, 1)) +
      theme_classic(base_size = 30) +
      theme(text = element_text(family = "serif"),
            legend.key.size=unit(1,'cm'),
            legend.position = 'none') +
      labs(x=bquote(beta),
           y=bquote(MSE)) +
      geom_label_repel(data=plotdat %>% filter(beta1==unique(plotdat$beta1)[label.row]),aes(label=Method),
                       max.overlaps=Inf, size=10)
    
  }
  
  return(p)
}

create_plots <- function(linear.output, nonlinear.output, 
                         interactive.output, sine.output, 
                         opposite.output=NULL, rho.sim, p.sim, n.sim){
  
  ################################################################################
  #                     Extract data                                             #
  ################################################################################
  summ.lin <- summ_data(as.data.frame(linear.output))
  summ.nonlin <- summ_data(as.data.frame(nonlinear.output))
  summ.int <- summ_data(as.data.frame(interactive.output))
  summ.sin <- summ_data(as.data.frame(sine.output))
  
  vs.power.linear <- linear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  vs.power.nonlinear <- nonlinear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  vs.power.int <- interactive.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  vs.power.sin <- sine.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  index.power.linear <- linear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  index.power.nonlinear <- nonlinear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  index.power.interactive <- interactive.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  index.power.sin <- sine.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  
  if(!is.null(opposite.output)){
  index.power.opposite <- opposite.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  }
  
  mse.linear <- linear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  mse.nonlinear <- nonlinear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  mse.interactive <- interactive.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  mse.sin <- sine.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  
  ###############################################################################
  #                     Variable selection plots                                #
  ###############################################################################
  
  # load("outputs//var_sel_sims_p5.Rdata")
  
  
  #https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
  
  # Make power plots for linear effect
  colnames(vs.power.linear) <- colnames(vs.power.nonlinear) <-  
    colnames(vs.power.int) <- colnames(vs.power.sin) <- 
    c("beta", "GAM (HT)", "GAM (VS)", "BKMR (0.50)", "BKMR (0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
      "ENET", "GLM")
  
  use.cols.vs <- c("beta", "GAM (HT)", "GAM (VS)", "BKMR (0.50)", "BKMR (0.95)",# "SSGAM (0.50)", #"SSGAM (0.95)", 
                   "ENET", "GLM")
  
  vs.lin <- make_power_plot_vs(vs.power.linear[,use.cols.vs], title="Linear exposure-response function", 
                               smooth=FALSE)
  vs.nonlin <- make_power_plot_vs(vs.power.nonlinear[,use.cols.vs], title="Nonlinear exposure-response function",
                                  smooth=FALSE)
  vs.int <- make_power_plot_vs(vs.power.int[,use.cols.vs], title="Interactive exposure-response function", 
                               smooth=FALSE)
  
  vs.sin <- make_power_plot_vs(vs.power.sin[,use.cols.vs], title="Sinusoid exposure-response function", 
                               smooth=FALSE)
  
  vs.lin <- vs.lin +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))#+
  # xlim(0, .8)
  
  vs.nonlin <- vs.nonlin +
    scale_color_brewer(type="qual", palette="Dark2")+
    guides(colour = guide_legend(override.aes = list(linewidth=2)))#+
  # xlim(0, .3)
  
  vs.int <- vs.int +
    scale_color_brewer(type="qual", palette="Dark2")+
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  vs.sin <- vs.sin +
    scale_color_brewer(type="qual", palette="Dark2")+
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  
  # Combine into grid
  vs.fig <- ggarrange(vs.lin, vs.nonlin, vs.int, vs.sin, common.legend=TRUE, legend="bottom")
  
  # define overall plot title
  vs.title <- ggdraw() + 
    draw_label(
      bquote(Power~as~a~`function`~of~beta*":"~componentwise~tests*";"~rho==.(rho.sim)*","~p==.(p.sim)*","~n==.(n.sim)),
      fontfamily = 'serif',
      x = 0,
      hjust = 0,
      size=40
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  # arrange title and grid
  vs.plot <- plot_grid(
    vs.title, vs.fig,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  # vs.plot
  
  ################################################################################
  #                                Index methods                                 #
  ################################################################################
  
  
  
  # c("beta", "GAM (HT)", "GAM (VS)", "BKMR (0.50)", "BKMR (0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
  # "ENET", "OLS")
  
  # Need to update to include OLS when rerun finished
  colnames(index.power.linear) <- colnames(index.power.nonlinear) <-
    colnames(index.power.interactive)<- colnames(index.power.sin) <-
    c("beta", "GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)", "GAM (VS)", "BKMR (Contrast)", 
      "BKMR (Hier., 0.50)", "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
      "ENET", "PCR", "WQS", "WQS (Neg.)", "QGC", "TEV", "GLM")
  
  if(!is.null(opposite.output)){
    colnames(index.power.opposite) <- colnames(index.power.interactive)
  }
  
  use.cols <- c("beta", "GAM (HT, bonf.)", "BKMR (Contrast)",  "BKMR (Hier., 0.95)", "GLM",
                 "WQS", "QGC")
  
  index.lin <- make_power_plot_index(index.power.linear[,use.cols], title="Linear exposure-response function", smooth=FALSE) +
    # xlim(0, .8) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  
  index.nonlin <- make_power_plot_index(index.power.nonlinear[,use.cols], title="Nonlinear exposure-response function", smooth=FALSE) +
    # xlim(0, .3) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  index.int <- make_power_plot_index(index.power.interactive[,use.cols], title="Interactive exposure-response function", smooth=FALSE) +
    # xlim(0, .3) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  index.sin <- make_power_plot_index(index.power.sin[,use.cols], title="Sinusoid exposure-response function", smooth=FALSE) +
    # xlim(0, .3) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  if(!is.null(opposite.output)){
  index.opp <- make_power_plot_index(index.power.opposite[,use.cols], title=bquote(Possibly~ opposing~effects~beta[2]==1), smooth=FALSE) +
    # xlim(0, .3) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2))) +
    xlab(bquote(beta[1]))
  }else{
    index.opp <- NULL
  }
  
  index.fig <- ggarrange(index.lin, index.nonlin, index.int, index.sin,
                         # index.opp, 
                         common.legend=TRUE, legend="bottom", font.label=list(size=50))
  
  # define overall plot title
  index.title <- ggdraw() + 
    draw_label(
      bquote(Power~as~a~`function`~of~beta*":"~whole~mixture~tests*";"~rho==.(rho.sim)*","~p==.(p.sim)*","~n==.(n.sim)),
      fontfamily = 'serif',
      x = 0,
      hjust = 0,
      size=40
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  # arrange title and grid
  index.plot <- plot_grid(
    index.title, index.fig,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  # vs.lin.plot
  
  
  ################################################################################
  #                                MSE Plots                                 #
  ################################################################################
  
  
  
  # c("beta", "GAM (HT)", "GAM (VS)", "BKMR (0.50)", "BKMR (0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
  # "ENET", "OLS")
  
  # Need to update to include OLS when rerun finished
  colnames(mse.linear) <- colnames(mse.nonlinear) <-
    colnames(mse.interactive)<- colnames(mse.sin) <-
    c("beta", "GAM", "GAM (one term)", "BKMR", 
      "SSGAM", 
      "ENET",  "GLM")
  
  if(!is.null(opposite.output)){
    colnames(index.power.opposite) <- colnames(index.power.interactive)
  }
  
  use.cols.mse <- c("beta", "GAM", "BKMR", 
                    "ENET",  "GLM")
  
  mse.lin <- make_mse_plot(mse.linear[,use.cols.mse], title="Linear exposure-response function", smooth=FALSE) +
    # xlim(0, .8) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  
  mse.nonlin <- make_mse_plot(mse.nonlinear[,use.cols.mse], title="Nonlinear exposure-response function", smooth=FALSE) +
    # xlim(0, .3) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  mse.int <- make_mse_plot(mse.interactive[,use.cols.mse], title="Interactive exposure-response function", smooth=FALSE) +
    # xlim(0, .3) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  # Bad naming conventions here sorry!:
  mse.sine <- make_mse_plot(mse.sin[,use.cols.mse], title="Sinusoid exposure-response function", smooth=FALSE) +
    # xlim(0, .3) +
    scale_color_brewer(type="qual", palette="Dark2") +
    guides(colour = guide_legend(override.aes = list(linewidth=2)))
  
  if(!is.null(opposite.output)){
    mse.opp <- make_mse_plot(mse.opposite[,use.cols.nse], title=bquote(Possibly~ opposing~effects~beta[2]==1), smooth=FALSE) +
      # xlim(0, .3) +
      scale_color_brewer(type="qual", palette="Dark2") +
      guides(colour = guide_legend(override.aes = list(linewidth=2))) +
      xlab(bquote(beta[1]))
  }else{
    mse.opp <- NULL
  }
  
  mse.fig <- ggarrange(mse.lin, mse.nonlin, mse.int, mse.sine,
                         # index.opp, 
                         common.legend=TRUE, legend="bottom", font.label=list(size=50))
  
  # define overall plot title
  mse.title <- ggdraw() + 
    draw_label(
      bquote(MSE~as~a~`function`~of~beta*";"~rho==.(rho.sim)*","~p==.(p.sim)*","~n==.(n.sim)),
      fontfamily = 'serif',
      x = 0,
      hjust = 0,
      size=40
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  # arrange title and grid
  mse.plot <- plot_grid(
    mse.title, mse.fig,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  return(list(vs.plot, index.plot, mse.plot))
}

# Need to revise to take into acct relabed filenames


# n = 100

# p = 5

plots.rho0.p5.n100 <- create_plots(out.rho0.linear.p5.n100, out.rho0.nonlinear.p5.n100, 
                                   out.rho0.interactive.p5.n100, out.rho0.sin.p5.n100,
                                   rho.sim = 0, p.sim=5, n.sim=100)

png("outputs//vs_rho0_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho0.p5.n100[[1]]
dev.off()

png("outputs//idx_rho0_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho0.p5.n100[[2]]
dev.off()

png("outputs//mse_rho0_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho0.p5.n100[[3]]
dev.off()


plots.rho5.p5.n100 <- create_plots(out.rho5.linear.p5.n100, out.rho5.nonlinear.p5.n100, 
                                   out.rho5.interactive.p5.n100, out.rho5.sin.p5.n100, 
                                   rho.sim = .5, p.sim=5, n.sim=100)

png("outputs//vs_rho5_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho5.p5.n100[[1]]
dev.off()

png("outputs//idx_rho5_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho5.p5.n100[[2]]
dev.off()

png("outputs//mse_rho5_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho5.p5.n100[[3]]
dev.off()


plots.rho9.p5.n100 <- create_plots(out.rho9.linear.p5.n100, out.rho9.nonlinear.p5.n100, 
                                   out.rho9.interactive.p5.n100, out.rho9.sin.p5.n100,
                                   rho.sim = .9, p.sim=5, n.sim=100)

png("outputs//vs_rho9_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho9.p5.n100[[1]]
dev.off()

png("outputs//idx_rho9_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho9.p5.n100[[2]]
dev.off()

png("outputs//mse_rho9_p5_n_100_plot.png", width = 1500, height = 1200)
plots.rho9.p5.n100[[3]]
dev.off()
# n = 100

# p = 10

plots.rho0.p10.n100 <- create_plots(out.rho0.linear.p10.n100, out.rho0.nonlinear.p10.n100, 
                                    out.rho0.interactive.p10.n100, out.rho0.sin.p10.n100,
                                    rho.sim = 0, p.sim=10, n.sim=100)

png("outputs//vs_rho0_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho0.p10.n100[[1]]
dev.off()

png("outputs//idx_rho0_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho0.p10.n100[[2]]
dev.off()

png("outputs//mse_rho0_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho0.p10.n100[[3]]
dev.off()

# plots.rho5.p10.n100 <- create_plots(out.rho5.linear.p10.n100, out.rho5.nonlinear.p10.n100, out.rho5.interactive.p10.n100, opposite.output=out.rho5.opposite.p10.n100, rho.sim = .5, p.sim=10, n.sim=100)
# 
# png("outputs//vs_rho5_p10_n_100_plot.png", width = 1000, height = 800)
# plots.rho5.p10.n100[[1]]
# dev.off()
# 
# png("outputs//idx_rho5_p10_n_100_plot.png", width = 1000, height = 800)
# plots.rho5.p10.n100[[2]]
# dev.off()

# Removing the opposite plot since it has diff scales, etc.

plots.rho5.p10.n100 <- create_plots(out.rho5.linear.p10.n100, out.rho5.nonlinear.p10.n100, 
                                    out.rho5.interactive.p10.n100, out.rho5.sin.p10.n100,
                                    opposite.output=NULL, rho.sim = .5, p.sim=10, n.sim=100)

png("outputs//vs_rho5_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho5.p10.n100[[1]]
dev.off()

png("outputs//idx_rho5_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho5.p10.n100[[2]]
dev.off()

png("outputs//mse_rho5_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho5.p10.n100[[3]]
dev.off()

index.power.opp <- out.rho5.opposite.p10.n100 %>%
  as.data.frame() %>%
  select(beta, starts_with("idx."), -idx.ENET_alpha)

colnames(index.power.opp) <-  c("beta", "GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)", "GAM (VS)", "BKMR (Contrast)", 
    "BKMR (Hier., 0.50)", "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
    "ENET", "PCR", "WQS", "WQS (Neg.)", "QGC", "TEV", "GLM")

use.cols <- c("beta", "GAM (HT, bonf.)", "BKMR (Contrast)",  "BKMR (Hier., 0.95)", "GLM",
              "WQS", "QGC")

index.opp <- make_power_plot_index(index.power.opp[,use.cols], title=bquote(Possibly~ opposing~effects~beta[2]==1), smooth=FALSE) +
  # xlim(0, .3) +
  scale_color_brewer(type="qual", palette="Dark2") +
  guides(colour = guide_legend(override.aes = list(linewidth=2))) +
  xlab(bquote(beta[1]))

png("outputs//idx_rho5_p10_n_100_opp_plot.png", width = 800, height = 640)
index.opp
dev.off()

plots.rho9.p10.n100 <- create_plots(out.rho9.linear.p10.n100, out.rho9.nonlinear.p10.n100, 
                                    out.rho9.interactive.p10.n100, out.rho9.sin.p10.n100,
                                    rho.sim = .9, p.sim=10, n.sim=100)

png("outputs//vs_rho9_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho9.p10.n100[[1]]
dev.off()

png("outputs//idx_rho9_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho9.p10.n100[[2]]
dev.off()

png("outputs//mse_rho9_p10_n_100_plot.png", width = 1500, height = 1200)
plots.rho9.p10.n100[[3]]
dev.off()
# n = 400

# p = 5

plots.rho0.p5.n400 <- create_plots(out.rho0.linear.p5.n400, out.rho0.nonlinear.p5.n400, 
                                   out.rho0.interactive.p5.n400, out.rho0.sin.p5.n400,
                                   rho.sim = 0, p.sim=5, n.sim=400)

png("outputs//vs_rho0_p5_n_400_plot.png", width = 1500, height = 1200)
plots.rho0.p5.n400[[1]]
dev.off()

png("outputs//idx_rho0_p5_n_400_plot.png", width = 1500, height = 1200)
plots.rho0.p5.n400[[2]]
dev.off()

png("outputs//mse_rho0_p5_n_400_plot.png", width = 1500, height = 1200)
plots.rho0.p5.n400[[3]]
dev.off()

plots.rho5.p5.n400 <- create_plots(out.rho5.linear.p5.n400, out.rho5.nonlinear.p5.n400, 
                                   out.rho5.interactive.p5.n400, out.rho5.sin.p5.n400,
                                   rho.sim = .5, p.sim=5, n.sim=400)

png("outputs//vs_rho5_p5_n_400_plot.png", width = 1500, height = 1200)
plots.rho5.p5.n400[[1]]
dev.off()

png("outputs//idx_rho5_p5_n_400_plot.png", width = 1500, height = 1200)
plots.rho5.p5.n400[[2]]
dev.off()

png("outputs//mse_rho5_p5_n_400_plot.png", width = 1500, height = 1200)
plots.rho5.p5.n400[[3]]
dev.off()

############################## Dense plot ######################################
use.cols <- c("beta", "GAM (HT, bonf.)", "BKMR (Contrast)",  "BKMR (Hier., 0.95)", "GLM",
              "WQS", "QGC")
dense_idx_p10 <- out.rho5.dense.p10.n100 %>%
  as.data.frame() %>%
  select(beta, starts_with("idx."), -idx.ENET_alpha)
colnames(dense_idx_p10) <-
  c("beta", "GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)", "GAM (VS)", "BKMR (Contrast)", 
    "BKMR (Hier., 0.50)", "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
    "ENET", "PCR", "WQS", "WQS (Neg.)", "QGC", "TEV", "GLM")

dense_p10_plot <- make_power_plot_index(dense_idx_p10[,use.cols], "Dense", smooth=FALSE) +
  theme(text = element_text(size = 15))


png("outputs//idx_rho5_p10_dense_plot.png", width = 800, height = 640)
dense_p10_plot
dev.off()

dense_mse_p10 <- out.rho5.dense.p10.n100 %>%
  as.data.frame() %>%
  select(beta, starts_with("mse."))
colnames(dense_mse_p10) <- c("beta", "GAM", "GAM (big)", "BKMR", "SSGAM", "ENET", "GLM")
use.cols.mse <- c("beta", "GAM", "BKMR", "ENET", "GLM")


dense_p10_mse_plot <- make_mse_plot(dense_mse_p10[,use.cols.mse], "Dense MSE", smooth=FALSE) +
  theme(text=element_text(size=15))


png("outputs//mse_rho5_p10_dense_plot.png", width = 800, height = 640)
dense_p10_mse_plot
dev.off()


dense_idx_p20 <- out.rho5.dense.p20.n100 %>%
  as.data.frame() %>%
  select(beta, starts_with("idx."), -idx.ENET_alpha)
colnames(dense_idx_p20) <-
  c("beta", "GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)", "GAM (VS)", "BKMR (Contrast)", 
    "BKMR (Hier., 0.50)", "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
    "ENET", "PCR", "WQS", "WQS (Neg.)", "QGC", "TEV", "GLM")
dense_p20_plot <- make_power_plot_index(dense_idx_p20[,use.cols], "Dense", smooth=FALSE) +
  theme(text = element_text(size = 15))

png("outputs//idx_rho5_p20_dense_plot.png", width = 800, height = 640)
dense_p20_plot
dev.off()

dense_mse_p20 <- out.rho5.dense.p20.n100 %>%
  as.data.frame() %>%
  select(beta, starts_with("mse."))
colnames(dense_mse_p20) <- c("beta", "GAM", "GAM (big)", "BKMR", "SSGAM", "ENET", "GLM")
use.cols.mse <- c("beta", "GAM", "BKMR", "ENET", "GLM")


dense_p20_mse_plot <- make_mse_plot(dense_mse_p20[,use.cols.mse], "Dense MSE", smooth=FALSE) +
  theme(text=element_text(size=15))


png("outputs//mse_rho5_p20_dense_plot.png", width = 800, height = 640)
dense_p20_mse_plot
dev.off()


################################################################################
#           Tables code                                                        #
################################################################################

create_tables <- function(linear.output, nonlinear.output, p.sim, rho.sim, n.sim,
                          interactive.output, sin.output, opposite.output=NULL){
  
  ################################################################################
  #                        Extract data                                          #
  ################################################################################
  
  ########################    Linear scenarios         ###########################
  
  vs.linear <- linear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  idx.linear <- linear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  mse.linear <- linear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  
  #######################    Nonlinear scenarios         #########################

  vs.nonlinear <- nonlinear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  idx.nonlinear <- nonlinear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  mse.nonlinear <- nonlinear.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  #######################    Interactive scenarios         #########################
  
  vs.interactive <- interactive.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  idx.interactive <- interactive.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  mse.interactive <- interactive.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  #######################    Sine scenarios         #########################
  
  vs.sin <- sin.output %>%
    as.data.frame() %>%
    select(beta, starts_with("vs."), -vs.ENET_alpha)
  
  idx.sin <- sin.output %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  mse.sin <- sin.output %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  #############################  Opposite scenario      ########################
  if(!is.null(opposite.output)){
    idx.opposite <- opposite.output %>%
      as.data.frame() %>%
      select(beta, starts_with("idx."), -idx.ENET_alpha)
    
    mse.opposite <- opposite.output %>%
      as.data.frame() %>%
      select(beta, starts_with("mse"))
  }else{
    idx.opposite <- NULL
    mse.opposite <- NULL
  }
  
  colnames(vs.linear) <- colnames(vs.nonlinear) <-  
    colnames(vs.interactive) <- colnames(vs.sin) <-
    c("beta", "GAM (HT)", "GAM (VS)", "BKMR (0.50)", "BKMR (0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
      "ENET", "GLM")
  
  use.cols.vs <- c("beta", "GAM (HT)", "GAM (VS)", "BKMR (0.50)", "BKMR (0.95)",# "SSGAM (0.50)", #"SSGAM (0.95)", 
                   "ENET", "GLM")
  
  
  colnames(idx.linear) <- colnames(idx.nonlinear) <-
    colnames(idx.interactive)<- colnames(idx.sin) <-
    c("beta", "GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)", "GAM (VS)", "BKMR (Contrast)", 
      "BKMR (Hier., 0.50)", "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
      "ENET", "PCR", "WQS", "WQS (Neg.)", "QGC", "TEV", "GLM")
  
  if(!is.null(opposite.output)){
    colnames(idx.opposite) <- colnames(idx.interactive)
  }
  
  use.cols <- c("beta", "GAM (HT, bonf.)", "BKMR (Contrast)",  "BKMR (Hier., 0.95)", "GLM", "PCR",
                "WQS", "QGC")
  ################################################################################
  #                                   Tables                                     #
  ################################################################################
  
  ##################
  #  Var sel.      #
  ##################
  
  # summmarize data
  summ.vs.lin <- summ_data(vs.linear[,use.cols.vs])
  summ.vs.nonlinear <- summ_data(vs.nonlinear[,use.cols.vs])
  summ.vs.interactive <- summ_data(vs.interactive[,use.cols.vs])
  summ.vs.sin <- summ_data(vs.sin[,use.cols.vs])
  
  # # pick rows to make tables of
  long.summ.vs.lin <- summ.vs.lin[2:nrow(summ.vs.lin),] %>% # to avoid selecting beta=0
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value)

  powerdiffs.lin.vs <- long.summ.vs.lin %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.lin.vs <- which.max(powerdiffs.lin.vs$diff) + 1 # bc excluded beta=0
  
  long.summ.vs.nonlin <- summ.vs.nonlinear[2:nrow(summ.vs.nonlinear),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value)
  
  powerdiffs.nonlin.vs <- long.summ.vs.nonlin %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.nonlin.vs <- which.max(powerdiffs.nonlin.vs$diff) + 1
  
  long.summ.vs.int <- summ.vs.interactive[2:nrow(summ.vs.interactive),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value)
  
  powerdiffs.int.vs <- long.summ.vs.int %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.int.vs <- which.max(powerdiffs.int.vs$diff) + 1
  
  long.summ.vs.sin <- summ.vs.sin[2:nrow(summ.vs.sin),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value)
  
  powerdiffs.sin.vs <- long.summ.vs.sin %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.sin.vs <- which.max(powerdiffs.sin.vs$diff) + 1
  
  # vs.rownames <- c("GAM (HT)", "GAM (VS)", "BKMR (0.50)", "BKMR (0.95)", "SSGAM (0.50)",
  #                  "SSGAM (0.95)", "ENET", "OLS")
  # Create size row
  ncol.vs <- ncol(summ.vs.lin)
  size.vs <- rbind(summ.vs.lin[1,], 
                            summ.vs.nonlinear[1,],
                            summ.vs.interactive[1,],
                            summ.vs.sin[1,]) %>%
    t() %>%
    as.data.frame() %>%
    rowMeans() %>%
    round(5)
  # colnames(size.vs) <- c("Linear", "Nonlinear", "Interactive")
  # rownames(size.vs) <- vs.rownames
  # size.vs
  
  # Create power rows and combine with size row
  power.vs <- rbind(summ.vs.lin[label.row.lin.vs,],
                    summ.vs.nonlinear[label.row.nonlin.vs,],
                    summ.vs.interactive[label.row.int.vs,],
                    summ.vs.sin[label.row.sin.vs,],
                    size.vs) %>%
    t() %>%
    as.data.frame() %>%
    round(5)
  
  # Combine and format output table
  colnames(power.vs) <- c("Lin.", "N.L", "Int.", "Sine", "None")
  rownames(power.vs)[1] <- "beta"#c("beta", vs.rownames)
  power.vs <- cbind( rep(n.sim, 5), rep(p.sim, 5), rep(rho.sim, 5),  t(power.vs))
  colnames(power.vs)[1:3] <- c("n", "p", "rho")
  power.vs
  
  #####################################
  #   Index (whole-mixture) methods   #
  #####################################
  
  # summmarize data
  summ.index.lin <- summ_data(idx.linear[,use.cols])
  summ.index.nonlinear <- summ_data(idx.nonlinear[,use.cols])
  summ.index.interactive <- summ_data(idx.interactive[,use.cols])
  summ.index.sin <- summ_data(idx.sin[,use.cols])
  summ.index.opposite <- NULL
  if(!is.null(opposite.output)){
    summ.index.opposite <- summ_data(idx.opposite[,use.cols])
  }
  
  # pick rows to make tables of
  long.summ.idx.lin <- summ.index.lin[2:nrow(summ.index.lin),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))

  powerdiffs.lin.idx <- long.summ.idx.lin %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))

  label.row.lin.idx <- which.max(powerdiffs.lin.idx$diff) + 1

  long.summ.idx.nonlin <- summ.index.nonlinear[2:nrow(summ.index.nonlinear),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))

  powerdiffs.nonlin.idx <- long.summ.idx.nonlin %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))

  label.row.nonlin.idx <- which.max(powerdiffs.nonlin.idx$diff) + 1

  long.summ.idx.int <- summ.index.interactive[2:nrow(summ.index.interactive),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))

  powerdiffs.int.idx <- long.summ.idx.int %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))

  label.row.int.idx <- which.max(powerdiffs.int.idx$diff) + 1
  
  long.summ.idx.sin <- summ.index.sin[2:nrow(summ.index.sin),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.sin.idx <- long.summ.idx.sin %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.sin.idx <- which.max(powerdiffs.sin.idx$diff) + 1
  
  # 
  if(!is.null(opposite.output)){
  long.summ.idx.opp <- summ.index.opposite[2:nrow(summ.index.opposite),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.opp.idx <- long.summ.idx.opp %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.opp.idx <- which.max(powerdiffs.opp.idx$diff) + 1
  }else{
    label.row.opp.idx <- NULL
  }
  # index.rownames <- c("GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)",
  #                     "GAM (VS)", "BKMR (Contrast)", "BKMR (Hier., 0.50)",
  #                     "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)",
  #                     "ENET", "PCR", "WQS (Pos.)", "WQS (Neg.)", "QGC", "TEV",
  #                     "OLS"
  # )
  
  # Create size row
  ncol.index <- ncol(summ.index.lin)
  size.index <- rbind(
    summ.index.lin[1, ],
    summ.index.nonlinear[1, ],
    summ.index.interactive[1, ],
    summ.index.sin[1,]) %>%
    t() %>%
    as.data.frame() %>%
    rowMeans() %>%
    round(5)
  # colnames(size.index) <- c("Linear", "Nonlinear", "Interactive")
  # rownames(size.index) <- index.rownames
  size.index
  
  # Create power rows and combine with size row
  power.index <- rbind(
    summ.index.lin[label.row.lin.idx,],
    summ.index.nonlinear[label.row.nonlin.idx,],
    summ.index.interactive[label.row.int.idx,],
    summ.index.sin[label.row.sin.idx,],
    summ.index.opposite[label.row.opp.idx,] ,
    size.index
    
  ) %>%
    t() %>%
    as.data.frame() %>%
    round(5)
  
  # Combine and format output table
  if(!is.null(opposite.output)){
    colnames(power.index) <- c("Lin.", "N.L.", "Int.","Sine",  "Opp.", "None")
    rownames(power.index)[1] <-"beta"# c("beta", index.rownames)
    power.index <- cbind(rep(n.sim, 6), rep(p.sim, 6), rep(rho.sim, 6), t(power.index))
  }else{
    colnames(power.index) <- c("Lin.", "N.L.", "Int.", "Sine", "None")
    rownames(power.index)[1] <- "beta"# c("beta", index.rownames)
    power.index <- cbind(rep(n.sim, 5), rep(p.sim, 5), rep(rho.sim, 5), t(power.index))
  }
  colnames(power.index)[1:3] <- c("n", "p", "rho")
  power.index
  
  #####################################
  #             MSE Tables            #
  #####################################
  
  use.cols.mse <- c("beta", "mse.GAM.add", "mse.BKMR",# "mse.SSGAM",
                    "mse.ENET", "mse.OLS")
  # summmarize data
  summ.mse.lin <- summ_data(mse.linear[,use.cols.mse])
  summ.mse.nonlinear <- summ_data(mse.nonlinear[,use.cols.mse])
  summ.mse.interactive <- summ_data(mse.interactive[,use.cols.mse])
  summ.mse.sin <- summ_data(mse.sin[,use.cols.mse])
  summ.mse.opposite <- NULL
  if(!is.null(opposite.output)){
    summ.mse.opposite <- summ_data(mse.opposite[,use.cols.mse])
  }
  
  # pick rows to make tables of
  long.summ.mse.lin <- summ.mse.lin[2:nrow(summ.mse.lin),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.lin.mse <- long.summ.mse.lin %>%
    group_by(beta1) %>%
    summarize(sd = sd(Power))
  
  label.row.lin.mse <- which.max(powerdiffs.lin.mse$sd) + 1
  
  long.summ.mse.nonlin <- summ.mse.nonlinear[2:nrow(summ.mse.nonlinear),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.nonlin.mse <- long.summ.mse.nonlin %>%
    group_by(beta1) %>%
    summarize(sd = sd(Power))
  
  label.row.nonlin.mse <- which.max(powerdiffs.nonlin.mse$sd) + 1
  
  long.summ.mse.int <- summ.mse.interactive[2:nrow(summ.mse.interactive),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.int.mse <- long.summ.mse.int %>%
    group_by(beta1) %>%
    summarize(sd = sd(Power))
  
  label.row.int.mse <- which.max(powerdiffs.int.mse$sd) + 1
  
  long.summ.mse.sin <- summ.mse.sin[2:nrow(summ.mse.sin),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.sin.mse <- long.summ.mse.sin %>%
    group_by(beta1) %>%
    summarize(sd = sd(Power))
  
  label.row.sin.mse <- which.max(powerdiffs.sin.mse$sd) + 1
  
  
  # 
  if(!is.null(opposite.output)){
    long.summ.mse.opp <- summ.mse.opposite[2:nrow(summ.mse.opposite),] %>%
      pivot_longer(-beta1) %>%
      rename(Method=name, Power=value) %>%
      filter(!is.na(Power))
    
    powerdiffs.opp.mse <- long.summ.mse.opp %>%
      group_by(beta1) %>%
      summarize(sd = sd(Power))
    
    label.row.opp.mse <- which.max(powerdiffs.opp.mse$sd) + 1
  }else{
    label.row.opp.mse <- NULL
  }
  mse.rownames <- c("GAM", "BKMR",# "SSGAM",
                    "ENET", "GLM" )
  
  # Create size row
  ncol.mse <- ncol(summ.mse.lin)
  null.mse <- rbind(
    summ.mse.lin[1, ],
    summ.mse.nonlinear[1, ],
    summ.mse.interactive[1, ],
    summ.mse.sin[1,]) %>%
    t() %>%
    as.data.frame() %>%
    rowMeans() %>%
    round(5)
  # colnames(size.index) <- c("Linear", "Nonlinear", "Interactive")
  # rownames(size.index) <- index.rownames
  null.mse
  
  # Create power rows and combine with size row
  mse.table <- rbind(
    summ.mse.lin[label.row.lin.mse,],
    summ.mse.nonlinear[label.row.nonlin.mse,],
    summ.mse.interactive[label.row.int.mse,],
    summ.mse.sin[label.row.sin.mse,],
    summ.mse.opposite[label.row.opp.mse,] ,
    null.mse
    
  ) %>%
    t() %>%
    as.data.frame() %>%
    round(5)
  
  # Combine and format output table
  if(!is.null(opposite.output)){
    colnames(mse.table) <- c("Lin.", "N.L.", "Int.", "Sine", "Opp.", "None")
    rownames(mse.table) <- c("beta", mse.rownames)
    mse.table <- cbind(rep(n.sim, 6), rep(p.sim, 6), rep(rho.sim, 6), t(mse.table))
  }else{
    colnames(mse.table) <- c("Lin.", "N.L.", "Int.", "Sine", "None")
    rownames(mse.table) <- c("beta", mse.rownames)
    mse.table <- cbind(rep(n.sim, 5), rep(p.sim, 5), rep(rho.sim, 5), t(mse.table))
  }
  colnames(mse.table)[1:3] <- c("n", "p", "rho")
  mse.table
  
  return(list(power.vs=power.vs, power.index=power.index, mse.table=mse.table))
  
}

create_tables_dense <- function(dense.output.1, dense.output.2, p.sim.1, p.sim.2, rho.sim.1, rho.sim.2, n.sim){
  
  ################################################################################
  #                        Extract data                                          #
  ################################################################################
  
 
  idx.1 <- dense.output.1 %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  idx.2 <- dense.output.2 %>%
    as.data.frame() %>%
    select(beta, starts_with("idx."), -idx.ENET_alpha)
  
  mse.1 <- dense.output.1 %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  mse.2 <- dense.output.2 %>%
    as.data.frame() %>%
    select(beta, starts_with("mse"))
  
  colnames(idx.1) <- colnames(idx.2) <-
    c("beta", "GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)", "GAM (VS)", "BKMR (Contrast)", 
      "BKMR (Hier., 0.50)", "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)", 
      "ENET", "PCR", "WQS", "WQS (Neg.)", "QGC", "TEV", "GLM")
  
  
  use.cols <- c("beta", #"GAM (HT, bonf.)", 
                "BKMR (Contrast)",  "BKMR (Hier., 0.95)", "GLM", "PCR",
                "WQS", "QGC")
  ################################################################################
  #                                   Tables                                     #
  ################################################################################
  
  #####################################
  #   Index (whole-mixture) methods   #
  #####################################
  
  # summmarize data
  summ.index.1 <- summ_data(idx.1[,use.cols])
  summ.index.2 <- summ_data(idx.2[,use.cols])
  
  
  # pick rows to make tables of
  long.summ.idx.1 <- summ.index.1[2:nrow(summ.index.1),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.idx.1 <- long.summ.idx.1 %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.idx.1 <- which.max(powerdiffs.idx.1$diff) + 1
  
  long.summ.idx.2 <- summ.index.2[2:nrow(summ.index.2),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.idx.2 <- long.summ.idx.2 %>%
    group_by(beta1) %>%
    summarize(diff = max(Power) - min(Power))
  
  label.row.idx.2 <- which.max(powerdiffs.idx.2$diff) + 1
  
  # index.rownames <- c("GAM (HT, bonf.)", "GAM (HT, one term)", "GAM (HT, F)",
  #                     "GAM (VS)", "BKMR (Contrast)", "BKMR (Hier., 0.50)",
  #                     "BKMR (Hier., 0.95)", "SSGAM (0.50)", "SSGAM (0.95)",
  #                     "ENET", "PCR", "WQS (Pos.)", "WQS (Neg.)", "QGC", "TEV",
  #                     "OLS"
  # )
  
  # Create size row
  ncol.index <- ncol(summ.index.1)
  size.index.1 <- summ.index.1[1, ] %>%
    t() %>%
    as.data.frame() %>%
    rowMeans() %>%
    round(5)
  # colnames(size.index) <- c("Linear", "Nonlinear", "Interactive")
  # rownames(size.index) <- index.rownames
  # size.index
  size.index.2 <- summ.index.2[1, ] %>%
    t() %>%
    as.data.frame() %>%
    rowMeans() %>%
    round(5)
  
  # Create power rows and combine with size row
  power.index <- rbind(
    summ.index.1[label.row.idx.1,],
    summ.index.2[label.row.idx.2,],
    size.index.1,
    size.index.2
    
  ) %>%
    t() %>%
    as.data.frame() %>%
    round(5)
  
  # Combine and format output table
  
  colnames(power.index) <- c(p.sim.1, p.sim.2, p.sim.1, p.sim.2)
  rownames(power.index)[1] <- "beta"# c("beta", index.rownames)
  power.index <- cbind(rep(n.sim, 4), c(p.sim.1, p.sim.2, p.sim.1, p.sim.2), 
                       c(rho.sim.1, rho.sim.2, rho.sim.1, rho.sim.2), t(power.index))
  
  colnames(power.index)[1:3] <- c("n", "p", "rho")
  power.index
  
  #####################################
  #             MSE Tables            #
  #####################################
  
  use.cols.mse <- c("beta", "mse.GAM.add", "mse.BKMR",# "mse.SSGAM",
                    "mse.ENET", "mse.OLS")
  # summmarize data
  summ.mse.1 <- summ_data(mse.1[,use.cols.mse])
  summ.mse.2 <- summ_data(mse.2[,use.cols.mse])

  # pick rows to make tables of
  long.summ.mse.1 <- summ.mse.1[2:nrow(summ.mse.1),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.1.mse <- long.summ.mse.1 %>%
    group_by(beta1) %>%
    summarize(sd = sd(Power))
  
  label.row.1.mse <- which.max(powerdiffs.1.mse$sd) + 1
  
  long.summ.mse.2 <- summ.mse.2[2:nrow(summ.mse.2),] %>%
    pivot_longer(-beta1) %>%
    rename(Method=name, Power=value) %>%
    filter(!is.na(Power))
  
  powerdiffs.2.mse <- long.summ.mse.2 %>%
    group_by(beta1) %>%
    summarize(sd = sd(Power))
  
  label.row.2.mse <- which.max(powerdiffs.2.mse$sd) + 1
  
  mse.rownames <- c("GAM", "BKMR",# "SSGAM",
                    "ENET", "GLM" )
  
  # Create size row
  ncol.mse.1 <- ncol(summ.mse.1)
  null.mse.1 <- rbind(
    summ.mse.1[1, ]) %>%
    t() %>%
    as.data.frame() %>%
    rowMeans() %>%
    round(5)
  # colnames(size.index) <- c("Linear", "Nonlinear", "Interactive")
  # rownames(size.index) <- index.rownames
  null.mse.1
  
  null.mse.2 <- rbind(
    summ.mse.2[1, ]) %>%
    t() %>%
    as.data.frame() %>%
    rowMeans() %>%
    round(5)
  # colnames(size.index) <- c("Linear", "Nonlinear", "Interactive")
  # rownames(size.index) <- index.rownames
  null.mse.2
  
  # Create power rows and combine with size row
  mse.table <- rbind(
    summ.mse.1[label.row.1.mse,],
    summ.mse.2[label.row.2.mse,],
    null.mse.1,
    null.mse.2
    
  ) %>%
    t() %>%
    as.data.frame() %>%
    round(5)
  
  # Combine and format output table
  
  colnames(mse.table) <- c("1", "2", "None", "None")
  rownames(mse.table) <- c("beta", mse.rownames)
  mse.table <- cbind(rep(n.sim, 4), c(p.sim.1, p.sim.2, p.sim.1, p.sim.2), 
                     c(rho.sim.1, rho.sim.2, rho.sim.1, rho.sim.2), t(mse.table))

  colnames(mse.table)[1:3] <- c("n", "p", "rho")
  mse.table
  
  
  return(list(power.index=power.index, mse.table=mse.table))
  
}

create_tables_dense(out.rho5.dense.p10.n100, out.rho5.dense.p20.n100, 10, 20, .5, .5, 100)
out.rho5.dense.p10.n100

# Need to update for new object labeling
create_tables(out.rho0.linear.p5.n100, out.rho0.nonlinear.p5.n100, 
              out.rho0.interactive.p5.n100, out.rho0.sin.p5.n100, n.sim=100,
              p.sim=5, rho.sim=0)  

create_tables(out.rho5.linear.p5.n100, out.rho5.nonlinear.p5.n100, 
              out.rho5.interactive.p5.n100, out.rho5.sin.p5.n100, n.sim=100,
              p.sim=5, rho.sim=0.5)  

create_tables(out.rho9.linear.p5.n100, out.rho9.nonlinear.p5.n100, 
              out.rho9.interactive.p5.n100, out.rho9.sin.p5.n100, n.sim=100,
              p.sim=5, rho.sim=0.9)

create_tables(out.rho0.linear.p10.n100, out.rho0.nonlinear.p10.n100,
              out.rho0.interactive.p10.n100, out.rho0.sin.p10.n100,  n.sim=100,
              p.sim=10, rho.sim=0)  

create_tables(out.rho5.linear.p10.n100, out.rho5.nonlinear.p10.n100, 
              out.rho5.interactive.p10.n100, out.rho5.sin.p10.n100,
              opposite.output=out.rho5.opposite.p10.n100, n.sim=100,
              p.sim=10, rho.sim=.5)  

create_tables(out.rho9.linear.p10.n100, out.rho9.nonlinear.p10.n100, 
              out.rho9.interactive.p10.n100, out.rho9.sin.p10.n100, n.sim=100,
              p.sim=10, rho.sim=0.9)  

create_tables(out.rho0.linear.p5.n400, out.rho0.nonlinear.p5.n400, 
              out.rho0.interactive.p5.n400, out.rho0.sin.p5.n400, n.sim=400,
              p.sim=5, rho.sim=0)  

create_tables(out.rho5.linear.p5.n400, out.rho5.nonlinear.p5.n400, 
              out.rho5.interactive.p5.n400, out.rho5.sin.p5.n400, n.sim=400,
              p.sim=5, rho.sim=0.5)  


# create_tables(out.rho5.linear, out.rho5.nonlinear, out.rho5.interactive, opposite.output=out.rho5.opposite,
#               p.sim=5, rho.sim=.5)  
# create_tables(out.rho5.linear, out.rho5.nonlinear, out.rho5.interactive,
#               p.sim=5, rho.sim=.5)  


################################################################################
#                           Null distributions                                 #
################################################################################
str(out.rho5.null.p5.n100)
head(out.rho5.null.p5.n100)
hist(out.rho5.null.p5.n100[,"vs.GAM_HT"], xlim=c(0,1), main="Null distn. of GAM p-value (one term)")
hist(out.rho5.null.p5.n100[,"vs.BKMR"], xlim=c(0,1), main="Null distn. of BKMR PIP (one term, default prior)")
hist(out.rho5.null.p5.n100[,"vs.SSGAM"], xlim=c(0,1), main="Null distn. of SSGAM PIP (one term, default prior)")
hist(out.rho5.null.p5.n100[,"vs.OLS"], xlim=c(0,1), main="Null distn. of OLS p-value (one term)")
hist(out.rho5.null.p5.n100[,"idx.GAM_HT_big"], xlim=c(0,1), main="Null distn. of GAM p-value (one big term)")
hist(out.rho5.null.p5.n100[,"idx.GAM_HT_F"], xlim=c(0,1), main="Null distn. of GAM p-value (F-test)")
hist(out.rho5.null.p5.n100[,"idx.BKMR_H"], xlim=c(0,1), main="Null distn. of BKMR PIP (Hier., group, default prior)")
hist(out.rho5.null.p5.n100[,"idx.PCR"], xlim=c(0,1), main="Null distn. of PCR p-value")
hist(out.rho5.null.p5.n100[,"idx.WQS_POS"], xlim=c(0,1), main="Null distribution of WQS (pos.) p-value")
hist(out.rho5.null.p5.n100[,"idx.WQS_NEG"], xlim=c(0,1), main="Null distribution of WQS (neg.) p-value")
hist(out.rho5.null.p5.n100[,"idx.QGC"], xlim=c(0,1), main="Null distribution of QGCOMP p-value")
hist(out.rho5.null.p5.n100[,"idx.OLS"], xlim=c(0,1), main="Null distribution of OLS p-value (F-test)")
