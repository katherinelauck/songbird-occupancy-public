##### Produce figures for songbird occupancy publication
# Author: Katherine Lauck
# Last updated: 19 May 2020
#
# This script builds figures for use in Spring 2020 Conservation Ecology term paper and eventual publication.
# 
# Dependencies
library(tidyverse)
library(rio)
library(knitr)
library('plyr')
library(dplyr)
library(magrittr)
library(boot)
library(scales)
library(gt)
library(gridExtra)

load("results/jags/ssom_2018_Vintf_2020-08-26.rdata")
colnames(res$BUGSoutput$summary)
cols<-c('mean', 'sd','2.5%','97.5%','Rhat','n.eff')
summ<-res$BUGSoutput$summary

vars<-rownames(summ)

summ[grep("mu", vars),cols]
summ[grep("sigma", vars),cols]
summ[grep("p", vars),cols]
# R2jags::traceplot(res$BUGSoutput,varname = grep('mu',vars,value = T))

summ[which(summ[,'Rhat'] >= 1.1),'Rhat']
dimnames(dd$data$X)[[2]][89]

###### graph: spp ordered by distance to road effect. Probably could be part of supplemental information but not to be used in main manuscript
###### 
fig.data <- as_tibble(summ[grep('^lambda.dr\\[',vars,value = T),c('mean','2.5%','97.5%')]) %>%
  mutate(commercial = factor(dd$data$commercial)) %>%
  arrange(mean)

fig.data %>%
  ggplot(mapping = aes(y = mean,x = seq(length(dd$data$commercial)))) +
  geom_segment(mapping = aes(y = `2.5%`,
                             yend = `97.5%`,
                             x = seq(length(dd$data$commercial)),
                             xend = seq(length(dd$data$commercial)),
                             color = commercial)) +
  geom_point() +
  # geom_boxplot()+
  # coord_flip()+
  geom_hline(mapping = aes(yintercept = 0)) +
  labs(
    x = "Species ordered by mean simulated effect of distance to roads",
    y = "Simulated value"
  ) +
  # scale_x_discrete(labels = c(seq(1:15))) +
  theme_classic() +
  theme(legend.position = "none")

# ggsave('results/graphics/dr.com.png')


##### 
# plots of mu.lambda.dr & mu.lambda.intf
#####

bayes_predict <- function(data,x,vars,type,quantile=NULL) {
  if(type == 'mu'){
    mu.lambda <- paste0("mu.lambda.",x)
    mu.lambda.com <- paste0('mu.lambda.',x,'.com')
  if(quantile %in% c('mean','2.5%','97.5%')) {
    assign(mu.lambda, data$BUGSoutput$summary[mu.lambda,quantile])
    assign(mu.lambda.com, data$BUGSoutput$summary[mu.lambda.com,quantile])
    lambda.com <- data$BUGSoutput$summary['lambda.com',quantile]
  } else {
    quantile <- as.numeric(str_extract(quantile,'\\d+\\.*\\d*'))/100
    assign(mu.lambda,quantile(data$BUGSoutput$sims.list[[mu.lambda]],
                              probs = quantile))
    assign(mu.lambda.com,quantile(data$BUGSoutput$sims.list[[mu.lambda.com]],
                              probs = quantile))
    lambda.com <- quantile(data$BUGSoutput$sims.list$lambda.com,probs = quantile)
  }
  predicted <- inv.logit(get(mu.lambda)*vars[[1]] + lambda.com*vars[[2]] + get(mu.lambda.com)*vars[[1]]*vars[[2]])
  return(predicted)
  } else if(type == 'species') {
    ### Eventually need to add in lambda.0[species] term to prediction. However, the current res do not include lambda.0 because I forgot to include it as a variable to track. DX
    lambda.x <- paste0('lambda.',x)
    lambda.x.com <- paste0('lambda.',x,'.com')
    lambda.0 <- rep(data$BUGSoutput$mean$lambda.0,101)
    assign(lambda.x, rep(data$BUGSoutput$mean[[lambda.x]],101))
    assign(lambda.x.com, rep(data$BUGSoutput$mean[[lambda.x.com]],101))
    lambda.com <- as.vector(data$BUGSoutput$mean$lambda.com)
    predicted <- inv.logit(lambda.0 + get(lambda.x)*vars[[1]] + lambda.com*vars[[2]] + 
                             get(lambda.x.com)*vars[[1]]*vars[[2]])
    return(predicted)
  } else {stop('Missing valid type. choose \"species\" or \"mu\"')}
}
bayes_predict_plot <- function(data, x, save=FALSE){
  if (x == 'dr'){
  predictor <- read.csv('results/data/siteCov/original/site.cov.csv')$distRoad
  xlab <- 'Distance to nearest road (km)'
  ylab <- 'distance to nearest road'
  } else if (x == 'intf') {
    predictor <- read.csv('results/data/siteCov/original/site.cov.csv')$forest500
    xlab <- 'Percent intact forest within 500 m'
    ylab <- 'percent intact forest within 500 m'
  } else {stop('Missing valid x. choose \"dr\" or \"intf\"')}
  fig.data.mu <- bind_cols(x.scaled = rep(seq(0,1,.01),2),
                              commercial = c(rep(0,101),rep(1,101))
  )
  
  fig.data.mu %<>%
    mutate(x.unscaled = rescale(x.scaled,c(min(predictor),max(predictor))),
           mean = bayes_predict(data,x,list(x.scaled,commercial),'mu','mean'),
           low.90 = bayes_predict(data,x,list(x.scaled,commercial),'mu','5%'),
           high.90 = bayes_predict(data,x,list(x.scaled,commercial),'mu','95%'),
           low.95 = bayes_predict(data,x,list(x.scaled,commercial),'mu','2.5%'),
           high.95 = bayes_predict(data,x,list(x.scaled,commercial),'mu','97.5%')
    )
  fig.data.mu$commercial <- factor(fig.data.mu$commercial)
  levels(fig.data.mu$commercial) <- c('Not commercially valuable','Commercially valuable')
  
  mu_90 <- fig.data.mu %>%
    ggplot(mapping = aes(x = x.unscaled, group = factor(commercial))) +
    geom_ribbon(aes(ymin = low.90, ymax = high.90,colour = factor(commercial), fill = factor(commercial)),linetype = 2, alpha = .2) +
    geom_line(aes(y = mean, color = factor(commercial)),size = 1.5) +
    theme_classic(base_size = 22) +
    labs(x = xlab, y = "Predicted occupancy probability") +
    theme(legend.position = "none") +
    scale_color_viridis_d(end = .8, aesthetics = c('color','fill'))
  
  mu_95 <- fig.data.mu %>%
    ggplot(mapping = aes(x = x.unscaled, group = factor(commercial))) +
    geom_ribbon(aes(ymin = low.95, ymax = high.95,colour = factor(commercial), fill = factor(commercial)),linetype = 2, alpha= 0.2) +
    geom_line(aes(y = mean, color = factor(commercial)),size = 1.5) +
    theme_classic(base_size = 22) + 
    labs(x =xlab, y = "Predicted occupancy probability") +
    theme(legend.position = "none") +
    scale_color_viridis_d(end = .8, aesthetics = c('color','fill'))
  
  ##### 
  # per spp effect
  # If spp driving trend, pull them out into their own graph
  # Threshold effect: could add quadratic term to model, allow to interact with commercial
  #####
  
  commercial <- dd$data$commercial %>% 
    adply(c(1))
  colnames(commercial) <- c('species','commercial')
  
  fig.data.sp <- bind_cols(species = rep(commercial$species,101),
                              x.scaled = rep(seq(0,1,.01),each = 95),
                              commercial = rep(commercial$commercial,101)
  )
  
  fig.data.sp %<>%
    mutate(x.unscaled = rescale(x.scaled,c(min(predictor),max(predictor))),
           mean = bayes_predict(data,x,list(x.scaled,commercial),type = 'species'))
  fig.data.sp$commercial <- factor(fig.data.sp$commercial)
  levels(fig.data.sp$commercial) <- c('Not commercially valuable','Commercially valuable')
  
  per.sp.mu <- fig.data.sp %>%
    ggplot(mapping = aes(x = x.unscaled, group = species)) +
    coord_cartesian(xlim = c(min(fig.data.sp$x.unscaled,na.rm = TRUE),max(fig.data.sp$x.unscaled,na.rm = TRUE)),
                    ylim = c(min(fig.data.sp$mean,na.rm = TRUE),max(fig.data.sp$mean,na.rm = TRUE))) +
    #geom_line(aes(y = mean, color = factor(commercial)), alpha = .2,size = 1.2) +
    geom_line(data = fig.data.mu,aes(y = mean,x = x.unscaled, group = factor(commercial), color = factor(commercial)),size = 2) +
    theme_classic(base_size = 22) +
    labs(x =xlab, y = "Predicted occupancy probability") +
    theme(legend.position = "top", legend.title = element_blank()) +
    #scale_color_discrete(labels = c("Not commercially valuable", "Commercially valuable")) +
    scale_color_viridis_d(end = .8, aesthetics = c('color','fill')) 
  
  per.sp.full <- fig.data.sp %>%
    ggplot(mapping = aes(x = x.unscaled, group = species)) +
    coord_cartesian(xlim = c(min(fig.data.sp$x.unscaled,na.rm = TRUE),max(fig.data.sp$x.unscaled,na.rm = TRUE)),
                    ylim = c(min(fig.data.sp$mean,na.rm = TRUE),max(fig.data.sp$mean,na.rm = TRUE))) +
    geom_line(aes(y = mean, color = factor(commercial),alpha = factor(commercial)),size = 1.1) +
    scale_alpha_discrete(range = c(0.1, 0.4)) +
    geom_line(data = fig.data.mu,aes(y = mean,x = x.unscaled, group = factor(commercial),color = factor(commercial)),size = 2) +
    theme_classic(base_size = 22) +
    labs(x =xlab, y = "Predicted occupancy probability") +
    theme(legend.position = "top",legend.title = element_blank()) +
    #scale_color_discrete(labels = c("Not commercially valuable", "Commercially valuable")) +
    scale_color_viridis_d(end = .8, aesthetics = c('color','fill'))
  
  fig.data.bci <- bind_cols(commercial = c(rep(0,1400),rep(1,1400)),
                            est = c(c(data$BUGSoutput$sims.list[[paste0('mu.lambda.',x)]]),
                                    c(data$BUGSoutput$sims.list[[paste0('mu.lambda.',x)]])+
                              c(data$BUGSoutput$sims.list[[paste0('mu.lambda.',x,'.com')]])))
  
  bci <- fig.data.bci %>%
    ggplot(mapping = aes(x = factor(commercial), y = est, fill = factor(commercial))) +
    geom_boxplot() +
    scale_x_discrete(labels=c("0" = "Not commercially valuable", "1" = "Commercially valuable")) +
    theme_classic(base_size = 22) +
    labs(x = NULL,y = paste0('Estimated effect of ',ylab)) +
    theme(legend.position = "none") +
    scale_color_viridis_d(end = .8, aesthetics = c('color','fill'))
  
  mu <- ddply(fig.data.bci, "commercial", summarise, grp.mean=mean(est))
  
  fig.data.bci2 <- bind_cols(commercial = c(0,1),
                             mean = c(mean(fig.data.bci$est[1:1400]),
                                      mean(fig.data.bci$est[1401:2800])),
                             ymin = c(quantile(fig.data.bci$est[1:1400],.025),
                                      quantile(fig.data.bci$est[1401:2800],.025)),
                             ymax = c(quantile(fig.data.bci$est[1:1400],.975),
                                      quantile(fig.data.bci$est[1401:2800],.975)))
  
  bci_hist <- fig.data.bci %>%
    ggplot(mapping = aes(group = factor(commercial), x = est, fill = factor(commercial))) +
    #geom_histogram(aes(y=..density..),position = "identity", alpha = .7) +
    geom_density(mapping = aes(x = est),position = 'identity',alpha = .7) +
    #geom_vline(data = mu,aes(xintercept=grp.mean),linetype = "dashed",size = 1) +
    geom_errorbar(data = fig.data.bci2,mapping = aes(y = c(.03,.07),xmin = ymin,xmax = ymax),color = 'black', width = .02,size = 1,inherit.aes = FALSE) +
    geom_point(data = mu,aes(x = grp.mean,y = c(.03,.07), fill = factor(commercial)),shape = 21, color = 'black',size = 3,stroke = 2) +
    theme_classic(base_size = 16) +
    labs(y = "Density",x = paste0('Estimated effect of ',ylab)) +
    theme(legend.position = "none") +
    scale_color_viridis_d(end = .8, aesthetics = c('color','fill'))
    
  
  bci2 <- fig.data.bci2 %>%
    ggplot(mapping = aes(x = factor(commercial), color = factor(commercial))) +
    geom_errorbar(mapping = aes(ymin = ymin,ymax = ymax),color = 'black', width = .1) +
    geom_point(mapping = aes(y = mean), size = 3) +
    scale_x_discrete(labels=c("0" = "Not commercially valuable", "1" = "Commercially valuable")) +
    theme_classic(base_size = 16) +
    labs(x = NULL,y = paste0('Estimated effect of ',ylab)) +
    theme(legend.position = "none") +
    scale_color_viridis_d(end = .8, aesthetics = c('color','fill'))
  
  if(save == TRUE){
    ggsave(paste0('results/graphics/mu.90.',x,'.png'),mu_90,width = 10,height = 6.5)
    ggsave(paste0('results/graphics/mu.95.',x,'.png'),mu_95,width = 10,height = 6.5)
    ggsave(paste0('results/graphics/per.sp.mu.',x,'.png'),per.sp.mu,width = 10,height = 6.5)
    ggsave(paste0('results/graphics/per.sp.full.',x,'.png'),per.sp.full,width = 10,height = 6.5)
    ggsave(paste0('results/graphics/bci.',x,'.png'),bci,width = 10,height = 6.5)
    ggsave(paste0('results/graphics/bci_hist.',x,'.png'),bci_hist,width = 10,height = 6.5)
    ggsave(paste0('results/graphics/bci2.',x,'.png'),bci2,width = 10,height = 6.5)
  }
  return(list(mu_90=mu_90,
              mu_95=mu_95,
              per.sp.mu=per.sp.mu,
              per.sp.full = per.sp.full,
              bci=bci,
              bci_hist=bci_hist,
              bci2=bci2))
}

dr_plot <- bayes_predict_plot(res,'dr',save = TRUE)
intf_plot <- bayes_predict_plot(res,'intf',save = TRUE)

dr_plot_grid <- grid.arrange(dr_plot$per.sp.full + theme_classic() + ggtitle('a)') + theme(legend.position = "top",legend.title = element_blank()),
                             dr_plot$bci_hist + theme_classic() + ggtitle('b)') + theme(legend.position = 'none'),nrow = 1)
ggsave('results/graphics/dr_plot.png',dr_plot_grid,width = 8, height = 4)
intf_plot_grid <- grid.arrange(intf_plot$per.sp.full + ggtitle('a)') + theme_classic() + theme(legend.position = "top",legend.title = element_blank()),
                               intf_plot$bci_hist + ggtitle('b)') + theme_classic() + theme(legend.position = 'none'),nrow = 1)
ggsave('results/graphics/intf_plot.png',intf_plot_grid,width = 8, height = 4)

# Ordered graph + labeled sensitive species (possibly supplemental figure)

## Table 2: table of statistics
mu <- summ[grep("mu", vars),cols]
sigma <- summ[grep("sigma", vars),cols]
p <- summ[grep("p", vars),cols]

raw <- rbind(p,mu,sigma[-c(10:11),])
raw <- data.frame(raw)
rownames(raw)
parameter_names <- c('Observer','Time of day','Site SD','Species SD','Intercept mean','Canopy height mean','Distance to road mean','Distance to road * commercial status mean','Intact forest mean','Intact forest * commercial status mean','water mean','Intercept SD','Canopy height SD','Distance to road SD','Distance to road * commercial status SD','Intact forest SD','Intact forest * commercial status SD','Site effect SD','Transect effect SD','water SD')

ordered_parameters <- c('Observer','Time of day','Site SD','Species SD','Intercept mean','Intercept SD','Canopy height mean','Canopy height SD','Distance to road mean','Distance to road SD','Distance to road * commercial status mean','Distance to road * commercial status SD','Intact forest mean','Intact forest SD','Intact forest * commercial status mean','Intact forest * commercial status SD','water mean','water SD','Site effect SD','Transect effect SD')

stats_tab <- raw %>%
  data.frame() %>%
  tibble() %>%
  bind_cols(Parameter = parameter_names,.)%>%
  mutate(Parameter =  factor(Parameter, levels = ordered_parameters)) %>%
  arrange(Parameter) %>%
  gt() %>%
  cols_label(mean = 'Mean', sd = 'SD', X2.5. = '2.5% quantile', X97.5. = '97.5% quantile',n.eff = 'Effective sample size') %>%
  fmt_number (columns = vars(mean, sd, `X2.5.`, `X97.5.`,Rhat), decimals = 2) %>%
  tab_row_group(group = "Detection",rows = c(1:4)) %>%
  tab_row_group(group = "Occupancy",rows = c(5:20))

gtsave(stats_tab,filename = 'results/graphics/tbl_param.png')

