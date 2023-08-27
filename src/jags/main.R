##### Controller for MCMC runs of occupancy models
# Author: Katherine Lauck
# Last updated: 27 May 2020

# This script runs MCMC threads in parallel using detection/nondetection data collected in collaboration with Planet Indonesia during 2018 and collated using data.R
# 

# Dependencies
source('src/jags/data.R')
library('rio')

# 1. Build data

data <- make.array() # build dataset
data$X <- data$Y # copy abundance array
data$X[data$X>0] <- 1 # change to presence/absence

# 2. Reduce problem for bug test

# # Subset down to the most common X species
# spp.sub.num <- 5 # set subset
# spp.use <- names(rev(apply(data$X, 2, sum)[order(apply(data$X, 2, sum))]))[1:spp.sub.num] # list sp to use
# data$nsp <- spp.sub.num # set nsp
# data$X <- data$X[,spp.use,] # subset X
# data$commercial <- data$commercial[spp.use] # subset commercial

# # Subset to sites with complete visit information
# site.use <- names(which(rowSums(is.na(data$obs),1) == 0)) # Identify sites to remove by returning sites with NA in their row of obs
# data$nsite <- length(site.use) # set nsite
# data$X <- data$X[site.use,,] # Subset X
# data$nrep.run <- data$nrep.run[site.use] # subset the survey cov arrays
# data$obs <- data$obs[site.use,]
# data$time <- data$time[site.use,]
# data$water <- data$water[site.use] # subset the site cov arrays
# data$ele <- data$ele[site.use]
# data$dist.road <- data$dist.road[site.use]
# data$dist.cano <- data$dist.cano[site.use]
# data$transect <- data$transect[site.use]

# 3. Exclude spp with <10 detections

spp.use <- names(apply(data$X, 2, sum)[which(apply(data$X, 2, sum)>9)]) # list sp to use
data$nsp <- length(spp.use) # set nsp
data$X <- data$X[,spp.use,] # subset X
data$commercial <- data$commercial[spp.use] # subset commercial

# 4. Run ms.ms

# intact forest
source('src/jags/ssom_2018_Vintf.R') # load jags model
dd<-list(data=data) # wrap data in list
times<-system.time(res<-ms.ms(dd, 100000, 200, 30000, 4));save(res, dd, times, file='results/jags/ssom_2018_Vintf_2020-08-26.rdata') # run and then save results

# # % disturbed canopy
# source('src/jags/ssom_2018_Vdc.R') # load jags model
# dd<-list(data=data) # wrap data in list
# times<-system.time(res<-ms.ms(dd, 100000, 200, 30000, 4));save(res, dd, times, file='results/jags/ssom_2018_Vdc.rdata') # run and then save results

# # bug test
# source('src/jags/ssom_2018_Vintf.R') # load jags model
# dd<-list(data=data) # wrap data in list
# times<-system.time(res<-ms.ms(dd, 5000, 5, 2500, 4))#; save(res, dd, times, file='results/jags/ssom_2018_Vintf_short.rdata') # run and then save results

# 5. Evaluate results

load("results/jags/ssom_2018_Vintf_short.rdata")
load("results/jags/ssom_2018_Vintf.rdata")
load("results/jags/ssom_2018_Vdc.rdata")

colnames(res$BUGSoutput$summary)
cols<-c('mean', '2.5%','97.5%','Rhat','n.eff')
summ<-res$BUGSoutput$summary

vars<-rownames(summ)

summ[grep("mu", vars),cols]

R2jags::traceplot(res$BUGSoutput,varname = grep('mu',vars,value = T))
