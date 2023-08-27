##### Construct data frames for use by main.R
# Author: Katherine Lauck
# Last updated: 19 May 2020
#
# This script builds the data necessary to run MCMC threads in parallel using detection/nondetection, site, and occasion data collected in collaboration with Planet Indonesia during 2017-2018. These data will be fed to main.R.
# 
# To do:
# Drop sites with missed visit
# Ask Alison about missed visits

# Dependencies
library(reshape2)
library(rjags)
library(R2jags)
library(scales)

source('src/jags/myfunctions.R')

make.array <- function(df = read.csv('results/data/pointCountMaster.csv')){

# Step 1: build 3-dim array for detections. Dim1 = site, Dim2 = Species, Dim3 = Rep

df <- df[-c(which(df$species == ""),which(df$species == "unk")),]# remove empty row and unk records
XX<-acast(df, point ~ species ~ occasion, sum) # create array
# dimnames(XX)[[2]] # Check sp names


# Step 2: Collect each of 'obs','time','transect','water','ele','dist.road','dist.cano','commercial'

survey.cov <- read.csv('results/data/surveyCov/original/survey.cov.scaled.csv')# read in survey covariates
site.cov <- read.csv('results/data/siteCov/original/site.cov.scaled.csv')# read in site covariates
site <- t(read.csv("results/data/point.dummy.csv")[1:115]) # read in sitenames for dimname of arrays and filter out dummy points
my.array <- function(data,type){ # array creation function to avoid repetition of code. type = "survey", "site", or "species".
  if(type == "survey") {
    dim = c(length(site),5)
    dimnames = list(
      site = site,
      occasion = seq(1,5)
    )
  } else if(type == "site") {
    dim = c(length(site))
    dimnames = list(site = site)
  } else if(type == "species") {
    dim = c(length(dimnames(XX)[[2]]))
    dimnames = list(species = dimnames(XX)[[2]])
  }
  d <- array(
    data,
    dim = dim,
    dimnames = dimnames
  )
  return(d)
}
obs.raw <- array(survey.cov$observer1) # select and name 'obs'
obs.raw[obs.raw == 'Indah'] <- 1
obs.raw[obs.raw == 'Katie'] <- 2 # change names to numbers. Indah = 1, Katie = 2. Eventually should scale these. Q9
obs.raw[obs.raw == ''] <- 0 # indicate missing information
obs <- my.array(rescale(as.numeric(obs.raw)),'survey') # scale obs
time <- my.array(survey.cov$start,'survey') # select and name 'time'
time[is.na(time)] <- 0 # indicate missing information
water <- my.array(site.cov$water,'site') # select and name 'water','ele','dist.road','dist.cano'
ele <- my.array(site.cov$ele,'site') # create elevation array
dist.road <- my.array(site.cov$distRoad,'site') # create distance to road array
dist.cano <- my.array(site.cov$disturb100,'site') # create disturbed canopy array
nrep.run <- my.array(5-apply(obs == 0,1,sum),'site') # create nrep.run array
intf <- my.array(site.cov$forest500,'site') # create intact forest array
ch <- my.array(site.cov$height100,'site') # create canopy height array
visited <- my.array(as.numeric(obs != 0),'survey') # create visited array

# Step 3: Create 'commercial' and 'transect' factor


transect.raw <- stringr::str_extract(site,pattern = "^[[:alnum:]]{4}") # Create vector of transect names
transect.raw <- as.numeric(as.factor(transect.raw)) # change values to numerics
transect <- my.array(transect.raw,'site') # create 'transect'

commercial <- my.array(rep.int(0,206),'species') # create scaffold for 'commercial'
commercial[which(unlist(dimnames(commercial)) %in% c('Chloropsis sonnerati',
                                             'Copsychus malabaricus',
                                             'Irena puella',
                                             'Spilornis cheela',
                                             'Rhinoplax vigil',
                                             'Loriculus galgulus',
                                             'Argusianus argus',
                                             'Pernis ptilorhynchus',
                                             'Nisaetus alboniger',
                                             'Nisaetus limnaeetus',
                                             'Alophoixus tephrogenys',
                                             'Zosterops auriventer',
                                             'Gracula religiosa',
                                             'Geokichla interpres',
                                             'Copsychus saularis'))] <- 1 # indicate which species are traded commercially

# Step 4: Calculate 'nsp', 'nsite', 'ntransect'

nsp <- as.numeric(length(dimnames(XX)[[2]]))
nsite <- as.numeric(length(site))
ntransect <- as.numeric(length(unique(transect)))

# Step 5: Collate into list

dd <- list(Y = XX,
           nsp = nsp,
           nsite = nsite,
           ntransect = ntransect,
           nrep.run = nrep.run,
           obs = obs,
           time = time,
           water = water,
           ele = ele,
           dist.road = dist.road,
           dist.cano = dist.cano,
           transect = transect,
           commercial = commercial,
           visited = visited,
           intf = intf,
           ch = ch
           )
return(dd)
}
