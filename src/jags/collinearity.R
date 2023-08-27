##### Explore collinearity among predictors
# Author: Katherine Lauck
# Last updated: 23 May 2020
#
# This script explores collinearity among distance to roads and the various measures of disturbance or habitat calculated for use in ssom_2018.

# Dependencies
library(mctest)
library(car)

# load data
site.cov <- read.csv('results/data/siteCov/original/site.cov.scaled.csv')# read in site covariates
names(site.cov)

df <- site.cov[,-which(names(site.cov)%in% c('forest1000','forest1500','height500','disturb500','ndvi500'))]

vifs <- vif(lm(rnorm(nrow(df)) ~. , data = df))
vifs

plot(site.cov$height100~site.cov$distRoad)
plot(site.cov$disturb100~site.cov$distRoad)
plot(site.cov$disturb500~site.cov$distRoad)
plot(site.cov$forest100~site.cov$distRoad)
plot(site.cov$height100~site.cov$forest100)
plot(site.cov$height100~site.cov$disturb100)
plot(site.cov$height500~site.cov$disturb500)
plot(site.cov$forest500~site.cov$disturb500)
