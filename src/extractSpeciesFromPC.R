# Extract point count data for specific species
# Author: Katherine Lauck
# 19 June 2019
#
# Extract point count data for several species.
#
#Change import/export directories in file below
source('G:/My Drive/projects/songbird-occupancy/src/SETTINGS.R')

library('rio')
library('tidyverse')
pc <- import(paste0('results/data/pointCountMaster.csv')) #import master
#source(paste0(PROJECT_DIRECTORY,'/src/buildDetectionHistories.R')) #rebuild master

n.detect <- pc %>%
  group_by(species) %>%
  summarize(n()) 

filter(n.detect, `n()`>=10)
detections <- arrange(n.detect,desc(`n()`))

species <- list('Copsychus malabaricus', # list of species. Can add/subtract, 
                'Berenicornis comatus',  # use comma to separate
                'Anorrhinus galeritus',
                'Anthracoceros albirostris',
                'Anthracoceros malayanus',
                'Buceros rhinoceros',
                'Rhinoplax vigil',
                'Rhabdotorrhinus corrugatus',
                'Rhyticeros undulatus',
                'Chloropsis sonnerati')

spp.subset <- pc[which(pc$species %in% species),]

export(spp.subset,paste0(PROJECT_DIRECTORY,'/results/data/PCsubsetForAdam.csv'))
