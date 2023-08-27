##### Prep unit covariates for analysis in occupancy models using RPresence
# Author: Katherine Lauck
# Last updated: 2 November 2018
#
##
##
# Dependencies
library('rio')
source('src/functions.R')
library('rlang')
library('tidyverse')
library(scales)

dataDir <- 'data/'
outputDir <- 'results/'

# import GIS data
gisLandak <- import(paste0(dataDir,"landakPoints.csv"))
gisLandak$name <- as.character(gisLandak$name)
gisLandak$lat <- as.numeric(gisLandak$lat)

# Import YPI data
mhYPI.file <- list.files(paste0(dataDir,"siteCov/microhabitat"),
                         full.names = TRUE)[-1]

# Combine into single dataframe
mhYPI <- NULL
repeat{
  if(length(mhYPI.file)==0){break} else{
    temp <- import(mhYPI.file[1])
    mhYPI <- rbind(mhYPI,temp)
    mhYPI.file <- mhYPI.file[-1]
  }
}
# Fix times
mhYPI["start"] <- format(mhYPI["start"],"%H:%M")
mhYPI["end"] <- format(mhYPI["end"],"%H:%M")
mhYPI["date"] <- format(mhYPI["date"],"%Y-%m-%d")
mhYPI["point"] <- unlist(mhYPI["point"],use.names = FALSE)

# Add GIS
for(i in mhYPI$point){
  mhYPI[which(mhYPI["point"]==i),"lat"] <-
    gisLandak[which(gisLandak["name"]==i),"lat"]
  mhYPI[which(mhYPI["point"]==i),"lon"] <-
    gisLandak[which(gisLandak["name"]==i),"lon"]
  mhYPI[which(mhYPI["point"]==i),"altitude"] <-
    gisLandak[which(gisLandak["name"]==i),"ele"]
}

gisLandak <- gisLandak[-89,] # remove line that has incorrect point name

# import data from Hendra - most of this data is currently unuseable because
# Hendra did not in fact make a table of values. Instead he gave me a table
# of ranges. We are currently only able to use the table of near values. I am
# currently attempting to contact him for an explanation and/or a second try.

#mh.Hendra <- import_list("/Volumes/GoogleDrive/Team Drives/Biodiversity/CAGN occupancy/Microhabitat/arcMap analysis/microhabitat_arcMap_master_Hendra.xlsx")
# mh.Hendra <- import("/Volumes/GoogleDrive/Team Drives/Biodiversity/CAGN occupancy/Microhabitat/arcMap analysis/microhabitat_arcMap_master_Hendra_NearDistance.csv")
# near.human <- as.data.frame(cbind("point" = unlist(mh.Hendra["NAME"],use.names = FALSE),
#                                   "dist" = unlist(mh.Hendra["NEAR_DIST (Meter)"],use.names = FALSE)))
# near.human <- near.human[which(grepl("^(TT)(.)*",near.human[,1])),]
# near.human <- near.human[-which(grepl("(TTB6 -6)",near.human[,1])),]
# near.human[,1] <- as.character(near.human[,1])
# 
# # add distance from human habitation to microhabitat data
# near.human[,2] <- as.numeric(gsub(",","",near.human[,2]))
# for(i in mhYPI$point){
#   mhYPI[which(mhYPI["point"]==i),"dist"] <-
#     near.human[which(near.human[,1]==i),2]
# }
mhYPI <- mhYPI[order(mhYPI$point),]

# add transect effect - occupancy can vary per transect rather than per point
#sp.dep_psiTransect <- as.character(unlist(import("/Volumes/GoogleDrive/My Drive/CAGN occupancy/Analysis/R/Rpresence/sp.dep_psiTransect.csv")))

# cut down unit covariates further

unit.cov.full <- mhYPI[,c(7:8,11,13:16)]
# convert anthropogenic disturbance to factor
#unit.cov$anthropogenicDisturbance[which(unit.cov$anthropogenicDisturbance>60)] <- 2
#unit.cov$anthropogenicDisturbance[which(unit.cov$anthropogenicDisturbance >= 10)] <- 1

##### Import GIS-generated variables. All of these variables were created using
##### ArcMap 10.3. For all of the points from both Bengkayang and Landak, I
##### created buffers of 100m, 500m, 1000m, and 1500m. Then, I used the Zonal
##### Statistics to Table tool to calculate the mean or majority, as
##### appropriate, of the values of raster pixels that were contained in each
##### buffer zone. I used all .tif files, downloaded from EarthExplorer, Global
##### Forest Watch, and Google Earth Engine.
#####


# list files to be compiled
output.gis <- list.files(path = paste0(dataDir,'siteCov/gis'),
                       pattern = "(.)*csv",
                       full.names = T,
                       recursive = T)

# load reference frame to be added to
gis <- import(paste0(dataDir,"referenceGISoutput.xls"))

# remove first line of output.gis, which refers to a broken file (Elevation_1000m.csv)
output.gis <- output.gis[-1]

# recursively add to gis over output.gis
repeat{
  if(length(output.gis)==0){break} else{ # end condition for recursion
    gis <- mergeGISData(output.gis[1],ref = gis) # add to the previous gis
    output.gis <- output.gis[-1] # remove a row from the counter
  }
}

# export full result
export(gis,paste0(outputDir,"data/GISfull.csv"))

# trim full result
gis <- gis[,-c(grep(pattern="STD$",x=names(gis)))]
gis.cov <- gis[grep(pattern = "^TT",x = gis$OBJECTID_1),]
gis.cov <- gis.cov[order(gis.cov$OBJECTID_1),] # order matches order of other covariates

# add gis cov to unit cov
unit.cov.full[,c((length(unit.cov.full)+1):(length(unit.cov.full)+length(gis.cov)-6))] <- gis.cov[,-c(1:6)]
export(unit.cov.full,paste0(outputDir,"data/siteCov/original/unit.cov.full.csv"))

# trim to final columns: elevation, understory, NDVI + scales, water, %intact
# forest + scales, % canopy cover + scales.
var.keep <- c('understoryComplexity',
              'water',
              'Elevation_100mMEAN',
              'Canopy_Height_100mMEAN',
              'Canopy_Height_500mMEAN',
              'Eucdist_Roads_km_100MEAN',
              'PercentIntactforest20161000m',
              'PercentIntactforest2016100m',
              'PercentIntactforest20161500m',
              'PercentIntactforest2016500m',
              'Zonal_Stats100mMEAN',
              'Zonal_Stats500mMEAN',
              'Disturbed_Canopy_100mPercent',
              'Disturbed_Canopy_500mPercent')
unit.cov <- unit.cov.full[,which(names(unit.cov.full) %in% var.keep)]

names(unit.cov) <- c('und',
                     'water',
                     'ele',
                     'height100',
                     'height500',
                     'disturb100',
                     'disturb500',
                     'distRoad',
                     'forest1000',
                     'forest100',
                     'forest1500',
                     'forest500',
                     'ndvi100',
                     'ndvi500')


# # remove some columns DON'T RUN THIS
# unit.cov <- unit.cov.full[,-c(grep(x = colnames(unit.cov.full),
#                                    pattern = '(1000)|(1500)'))]

export(unit.cov,'results/data/siteCov/original/site.cov.csv')


# scale covariates to 0-1
unit.cov.scaled <- as.data.frame(apply(unit.cov,2,rescale))


export(unit.cov.scaled,'results/data/siteCov/original/site.cov.scaled.csv')


