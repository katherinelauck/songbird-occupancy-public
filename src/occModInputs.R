##### Build all objects for occupancy modeling for territorial birds, including
##### dummy covariates
# Author: Katherine Lauck
# Last updated: 4 November 2018
#
# To use this script, start by running buildDetectionHistories.R, which creates
# the source occupancy data for the analyses carried out by this script.
#
# To change the survey covariates available, modify surveyCov.R, and to change
# the site covariates, modify unitCov.R. Make sure you understand the required
# format for both covariate types.
#
#
# This code requires the following files. In the future, this should be replaced with
# source() commands.


require('rio')

source('C:/Users/kathe/Documents/songbird-occupancy/src/SETTINGS.R')
source(paste0(PROJECT_DIRECTORY,'/src/functions.R'))
source(paste0(PROJECT_DIRECTORY,'/src/surveyCov.R'))
source(paste0(PROJECT_DIRECTORY,'/src/unitCov.R'))
source(paste0(PROJECT_DIRECTORY,'/src/buildDetectionHistories.R'))
source(paste0(PROJECT_DIRECTORY,'/src/HighstatLibV10.R'))
inputDir <- paste0(PROJECT_DIRECTORY,'/results/')

spp <- table(pc[,"species"],pc[,"observer1"])[-1,]
spp <- data.frame(species=names(spp[,"Indah"]),Indah=spp[,"Indah"],Katie=spp[,"Katie"],
                  row.names=NULL)
export(spp,paste0(inputDir,"data/sppList.csv"))
# trim covariates for use with small birds exclusively
# unit.cov.scaled <- unit.cov.scaled[,-c(grep(pattern="1000",x=names(unit.cov.scaled)),
#                                        grep(pattern='1500',x=names(unit.cov.scaled)),
#                                        grep(pattern='canopyHeight100',x=names(unit.cov.scaled)),
#                                        grep(pattern='ciforFC500',x=names(unit.cov.scaled)),
#                                        grep(pattern='elev500',x=names(unit.cov.scaled)),
#                                        grep(pattern='slope500',x=names(unit.cov.scaled)),
#                                        grep(pattern='tcSexton500',x=names(unit.cov.scaled)),
#                                        grep(pattern='altitude',x=names(unit.cov.scaled)),
#                                        grep(pattern='elev',x=names(unit.cov.scaled))
# )]

# calculate VIF
# corvif(unit.cov.scaled[,-c(which(names(unit.cov.scaled)=="understoryComplexity"),
#                            which(names(unit.cov.scaled)=="anthropogenicDisturbance"),
#                            which(names(unit.cov.scaled)=="ciforFC100MAJORITY"))])
corvif(unit.cov.scaled)
# unit.cov.scaled <- unit.cov.scaled[,-c(grep(x = colnames(unit.cov.scaled),pattern = '500'))]
# 

dummy.unit.cov <- apply(unit.cov.scaled,MARGIN = 2,# create sequences to be used for 
                        function(x) {              # investigating the effects of specific
                          return(seq(from = min(x,na.rm = T), # variables on occupancy
                                     to = max(x,na.rm = T),
                                     length.out = 30))
                        })

# create dummy covariates that should equal the mean of the corresponding variable in
# unit.cov.scaled. These covariates measure habitat, not human disturbance.
dum.unit.cov <- cbind(v1=rep(mean(unit.cov.scaled[,1],na.rm = T),times = 120),
                      v2=rep(mean(unit.cov.scaled[,2],na.rm = T),times = 120),
                      v3=rep(mean(unit.cov.scaled[,3],na.rm = T),times = 120),
                      v4=rep(mean(unit.cov.scaled[,4],na.rm = T),times = 120),
                      v5=rep(mean(unit.cov.scaled[,5],na.rm = T),times = 120),
                      v6=rep(mean(unit.cov.scaled[,6],na.rm = T),times = 120),
                      v7=rep(mean(unit.cov.scaled[,7],na.rm = T),times = 120),
                      v8=rep(mean(unit.cov.scaled[,8],na.rm = T),times = 120),
                      v9=rep(mean(unit.cov.scaled[,9],na.rm = T),times = 120),
                      v10=rep(mean(unit.cov.scaled[,10],na.rm = T),times = 120),
                      v11=rep(mean(unit.cov.scaled[,11],na.rm = T),times = 120),
                      v12=rep(mean(unit.cov.scaled[,12],na.rm = T),times = 120),
                      v13=rep(mean(unit.cov.scaled[,13],na.rm = T),times = 120),
                      v14=rep(mean(unit.cov.scaled[,14],na.rm = T),times = 120)
                      
)
colnames(dum.unit.cov) <- names(unit.cov.scaled)

# add sequences to the specific factors of interest. As many as four may be compared.
#dum.unit.cov[c(1:30),6] <- as.factor(c(rep(0,10),rep(1,10),rep(2,10)))
dum.unit.cov[c(31:60),grep('Road',colnames(unit.cov.scaled))] <- dummy.unit.cov[,1]
dum.unit.cov[c(61:90),grep('height100',colnames(unit.cov.scaled))] <- dummy.unit.cov[,1]
dum.unit.cov[c(91:120),grep('height500',colnames(unit.cov.scaled))] <- dummy.unit.cov[,1]

unit.dummy <- unit.cov.scaled
unit.dummy[116:(115+120),] <- dum.unit.cov

export(unit.dummy,paste0(inputDir,"data/siteCov/dummy/unit.cov.dummy.csv"))

# dummy surveys
s1 <- survey.data.scaled[1:115,]
s2 <- survey.data.scaled[116:(115*2),]
s3 <- survey.data.scaled[(115*2+1):(115*3),]
s4 <- survey.data.scaled[(115*3+1):(115*4),]
s5 <- survey.data.scaled[(115*4+1):(115*5),]
s1[116:(115+120),] <- matrix(nrow=120,ncol=3)
s2[116:(115+120),] <- matrix(nrow=120,ncol=3)
s3[116:(115+120),] <- matrix(nrow=120,ncol=3)
s4[116:(115+120),] <- matrix(nrow=120,ncol=3)
s5[116:(115+120),] <- matrix(nrow=120,ncol=3)

survey.dummy <- rbind(s1,s2,s3,s4,s5)

export(survey.dummy,paste0(inputDir,"data/surveyCov/dummy/survey.cov.dummy.csv"))

# dummy point names
dummy.names <- lapply(seq.int(1,120,length.out=120),function(x) paste0("dummy",x))
ptL <- import(paste0(inputDir,"data/detectionHistories/pointList.csv"))
dummy.pt <- c(ptL[[1]],dummy.names)
export(as.data.frame(dummy.pt),paste0(inputDir,"data/point.dummy.csv"))
cl <- makeCluster(detectCores()-1)


# dummy occupancy matrices
dummy.occupancy <- matrix(nrow=120,ncol=5)

dummy.occ <- list.files(paste0(inputDir,"data/detectionHistories"),
                        full.names = T)
dummy.occ <- dummy.occ[-c(grep(pattern="unk.csv$",
                               x = dummy.occ),
                          grep(pattern="pointList.csv$",
                               x=dummy.occ),
                          grep(pattern="Icon\r$",
                               x = dummy.occ))]

# build dummy occupancy histories, try with parallel processing - way faster!
clusterExport(cl,c('dummy.occ','dummy.occupancy'))
clusterEvalQ(cl,{
  library('rio')
})
system.time(parLapply(dummy.occ,function(path,dummy.occupancy){
  temp <- import(path)
  temp <- rbind(temp,dummy.occupancy)
  dummy.path <- sub(pattern = "detectionHistories",
                    replacement = "dummyHistories",
                    x = path)
  export(temp,dummy.path)
},dummy.occupancy,cl=cl))
stopCluster(cl)


# ##### PCA to reduce the number of habitat variables used
# # import original data (prcomp will scale them itself)
# unit.cov <- import("/Volumes/GoogleDrive/My Drive/CAGN occupancy/Analysis/R/Rpresence/occModInputs/original/unit.cov.csv")
# # remove unuseable/collinear data
# unit.cov <- unit.cov[,-c(grep(pattern="1000",x=names(unit.cov)),
#                                        grep(pattern='1500',x=names(unit.cov)),
#                                        grep(pattern='canopyHeight100',x=names(unit.cov)),
#                                        grep(pattern='ciforFC500',x=names(unit.cov)),
#                                        grep(pattern='elev500',x=names(unit.cov)),
#                                        grep(pattern='slope500',x=names(unit.cov)),
#                                        grep(pattern='tcSexton500',x=names(unit.cov)),
#                                        grep(pattern='altitude',x=names(unit.cov)),
#                                        grep(pattern='elev',x=names(unit.cov))
# )]
# 
# # PCA
# unit.cov.pca <- prcomp(unit.cov[,c(1:5,8,11)],
#                        center = T,
#                        scale. = T)
# summary(unit.cov.pca)
# 
# install_github("vqv/ggbiplot")
# library(ggbiplot)
# 
# ggbiplot(unit.cov.pca,varname.abbrev=T)
# unit.cov.pca$rotation
