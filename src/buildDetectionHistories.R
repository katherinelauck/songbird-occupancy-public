# Detection histories
# Author: Katherine Lauck
# 25 June 2018
#
# Build per species detection histories for analysis with Presence from point
# count data entered according to the protocol and entry form provided by YPI.
#

require('rio')

# Data directory
dataDir <- paste0('data/pointCount')
outputDir <- paste0("results")


# List files

data <- grep("(TT)(.)*(?!proofed|proof)(\\w)*\\.xlsx"
             ,list.files(
               path = dataDir
               ,pattern = "^(TT)(.)*xlsx"
               ,recursive = TRUE,
               full.names = TRUE
               )
             ,perl=TRUE
             ,ignore.case = TRUE
             ,value = TRUE
             )
             

# Combine into single dataframe
# cl = makeCluster(detectCores()-1)
# clusterExport(cl = cl,"data")
# clusterEvalQ(cl = cl,library('data.table'))
# clusterEvalQ(cl = cl,library('rio'))
# pc <- rbindlist(pblapply(data,import))
# stopCluster(cl)
pc <- NULL
repeat{
  if(length(data)==0){break} else{
    temp <- import(data[1])
    pc <- rbind(pc,temp)
    data <- data[-1]
  }
}

# Fix times
pc["start"] <- format(pc["start"],"%H:%M")
pc["callTime"] <- format(pc["callTime"],"%M:%S")

# Add sampling occasion
unique(pc[,"date"])
pc <- cbind(pc,occasion = rep(NA,length(pc[,1])))

pc[which(as.Date(pc[,"date"]) %in% as.Date("2018-02-13"):as.Date("2018-02-24")),
   "occasion"] <- 5
pc[which(as.Date(pc[,"date"]) %in% as.Date("2017-12-06"):as.Date("2017-12-16")),
   "occasion"] <- 3
pc[which(as.Date(pc[,"date"]) %in% as.Date("2018-01-23"):as.Date("2018-02-02")),
   "occasion"] <- 4
pc[which(as.Date(pc[,"date"]) %in% as.Date("2017-11-03"):as.Date("2017-11-13")),
   "occasion"] <- 2
pc[which(as.Date(pc[,"date"]) %in% as.Date("2017-09-27"):as.Date("2017-10-07")),
   "occasion"] <- 1

# Test for missing occasions
any(is.na(pc[,"occasion"]))

# order data frame
pc <- pc[order(pc$occasion,pc$point),]

pc["date"] <- format(pc["date"],"%Y-%m-%d")

# Print full data frame
#setwd("/Volumes/GoogleDrive/My Drive/CAGN occupancy/Analysis/R/Rpresence")
export(pc,paste0(outputDir,'/data/pointCountMaster.csv'))




# Create list of detection histories
spp <- by(data=pc,
          INDICES = pc[,"species"],
          FUN = detect,
          pt.list = sort(unique(pc[,"point"])),
          survey.list = by(data = pc[,"point"],
                           INDICES = pc[,"occasion"],
                           FUN = unique),
          t = 5
          )

# Export to files named for species
setwd(paste0(outputDir,"/data/detectionHistories"))

for(i in seq.int(1,length(spp))){
  export(as.data.frame(spp[[i]])[,2:length(as.data.frame(spp[[i]]))],
         file = paste(names(spp[i]),
                      ".csv",sep = ""),
         col.names = FALSE)
}


# Export point list for reference
point <- as.data.frame(sort(unique(pc[,"point"])),
              col.names = "point")
names(point) <- "point"
export(point,
       file = "pointList.csv")

# Produce species list separated by observer
spp <- table(pc[,"species"],pc[,"observer1"])

setwd(paste0(outputDir,'/data'))
file.remove("sppByObserver.xlsx")

#export species list
export(spp,file = "sppByObserver.xlsx")

