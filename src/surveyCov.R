##### Prep survey covariates for analysis in occupancy models using RPresence
# Author: Katherine Lauck
# Last updated: 2 November 2018

dataDir <- paste0(PROJECT_DIRECTORY,'/results/data/')
outputDir <- paste0(PROJECT_DIRECTORY,'/results/data/surveyCov/original/')


##### Gather survey-specific data
# import full data frame
pc <- import(paste0(dataDir,'pointCountMaster.csv'))

# cleave survey-specific data
survey.data <- pc[,c(1:13,23)]

# narrow to one observation per survey. Should end up with ~10 rows less than 575
survey.data <- unique(survey.data)

# add rows for missing surveys
survey.data <- survey.data[,-1] # remove first column, location, because all in same site
survey.data <- rbind(survey.data,c("TTA6-10",NA,NA,"2017-12-09",rep(NA,8),3),
                     c("TTA6-8",NA,NA,"2017-12-09",rep(NA,8),3),
                     c("TTA6-9",NA,NA,"2017-12-09",rep(NA,8),3),
                     c("TTB3-6",NA,NA,"2017-12-13",rep(NA,8),3),
                     c("TTB8-7",NA,NA,"2017-12-07",rep(NA,8),3),
                     c("TTB8-9",NA,NA,"2017-10-06",rep(NA,8),1),
                     c("TTB7-6",NA,NA,"2018-02-19",rep(NA,8),5),
                     c("TTB4-8",NA,NA,"2017-10-01",rep(NA,8),1),
                     c("TTB4-9",NA,NA,"2017-10-01",rep(NA,8),1),
                     c("TTB4-5",NA,NA,"2018-02-17",rep(NA,8),5),
                     c("TTB3-8",NA,NA,"2017-09-30",rep(NA,8),1),
                     c("TTB1-5",NA,NA,"2017-09-28",rep(NA,8),1))
# order rows correctly to match the order of rows in the occupancy
# data
survey.data <- survey.data[order(survey.data$occasion,survey.data$point),]
survey.data <- survey.data[,c(2,4,5,6,13)] # reduce columns to useful ones
survey.data[which(is.na(survey.data$observer1)),] <- NA # explicitly insert NA's
survey.data$start <- as.numeric(format(strptime(survey.data$start, #convert to fractional hours
                                                format="%H:%M"),
                                       format="%H"))+
  ((as.numeric(format(strptime(survey.data$start,
                               format="%H:%M"),
                      format="%M")))/60)

survey.data$cloudCover <- as.integer(survey.data$cloudCover)

# Add a pretty classic trap response factor. After a HH is detected once, it is more
# likely to be detected again in later surveys on the same transect (within each day)
# trapResponse <- as.numeric(unlist(import("/Volumes/GoogleDrive/My Drive/CAGN occupancy/Analysis/R/Rpresence/sp.dep_trapResponse.csv")))

# survey.data <- cbind(survey.data,trapResponse)
export(survey.data,paste0(outputDir,"survey.cov.csv"))

# scale survey data- can add or remove trap response variable as needed
survey.data.scaled <- as.data.frame(cbind(observer1 = survey.data[,1],
                                          as.data.frame(apply(survey.data[,c(3:4)],
                                                              2,
                                                              rescale,
                                                              na.rm=T))
                                          #,trapResponse = survey.data[,6]
                                          )
                                    )
export(survey.data.scaled,paste0(outputDir,"survey.cov.scaled.csv"))
