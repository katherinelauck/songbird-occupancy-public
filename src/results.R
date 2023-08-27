##### Output results, including graphs and tables.
##### Author: Katherine Lauck
##### Last updated: 22 September 2019
##### 
##### Big idea: combine all spp into one graph. Possibly limit # of untrapped 
##### spp included. Trapped vs untrapped spp indicated using line color.
##### Build graph that compares site-specific occupancy estimates to changes in 
##### covariates of interest (tree height, distance from roads)

# Initialization

source("C:/Users/kathe/Documents/songbird-occupancy/src/SETTINGS.R")
source(paste0(PROJECT_DIRECTORY,"/src/functions.R"))

# If desired, you can recreate the input from scratch:
#source('src/occModInputs.R')

# import input files depending on whether this analysis will use dummy or normal
# dataset
importInput(DATASET)
occ.data <- makeFilenameVector(SPECIES_TO_BE_ANALYZED,DATASET)
names <- SPECIES_TO_BE_ANALYZED

##### Prep additions to paper 22 Sept 2019
# 1) top models for each spp w/ est site wide psi & AICc

global.aic.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/global"),
                               pattern = ".*(AICsum\\.csv)$",
                               recursive = T,
                               full.names = T
)
global.aic.tables <- lapply(global.aic.files,
                            import)
names(global.aic.tables) <- names

lapply(names,
       model.tables,
       tables = global.aic.tables,
       type = "aic")

# 2) beta values/evidence ratio per spp/covariate, with star for significance

global.rds.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/global"),
                               pattern = ".*(AIC\\.rds)$",
                               recursive = T,
                               full.names = T
)
psi.beta <- lapply(global.rds.files,
                      extract.beta)
est.psi <- lapply(global.rds.files,
                  extract.psi.est)

names(psi.beta) <- names
names(est.psi) <- names

lapply(names,
       model.tables,
       tables = psi.beta,
       aic.tables = global.aic.tables,
       psi.est = est.psi,
       type = "beta")

# 3) graphing response of psi to significant beta values for spp of interest




# 4) table of covariates, info/source, and expectations

# # test
# extract.model.averages(paste0(OUTPUT_DIRECTORY,'/global/Alcippe brunneicauda/',
#                               'Alcippe brunneicaudaAIC.rds'))
#                               

global.rds.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/global"),
                               pattern = ".*(AIC\\.rds)$",
                               recursive = T,
                               full.names = T
)
psi.mod.avg <- lapply(global.rds.files,
                      extract.model.averages)

names(psi.mod.avg) <- names

global.aic.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/global"),
                               pattern = ".*(AICsum\\.csv)$",
                               recursive = T,
                               full.names = T
)
global.aic.tables <- lapply(global.aic.files,
                            import)

stargazer(global.aic.tables[[1]][which(global.aic.tables[[1]]$DAIC <= 2),],
          type = 'html',
          out = 'results/graphics/aictable.doc',
          summary = F)
global.weight.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/global"),
                                  pattern = ".*(Weightpsi\\.csv)$",
                                  recursive = T,
                                  full.names = T
)

global.weight.tables <- lapply(global.weight.files,
                               import)
names(global.weight.tables) <- names

global.weight.sum <- sapply(global.weight.tables, "[", 'ER',simplify = 'array')
global.weight.sum <- do.call(rbind,global.weight.sum)
rownames(global.weight.sum) <- names
colnames(global.weight.sum) <- global.weight.tables[[1]]$covnames
global.weight.sum[which(global.weight.sum[,'height100'] == 0),'height100'] <- 
  global.weight.sum[which(global.weight.sum[,'height500'] != 0),'height500']
global.weight.sum[which(global.weight.sum[,'disturb100'] == 0),'disturb100'] <- 
  global.weight.sum[which(global.weight.sum[,'disturb500'] != 0),'disturb500']
global.weight.sum[which(global.weight.sum[,'ndvi100'] == 0),'ndvi100'] <- 
  global.weight.sum[which(global.weight.sum[,'ndvi500'] != 0),'ndvi500']
global.weight.sum <- as.data.frame(global.weight.sum)
global.weight.sum <- global.weight.sum[,-which(colnames(global.weight.sum) %in% c('height500','disturb500','ndvi500'))]


for(i in seq_len(length(global.weight.sum$forest100))){
  if(global.weight.sum$forest100[i] == 0){
    if(global.weight.sum$forest500[i] !=0){
      global.weight.sum$forest100[i] <-
        global.weight.sum$forest500[i]
    } else if(global.weight.sum$forest1000[i] !=0){
      global.weight.sum$forest100[i] <-
        global.weight.sum$forest1000[i]
    } else if(global.weight.sum$forest1500[i] !=0){
      global.weight.sum$forest100[i] <-
        global.weight.sum$forest1500[i]
    }
  }
}

stargazer(global.aic.tables[[1]][which(global.aic.tables[[1]]$DAIC <= 2),],
          type = 'html',
          out = 'results/graphics/aictable.doc',
          summary = F)

# print stargazer model tables

lapply(which(names %in% c(spp.trapped$species)),
       model.tables)
stargazer(global.aic.tables[[5]][which(global.aic.tables[[5]]$DAIC <= 3),],
          type = 'html',
          out = paste0('results/graphics/',names[5],'.doc'),
          summary = F)

global.weight.sum <- global.weight.sum[,-which(colnames(global.weight.sum) %in% c('forest500','forest1500','forest1000'))]
colnames(global.weight.sum) <- c('Understory',
                                 '% Water',
                                 'Elevation',
                                'Canopy height',
                                'Canopy disturbance',
                                'Distance from road',
                                '% Intact forest',
                                'NDVI')

stargazer(global.weight.sum,
          type = 'html',
          out = 'results/graphics/sumweight.doc',
          summary = F,
          digits = 2)

stargazer(global.weight.sum[which(rownames(global.weight.sum)%in% spp.trapped$species),][-c(6,7),-c(4,6)],
          type = 'html',
          out = 'results/graphics/sumweighttrapped.doc',
          summary = F,
          digits = 2)



# build plots - eventually compare slopes. Non-traded spp m ~= 0, traded spp 
# m != 0
# 
# 
cairo_pdf(file = 'results/graphics/distanceToRoads.pdf',
          width = 11.5, height = 8, onefile = F, family = "Helvetica")
site.cov <- import(paste0(INPUT_DIRECTORY,'/siteCov/original/site.cov.csv'))
par(mar = c(4,4,2,2) + .1)
xlim <- c(min(site.cov$distRoad),
          max(site.cov$distRoad))

plot(1~1,type = 'n',xlim = xlim,ylim = c(-1,1),xlab = 'Distance to roads (km)',
     ylab = 'Occupancy probability')


# make helper frames roadDistance and roadDistanceColor
roadDistanceColor <- list(1,1,1,1,'red',1,1,1,1,1,'red',1,1,1,1,1,1,'red',1,'red',
                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,'red',1,1,1,'red',1,1,1,1,1,1,1,1,1,1,1,1,
                          1,1,1,'red',1,1,1,1,1,1,1,1,1,1,1,1,
                          'red',1,1,1,1,1,1,1)
site.cov <- import(paste0(INPUT_DIRECTORY,'/siteCov/original/site.cov.csv'))
roadDistance <- seq(from = min(site.cov$distRoad),
                    to = max(site.cov$distRoad),
                    length.out = 30)
indexRoad <- rep(list((31+115):(60+115)),times = 77)

lapply(seq_along(psi.mod.avg),
       add.spp.to.plot,
       index = indexRoad,
       cov = roadDistance,
       color = roadDistanceColor)

dev.off()

# per species plots

lapply(seq_along(psi.mod.avg),
       plot.one.spp,
       index = indexRoad,
       cov = roadDistance,
       color = roadDistanceColor)



m.road <- t(sapply(seq_along(psi.mod.avg),
                   extract.m,
                   data = psi.mod.avg,
                   index = indexRoad,
                   color = roadDistanceColor))
rownames(m.road) <- names
m.road[,1] <- as.numeric(m.road[,1])

## Test for normality
hist(as.numeric(m.road[which(m.road[,2]=='1'),1]))
hist(as.numeric(m.road[-57,][which(m.road[-57,][,2]=='red'),1]))
hist(as.numeric(m.road[,1]))
qqnorm(y=as.numeric(m.road[which(m.road[,2]=='1'),1]))
qqline(as.numeric(m.road[which(m.road[,2]=='1'),1]))
qqnorm(as.numeric(m.road[-57,][which(m.road[-57,][,2]=='red'),1]))
qqline(as.numeric(m.road[-57,][which(m.road[-57,][,2]=='red'),1]))
ggqqplot(as.numeric(m.road[which(m.road[,2]=='1'),1]))
ggqqplot(as.numeric(m.road[-57,][which(m.road[-57,][,2]=='red'),1]))


# cairo_pdf(file = 'results/graphics/boxplotDistanceToRoads.pdf',
#           width = 8, height = 11.5, onefile = F, family = "Helvetica")

png(filename = "results/graphics/boxplotDistanceToRoads.png",
     width = 4, height = 3, units = "in", pointsize = 12,
     bg = "white", res = 300, family = "Helvetica", restoreConsole = TRUE,
     type = 'cairo-png')
par(mar = c(3,4,1,1) + .1)
total.detections <- cbind(names,rowSums(spp[which(spp$species %in% names),2:3]))



boxplot(as.numeric(m.road[-57,][which(m.road[-57,][,2]=='1'),1]),
        as.numeric(m.road[-57,][which(m.road[-57,][,2]=='red'),1]),
        notch = F,
        names = c('Spp. not trapped','Trapped spp.'),
        ylab = 'Slope of psi ~ road distance (km)')
dev.off()

include <- unique(c(which(total.detections[,2]>=40),
                    which(total.detections[,1] %in% spp.trapped$species)))
include <- include[-which(include ==57)]

#exclude those with # detections < 30
boxplot(as.numeric(m.road[include,][which(m.road[include,][,2]=='1'),1]),
        as.numeric(m.road[include,][which(m.road[include,][,2]=='red'),1]),
        notch = F,
        names = c('Spp. not trapped','Trapped spp.'),
        ylab = 'Slope of psi ~ road distance (km)')
t.test(as.numeric(m.road[include,1])~as.factor(m.road[include,2]),alternative = 'less')
t.test(as.numeric(m.road[which(m.road[include,2]=='1'),1]))
t.test(as.numeric(m.road[include,][which(m.road[include,][,2]=='red'),1]),alternative = 'greater')

# tree height

cairo_pdf(file = 'results/graphics/treeHeight.pdf',
          width = 11.5, height = 8, onefile = F, family = "Helvetica")
par(mar = c(4,4,2,2) + .1)
xlim <- c(min(site.cov$height100),
          max(site.cov$height100))

plot(1~1,type = 'n',xlim = xlim,ylim = c(-1,1),xlab = 'Tree height (m)',
     ylab = 'Occupancy probability')

# make helper frames roadDistance and roadDistanceColor

 
# list and import summaries of AIC tables
global.aic.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/anthro"),
                               pattern = ".*(AICsum\\.csv)$",
                               recursive = T,
                               full.names = T
)
global.aic.tables <- lapply(global.aic.files,
                            import)

# capture global model for each sp 
global.psi.models <- lapply(global.aic.tables,
                            function(x) return(sub("^p\\((.*?)\\)",
                                                   "",
                                                   x$Model[1],
                                                   perl = T)))
names(global.psi.models) <- names

heightColor <- list(1,1,1,1,1,1,1,1,1,1,1,"red",1,1,1,1,1,1,1,'red',
                    1,1,1,1,1,1,1,1,1,1,1,'red','red','red',1,1,1,1,'red',1,'red',
                    1,1,1,1,1,'red',1,1,1,1,1,1,1,1,1,1,1,1,'red','red','red','red',
                    1,1,1,1,1,1,1,1,1,1,1,1,1,1)
height <- seq(from = min(site.cov$height100),
              to = max(site.cov$height100),
              length.out = 30)

indexHeight <- rep(list(1),times = 77)
indexHeight[which(grepl("(.)*height100(.)*",
                        unlist(global.psi.models)))] <- rep(list((61+115):(90+115)),
                                                            times = length(which(grepl("(.)*height100(.)*",
                                                                                       unlist(global.psi.models)))))
indexHeight[which(grepl("(.)*height500(.)*",
                        unlist(global.psi.models)))] <- rep(list((91+115):(120+115)),
                                                            times = length(which(grepl("(.)*height500(.)*",
                                                                                       unlist(global.psi.models)))))

names(indexHeight) <- names

lapply(seq_along(psi.mod.avg),
       add.spp.to.plot,
       index = indexHeight,
       cov = height,
       color = heightColor)

dev.off()

m.height <- t(sapply(seq_along(psi.mod.avg),
                   extract.m,
                   data = psi.mod.avg,
                   index = indexHeight,
                   color = heightColor))
rownames(m.height) <- names
m.height[,1] <- as.numeric(m.height[,1])

png(filename = "results/graphics/boxplotTreeHeight.png",
    width = 4, height = 3, units = "in", pointsize = 12,
    bg = "white", res = 300, family = "Helvetica", restoreConsole = TRUE,
    type = 'cairo-png')
par(mar = c(3,4,1,1) + .1)

boxplot(as.numeric(m.height[-57,][which(m.height[-57,][,2]=='1'),1]),
        as.numeric(m.height[-57,][which(m.height[-57,][,2]=='red'),1]),
        notch = F,
        names = c('Cup nesters','Cavity nesters'),
        ylab = 'Slope of psi ~ canopy height (m)')
dev.off()

t.test(as.numeric(m.height[,1])~as.factor(m.height[,2]))


# most frequently observed spp
# 
spp.order <- spp[order(spp$Katie,decreasing = T),]
spp.trapped <- spp[which(spp$species %in% c('Loriculus galgulus','Copsychus malabaricus',
                                            'Irena puella','Alophoixus tephrogenys',
                                            'Argusanius argus','Chloropsis sonnerati',
                                            'Hydrornis schwaneri','Rhinoplax vigil',
                                            'Spilornis cheela','Platylophus galericulatus')),]
sum.trapped.spp <- rowSums(spp.trapped[,2:3])
names(sum.trapped.spp) <- spp.trapped$species

# habitat preferences

