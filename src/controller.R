##### Test parallel processing on at the subroutine inside runModelSet
# Author: Katherine Lauck
# Last updated: 23 July 2019
#
# To use this script, run occModInputs.R, which creates
# the source occupancy data for the analyses carried out by this script.
#
# To change the survey covariates available, modify surveyCov.R, and to change
# the site covariates, modify unitCov.R. Make sure you understand the required
# format for both covariate types.
#

# Initialization

source("src/SETTINGS.R")
source("src/functions.R")

# If desired, you can recreate the input from scratch:
#source('src/occModInputs.R')

 # import input files depending on whether this analysis will use dummy or normal
# dataset
importInput(DATASET)
occ.data <- makeFilenameVector(SPECIES_TO_BE_ANALYZED,DATASET)
names <- SPECIES_TO_BE_ANALYZED


# test: one species with parallel processing runModelSet
# unlink(paste0(OUTPUT_DIRECTORY,'/Alcippe brunneicauda'),recursive = T) # remove previous fill
# #dir.create(paste0(OUTPUT_DIRECTORY,'/Alcippe brunneicauda'))  # create directory
# system.time(runModelSet(paste0(INPUT_DIRECTORY,"/dummyHistories/Alcippe brunneicauda.csv"),
#             destination = OUTPUT_DIRECTORY,
#             #site.cov = site.cov.dummy,#[,c(1:8)], # drop grass and shrubs
#             survey.cov = survey.cov.dummy,
#             point.names = unlist(point.dummy,use.names = FALSE),
#             focus.param = "p"))
# brFul <- readRDS(paste0(OUTPUT_DIRECTORY,"/Alcippe brunneicauda/Alcippe brunneicaudaAIC.rds"))
# brFulEvidence <- summedWgt(names(survey.cov.dummy),param = "p",brFul)
# 
## all species with only survey covariates - later survey models for each
## species will be defined
# unlink(paste0(OUTPUT_DIRECTORY,'/survey'),recursive = T) # remove previous fill
# dir.create(paste0(OUTPUT_DIRECTORY,'/survey'))  # create directory
# results.spp.p <- lapply(occ.data,
#                         runModelSet,
#                         #site.cov = site.cov.dummy,
#                         destination = paste0(OUTPUT_DIRECTORY,"/survey"),
#                         survey.cov = survey.cov.dummy,
#                         point.names = unlist(point.dummy,use.names = FALSE),
#                         #cl = cl,
#                         #type = "so",
#                         focus.param = "p"
#                         )
# names(results.spp.p) <- names

## Model selection- save p structures with delta-AIC less than 10

# list and import summaries of AIC tables
aic.table.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/survey"),
                         pattern = ".*(AICsum\\.csv)$",
                         recursive = T,
                         full.names = T
)
aic.tables <- lapply(aic.table.files,
                     import)

# build list of p structures with delta AIC<10 for each species
p.models <- lapply(aic.tables,
                   function(x) return(sub("(psi\\(\\))$","",x$Model[which(x$DAIC<2)])))

# make list of variables to exclude based on evidence ratio
var.exclude <- list(c("cloudCover"),
                    c('j'),
                    c('j'),
                    c('j'),
                    c("start","cloudCover"),
                    c('j'),
                    c("start"),
                    c("start"),
                    c("observer1"),
                    c("cloudCover"),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c("cloudCover"),
                    c('j'),
                    c('start','cloudCover'),
                    c('start','SURVEY'),
                    c('j'),
                    c('cloudCover'),
                    c('cloudCover'),
                    c('j'),
                    c('cloudCover'),
                    c('start'),
                    c('j'),
                    c('SURVEY','start'),
                    c('j'),
                    c('start'),
                    c('j'),
                    c('start'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('SURVEY','cloudCover'),
                    c('start'),
                    c('j'),
                    c('start'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('cloudCover'),
                    c('cloudCover'),
                    c('cloudCover'),
                    c('cloudCover'),
                    c('start','cloudCover'),
                    c('start','SURVEY'),
                    c('start'),
                    c('cloudCover'),
                    c('j'),
                    c('cloudCover'),
                    c('cloudCover'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('start','cloudCover'),
                    c('j'),
                    c('start','cloudCover'),
                    c('cloudCover'),
                    c('start','cloudCover'),
                    c('j'),
                    c('cloudCover'),
                    c('j'),
                    c('cloudCover'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('j'),
                    c('start'),
                    c('start','cloudCover','observer1'),
                    c('j'),
                    c('cloudCover'),
                    c('observer1','cloudCover'))


names(var.exclude) <- names

# exclude models that include the uninformative variables

rem.var <- function(x,y) {
  x[grep(paste(y,collapse = "|"),x,invert = T)]
}

p.models <- foreach(i = 1:77) %do%
  rem.var(p.models[[i]],var.exclude[[i]])

# turn character vectors of models into appropriate form for runModelSet
p.form <- lapply(p.models,
                 function(x) return(lapply(x,
                                           make.formula,
                                           param = "p")))

names(p.form) <- names
# 
# one species test
# unlink(paste0(OUTPUT_DIRECTORY,'/habitat'),recursive = T) # remove previous fill
# dir.create(paste0(OUTPUT_DIRECTORY,'/habitat'))  # create directory
# runModelSet(paste0(INPUT_DIRECTORY,"/dummyHistories/Alcippe brunneicauda.csv"),
#             destination = paste0(OUTPUT_DIRECTORY,"/habitat"),
#             site.cov = site.cov.dummy[,c(1:5,8,10:12)], # drop anthro factors
#             survey.cov = survey.cov.dummy,
#             point.names = unlist(point.dummy,use.names = FALSE),
#             focus.param = "psi",
#             p.model = p.form)
# 
# # all species habitat output
# results.spp.habitat <- lapply(occ.data,
#                         runModelSet,
#                         site.cov = unit.cov.dummy[,c(1:5,8,10:12)], # drop anthro factors
#                         destination = "/Volumes/GoogleDrive/My Drive/occupancyOutput/habitat",
#                         survey.cov = survey.cov.dummy,
#                         point.names = unlist(point.dummy,use.names = FALSE),
#                         #cl = cl,
#                         #type = "so",
#                         focus.param = "psi",
#                         p.model = p.form
# )
# 

##### Build psi.form for detecting appropriate scales for each variable for each
##### species.
##### 
##### To do so, a set of 16 models describing habitat will be built; each will 
##### contain the same number of variables, so that the resulting AIC will 
##### compare only fit. The top model for each species will be used for the next
##### step of model selection.
##### 


# habitat.models <- expand.grid(grep("disturb",names(site.cov.dummy),value = T),
#             grep("forest",names(site.cov.dummy),value = T),
#             grep("ndvi",names(site.cov.dummy),value = T),stringsAsFactors = F)
# habitat.models[,4:6] <- rep(grep('disturb|forest|ndvi',
#                               names(site.cov.dummy),
#                               value = T,
#                               invert = T),each = 16)
# psi.form <- foreach(i = seq(1:length(habitat.models[,1]))) %do%
#   as.formula(paste('psi',paste(habitat.models[i,],collapse = ' + '),sep = ' ~ '))
# psi.form <- rep(list(psi.form),77)
# names(psi.form) <- names
# 
# #### Test one spp habitat output
# unlink(paste0(OUTPUT_DIRECTORY,'/habitat'),recursive = T) # remove previous fill
# dir.create(paste0(OUTPUT_DIRECTORY,'/habitat'))  # create directory
# # runModelSet(paste0(INPUT_DIRECTORY,"/dummyHistories/Alcippe brunneicauda.csv"),
# #             destination = paste0(OUTPUT_DIRECTORY,"/habitat"),
# #             site.cov = site.cov.dummy, # drop anthro factors
# #             survey.cov = survey.cov.dummy,
# #             point.names = unlist(point.dummy,use.names = FALSE),
# #             focus.param = "psi",
# #             p.model = p.form,
# #             psi.model = psi.form)
# 
# #### Run all spp habitat output
# 
# foreach(i=iter(occ.data)) %do%
#   runModelSet(i,
#               destination = paste0(OUTPUT_DIRECTORY,'/habitat'),
#               site.cov = site.cov.dummy,
#               survey.cov = survey.cov.dummy,
#               point.names = unlist(point.dummy,use.names = F),
#               focus.param = 'psi',
#               p.model = p.form,
#               psi.model = psi.form)
# 
# #### Retain top-ranked global model
# # list and import summaries of AIC tables
# habitat.aic.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/habitat"),
#                               pattern = ".*(AICsum\\.csv)$",
#                               recursive = T,
#                               full.names = T
# )
# habitat.aic.tables <- lapply(habitat.aic.files,
#                      import)
# 
# # capture global model for each sp 
# psi.models <- lapply(habitat.aic.tables,
#                    function(x) return(sub("^p\\((.*?)\\)",
#                                           "",
#                                           x$Model[1],
#                                           perl = T)))
# names(psi.models) <- names
# 
# # add anthro to selected global model per spp
# anthro.models <- expand.grid(grep("height",names(site.cov.dummy),value = T),
#                              grep("Road",names(site.cov.dummy),value = T),
#                              stringsAsFactors = F)
# 
# # turn top model from habitat determination into string
# anthro.psi.models <- lapply(psi.models,
#                             function(x) rep(list(make.formula.char(x,
#                                                                    param = 'psi')
#                                                  ),times = 2))
# 
# # add anthro factors to habitat model forms
# paste.anthro.factors <- function(global.model,anthro.factors){
#   as.formula(paste(global.model, paste(anthro.factors,collapse = ' + '),sep = ' + '))
# }
# 
# # iterate over internal list
# iterate.internal <- function(global.model,anthro.factors){
#   foreach(i = seq(1,length(anthro.factors[,1]))) %do%
#     paste.anthro.factors(global.model[[i]],anthro.factors[i,])
# }
# 
# # iterate over top level list
# anthro.psi.form <- lapply(anthro.psi.models,
#                             iterate.internal,
#                             anthro.models)
# 
# # Run all spp over anthro models to determine top ranked anthro scale
# unlink(paste0(OUTPUT_DIRECTORY,'/anthro'),recursive = T) # remove previous fill
# dir.create(paste0(OUTPUT_DIRECTORY,'/anthro'))
# foreach(i=iter(occ.data)) %do%
#   runModelSet(i,
#               destination = paste0(OUTPUT_DIRECTORY,'/anthro'),
#               site.cov = site.cov.dummy,
#               survey.cov = survey.cov.dummy,
#               point.names = unlist(point.dummy,use.names = F),
#               focus.param = 'psi',
#               p.model = p.form,
#               psi.model = anthro.psi.form)

# extract top-ranked global model per sp

# list and import summaries of AIC tables
global.aic.files <- list.files(path = paste0(OUTPUT_DIRECTORY,"/anthro"),
                                pattern = ".*(AICsum\\.csv)$",
                                recursive = T,
                                full.names = T
)
global.aic.tables <- lapply(global.aic.files,
                             import)
# export aic tables of 

# capture global model for each sp 
global.psi.models <- lapply(global.aic.tables,
                     function(x) return(sub("^p\\((.*?)\\)",
                                            "",
                                            x$Model[1],
                                            perl = T)))
names(global.psi.models) <- names

##### run all spp over global models per spp

# make global.psi.models into a list of factors
make.cov.list <- function(x) {
  unlist(str_split(sub('\\)','',sub('^psi\\(','',x)),' P '))
}

global.psi.cov <- lapply(global.psi.models,
                         make.cov.list)

# one-sp test
# unlink(paste0(OUTPUT_DIRECTORY,'/global'),recursive = T) # remove previous fill
# dir.create(paste0(OUTPUT_DIRECTORY,'/global'))  # create directory
# runModelSet(paste0(INPUT_DIRECTORY,"/dummyHistories/Alcippe brunneicauda.csv"),
#             destination = paste0(OUTPUT_DIRECTORY,"/global"),
#             site.cov = site.cov.dummy, # drop anthro factors
#             survey.cov = survey.cov.dummy,
#             point.names = unlist(point.dummy,use.names = FALSE),
#             focus.param = "psi",
#             p.model = p.form,
#             global.psi = global.psi.cov)

# THE BIG ONE (maybe???)
# unlink(paste0(OUTPUT_DIRECTORY,'/global'),recursive = T) # remove previous fill
# dir.create(paste0(OUTPUT_DIRECTORY,'/global'))  # create directory
# foreach(i=iter(occ.data)) %do%
#   runModelSet(i,
#               destination = paste0(OUTPUT_DIRECTORY,'/global'),
#               site.cov = site.cov.dummy,
#               survey.cov = survey.cov.dummy,
#               point.names = unlist(point.dummy,use.names = F),
#               focus.param = 'psi',
#               p.model = p.form,
#               global.psi = global.psi.cov)




