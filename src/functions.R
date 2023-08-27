##### Data-handling functions for occupancy modeling
# Author: Katherine Lauck
# Last updated: 18 January 2019
#
#
#

# Import the correct files for the intended analysis. Eventually this should
# just source the code to regen files each time, and reassign them to generic
# names that will be passed to the controller.
importInput <- function(dataset) {
  if(dataset == "dummy"){
    LIST <- list(
      survey.cov.dummy = import(paste0(INPUT_DIRECTORY,
                                       "/surveyCov/dummy/survey.cov.dummy.csv")),
      site.cov.dummy = import(paste0(INPUT_DIRECTORY,
                                     "/siteCov/dummy/unit.cov.dummy.csv")),
      point.dummy = import(paste0(INPUT_DIRECTORY,"/point.dummy.csv"))
    )
    list2env(LIST,envir = .GlobalEnv)
  }
  if(dataset == "original"){
    LIST <- list(
      survey.cov = import(paste0(INPUT_DIRECTORY,
                                 "/surveyCov/original/survey.cov.scaled.csv")),
      site.cov = import(paste0(INPUT_DIRECTORY,
                               "/siteCov/original/site.cov.scaled.csv")),
      points = import(paste0(INPUT_DIRECTORY,
                             "/detectionHistories/pointList.csv"))
    )
    list2env(LIST,envir = .GlobalEnv)
  }
  assign(
    "spp",
    import(paste0(INPUT_DIRECTORY,"/sppList.csv")),
    pos = .GlobalEnv
  )
}


# make a species name into a .csv full address. x is a vector of species names,
# dataset selects the correct dataset, and directory is the corresponding source
# folder.
makeFilename <- function(sppName,dataset){
  if(dataset == "dummy"){directory <- "dummyHistories"} else {
  if(dataset == "original"){directory <- "detectionHistories"}}
  return(
    paste0(INPUT_DIRECTORY,
           "/",
           directory,
           "/",
           sppName,
           ".csv"
    )
  )
}

makeFilenameVector <- function(spp,dataset){
  return(
    sapply(
      spp,
      makeFilename,
      dataset,
      USE.NAMES = FALSE
    )
  )
}


# turn character vectors into formulas of the form param ~ cov1 + cov2 + ...
# use later for psi model choices
make.formula <- function(x,param){
  form <- paste0(param,
    " ~ ",
    gsub(
      pattern = "( P )",
      " + ",
      sub(
        pattern = "\\)$",
        "",
        sub(
          pattern=paste0(
            "^",
            param,
            "\\(",
            collapse = ""
          ),
          "",
          x
  ))))
  if(form==paste0(param," ~ ")){
    form <- paste0(form,1,collapse = "")
  }
  return(as.formula(form))
}

make.formula.char <- function(x,param){
  form <- paste0(param,
                 " ~ ",
                 gsub(
                   pattern = "( P )",
                   " + ",
                   sub(
                     pattern = "\\)$",
                     "",
                     sub(
                       pattern=paste0(
                         "^",
                         param,
                         "\\(",
                         collapse = ""
                       ),
                       "",
                       x
                     ))))
  if(form==paste0(param," ~ ")){
    form <- paste0(form,1,collapse = "")
  }
  return(form)
}

##### Run a set of models built using RPresence, assuming the single season
##### occupancy model first described by McKenzie (2002)
# Author: Katherine Lauck
# Last updated: 2 November 2018

# Run a model set given site and survey covariates and a matrix of detection
# histories.
# site.cov =    data frame of site covariates (ncol = n covariates, nrow = n
#               site), either factors or continuous variables normalized to 0-1.
#               Missing site covariates are not allowed. The order of the sites
#               (i.e. rows) must be the same as the occ.data and point.names. If
#               not supplied, default is NULL, and model will be fixed at psi ~
#               1.
# survey.cov =  data frame of survey covariates (ncol = n covariates, nrow = n
#               site*n occasions), either factors or continuous variables
#               normalized to 0-1. Missing surveys should be indicated with NA.
#               The order of the sites (i.e. rows) must be the same as the
#               occ.data and point.names. If not supplied, default is NULL, and
#               model will be fixed at p ~ SURVEY or p ~ 1, depending on the
#               value supplied to incl.survey.
# data.path =   full directory path of matrix of detection histories (ncol = n
#               occasions, nrow = n sites). Missing surveys should be indicated
#               with NA.
# destination = full directory path of destination folder
# point.names = a character vector of the names of the points
# incl.survey = Should survey (i.e. occasion) be included as a covariate of p?
#               Default is 1.
# global.p,
# global.psi =  Character vector covariates describing the global model for p or psi,
#               or if iterating over more than one species, a list of lists of p
#               or psi covariates. Must be named with species' names. If set to 
#               NULL (the default) while p.model/psi.model as appropriate is set
#               to NULL, will behave as descibed below. If defined when p.model/
#               psi.model is set to NULL, will limit the global model to its 
#               elements. May not be defined while p.model/psi.model is also
#               defined.
# p.model,
# psi.model =   NULL or list of p or psi models, or if iterating over more than
#               one species, a list of lists of p or psi models. If set to NULL
#               (the default), the model set will include all the possible
#               combinations of the site or survey covariates as appropriate for
#               the parameter. The name of the list of models (or each component
#               list if iterating over multiple species) must be the intended
#               species' name.
# focus.param = Only required if a destination is set. Sets parameter to use
#               when storing model weights. Useful for model selection: focus
#               on p to select variables and models salient to detection, and 
#               focus on psi to select variables and models salient to 
#               occupancy.
# ... =         Other arguments to occMod (e.g. quiet, type)

# Value: RPresence AIC table comparing results from the heirarchical model set
# derived from all the possible combinations between site.cov and survey.cov.


runModelSet <- function(
  data.path,
  destination=NULL,
  site.cov=NULL,
  survey.cov=NULL,
  point.names,
  incl.survey=1,
  focus.param,
  global.p = NULL,
  global.psi = NULL,
  p.model = NULL,
  psi.model = NULL,
  ...
) {
  
  # Partial code to test for inputs that do not satisfy the requirements
  # Are there any badly formatted factors?
  # Of the continuous variables, are any not yet normalized?
  # if( any(site.cov < 0 | site.cov > 1) ) warning('x not between 0 and 1')
  # Are there any missing site covariates?
  # Are any missing surveys indicated with NA?
  # Do the missing surveys match up with missing survey data?
  # Unable to test whether the order of the site covariates is the same as the
  # detection history site order
  # Are there any missing names in the site or survey covariates?
  # Are the point names a character vector?
  #if( typeof(point.names) != "character") {
  #  stop("point names are not a character vector")
  #  }
  # Are there as many point names as rows in occ.data?
  if(!is.null(global.p)&&!is.null(p.model)){
    stop('One of either global.p or p.model must be NULL')
  }
  if(!is.null(global.psi)&&!is.null(psi.model)){
    stop('One of either global.psi or psi.model must be NULL')
  }
  # Create basic Pao object
  print(data.path)
  time1 <- Sys.time()
  occ.data <- data.frame(matrix(as.numeric(unlist(import(data.path))),
                                nrow = 235,ncol = 5))
  spp.name <- sub(
    "\\.csv$",
    "",
    sub(
      "^.*\\/(?!.*\\/)",
      "",
      data.path,
      perl = T),
    perl = T
  )
  pao <- createPao(
    occ.data,
    survcov = survey.cov,
    unitcov = site.cov,
    title = spp.name,
    unitnames = point.names
  )
  
  ### Build model set
  # Decide whether to build from scratch or use supplied model forms
  if(is.null(p.model)&&is.null(global.p)){
    # Decide whether survey should be included as a covariate, then build p set
    if(incl.survey == 1) {
      p.cov <- modCombos(param = "p", covs = c("SURVEY",names(survey.cov)))
    } else {
      p.cov <- modCombos(param = "p", covs = names(survey.cov))
    }
  } else if(is.null(global.p)) {
    p.cov <- p.model[[which(names(p.model)==spp.name)]]
  } else if(is.null(p.model)) {
    p.cov <- modCombos(param = 'p',
                       covs = global.p[[which(names(global.p)==spp.name)]])
  }
  
  if(is.null(psi.model)&&is.null(global.psi)){
    # build psi set
    psi.cov <- modCombos(param = "psi", covs = names(site.cov))
  } else if(is.null(global.psi)) {
    psi.cov <- psi.model[[which(names(psi.model)==spp.name)]]
  } else if(is.null(psi.model)) {
    psi.cov <- modCombos(param = 'psi',
                         covs = global.psi[[which(names(global.psi)==spp.name)]])
  }
  
  # Organize to correct form for occMod()
  modForm <- apply(
    expand.grid(p.cov,psi.cov),
    MARGIN = 1,
    function(x) list(x$Var1,x$Var2)
  )
  print(length((modForm)))
  # run models
  cl <- makeCluster(detectCores()-1)
  clusterExport(cl,c('modForm','pao'),envir = environment())
  clusterEvalQ(cl,library('RPresence'))
  run.models <- createAicTable(
    parLapply(
      modForm,
      fun = function(x) {
        gc()
        return(occMod(x,
               data = pao,
               type = "so",
               ...))},
      cl=cl
    )
    ,use.aicc=T
  )
  stopCluster(cl)
  gc()
  # Foreach version
  # cl <- makeCluster(detectCores()-1)
  # clusterExport(cl,c('modForm','pao'),envir = environment())
  # clusterEvalQ(cl,library('RPresence'))
  # registerDoParallel(cl)
  # 
  # run.models <- createAicTable(
  #   foreach(i = modForm) %dopar%
  #     occMod(i,data = pao,type = 'so',...)
  # , use.aicc = T)
  # 
  # stopCluster(cl)
  # gc()
  # 
  
  # cl <- makeCluster(detectCores()-1)
  # clusterExport(cl,c('modForm','pao'),envir = environment())
  # clusterEvalQ(cl,library('RPresence'))
  # dir.create(file.path(destination, spp.name))
  # clusterApply(cl = cl,x = seq_along(modForm),fun = function(x){
  #   saveRDS(occMod(modForm[[x]],data = pao,type = 'so',...),
  #           file = paste0(destination,'/',spp.name,'/',spp.name,x,'.rds'))
  #   gc()
  # })
  # 
  # run.models <- createAicTable(parLapply(cl = cl,
  #                         list.files(paste0(destination,'/',spp.name),
  #                                    full.names = T),
  #                         readRDS))
  # stopCluster(cl)
  # unlink(list.files(paste0(destination,'/',spp.name),full.names = T))
  # gc()
  ##### decide what output should occur. If a destination folder is not supplied,
  ##### the default output is to the R global environment
  if(is.null(destination)){
    print(paste("finished",data.path,sep = " "))
    return(run.models)
  } else {
    ##### if destination is supplied, write to file
    # create destination for species
    dir.create(file.path(destination, spp.name))
    # export AIC table
    export(
      summary(run.models),
      file = paste0(destination,"/",spp.name,"/",spp.name,"AICsum",".csv")
    )
    # export summed weights for one parameter
    if(focus.param=="psi"){
      param.names <- names(site.cov)
    } else if(focus.param=="p" && incl.survey==1){
      param.names <- c("SURVEY",names(survey.cov))
    } else if(focus.param=="p"){
      param.names <- names(survey.cov)
    }
    export(
      summedWgt(param.names,param = focus.param,run.models),
      file = paste0(
        destination,
        "/",
        spp.name,
        "/",
        spp.name,
        "sumWeight",
        focus.param,
        ".csv"
      )
    )
    # export AIC table, which includes the models
    saveRDS(
      run.models,
      file = paste0(destination,"/",spp.name,"/",spp.name,"AIC",".rds")
    )
    # create directory for data frames of site-specific parameter estimates
    # dir.create(
    #   file.path(
    #     paste0(destination,"/",spp.name),
    #     paste0("fitted",focus.param)
    #   )
    # )
    # export data frames of site-specific parameter estimates
    
    # models <- run.models$models
    # cl <- makeCluster(6)
    # clusterExport(cl,c('models','focus.param'),envir = environment())
    # clusterEvalQ(cl,library('rio'))
    # registerDoParallel(cl)
    # foreach(x = models) %dopar%
    #   export(
    #     fitted(x,param = focus.param),
    #     file = paste0(
    #       destination,
    #       "/",
    #       spp.name,
    #       "/",
    #       "fitted",
    #       focus.param,
    #       "/",
    #       x$modname,
    #       ".csv"
    #     )
    #   )
    gc()
    # lapply(
    #   run.models$models,
    #   function(x) export(
    #     fitted(x,param = focus.param),
    #     file = paste0(
    #       destination,
    #       "/",
    #       spp.name,
    #       "/",
    #       "fitted",
    #       focus.param,
    #       "/",
    #       x$modname,
    #       ".csv"
    #     )
    #   )
    #   #,cl = cl
    # )
    #stopCluster(cl)
    #gc()
    print(paste("finished",data.path,sep = " "))
    time2 <- Sys.time()
    print(time2-time1)
  }
}

# Test of function (not formal)

# problems encountered in past: 1) point names must be supplied as character
# vector; 2) mismatch between data lengths i.e. using longer dummy detection
# data with shorter original point list. Both of those caused persistent
# crashes with no error thrown. Super annoying but easy to fix!
# 
# #install.packages('devtools')
# library('devtools')
# library('rio')
# install('G:/My Drive/projects/songbird-occupancy/src/RPresence mac')
# #install('G:/My Drive/projects/songbird-occupancy/src/RPresence win')
# library('RPresence')
# library('parallel')
# #install.packages('pbapply')
# library('pbapply')
# 
# inputDir <- 'G:/My Drive/projects/songbird-occupancy/results/'
# ptL <- import(paste0(inputDir,'data/detectionHistories/pointList.csv'))
# dummy.pt <- import(paste0(inputDir,'data/point.dummy.csv'))
# survey.cov.dummy <- import(paste0(inputDir,'data/surveyCov/dummy/survey.cov.dummy.csv'))
# site.cov.dummy <- import(paste0(inputDir,'data/siteCov/dummy/unit.cov.dummy.csv'))
# test.survey.cov <- survey.cov.dummy[,c(2,3)]
# test.site.cov <- site.cov.dummy[,c(4,9)]
# #test.survey.cov <- data.frame(apply(test.survey.cov,2,as.numeric))
# #test.site.cov <- data.frame(apply(test.site.cov,2,as.numeric))
# test <- runModelSet(paste0(inputDir,"data/dummyHistories/Alcippe brunneicauda.csv"),
#                     destination = 'G:/My Drive/projects/songbird-occupancy/results/presenceOutput',
#                     site.cov = test.site.cov,
#                     survey.cov = test.survey.cov,
#                     point.names = as.character(unlist(dummy.pt,use.names = FALSE)),
#                     #type = "so",
#                     focus.param = 'psi'
#                     )
# 
# pao <- createPao(import(paste0(inputDir,"data/dummyHistories/Alcippe brunneicauda.csv")),
#                  title = 'test',
#                  unitnames = as.character(unlist(dummy.pt,use.names = FALSE)))
# test <- occMod(model = list(psi~1,p~SURVEY),data = pao,type = 'so')
# 
# 
# # Function detect, to be used later in a tapply command

detect <- function(x,pt.list,survey.list,t){
  
  # Creates a matrix of detections across sampling occasions
  # x - single-species detection records extracted from YPI point count data
  # pt.list - list of points used across entire survey
  # survey.list - by of lists with length t. Each componenet list contains the
  #               points surveyed during one sampling occasion.
  # t - number of sampling occasions
  
  # Create data frame to be filled. First column is the point list, and the
  # following columns will be filled with 1s where each species was detected.
  
  y <- cbind(point = pt.list,
             matrix(data = 0,
                    nrow = length(pt.list),
                    ncol = t,
                    dimnames = list(NULL,as.character(seq.int(1,t))
                    )
             )
  )
  
  # Fill in species detections
  
  for(i in seq.int(1,t)){
    temp <- x[which(x[,"occasion"]==as.integer(i)),]
    y[which(y[,"point"] %in% temp[,"point"]),i+1] <- 1
    y[which(!(y[,"point"] %in% survey.list[[i]])),i+1] <- NA
  }
  
  return(y)
  
}



##### function to merge together all the GIS files and rename the columns with the
##### appropriate data source tag
#####
##### x = a character string referring to one of the GIS outputs from averaging
##### buffers over various rasters. Must contain a column named "OBJECTID",
##### which is automatically generated by ArcGIS on import.
##### reference = a data sheet of point names and locations, used in ArcGIS to
##### generate the relevant buffers. Must contain a column named
##### "OBJECTID", which is automatically generated by ArcGIS on import.


mergeGISData <- function(x,ref){
  tmp <- import(x)
  nameAdd <- sub("\\.csv$","",sub('^(.)*\\/','',x))
  # if(nchar(nameAdd)>10){
  #   nameAdd <- substr(nameAdd,1,10)
  # }
  if("MEAN" %in% colnames(tmp)){
    tmp <- tmp[,c("OBJECTID_1","MEAN","STD")]
    colnames(tmp)[c(2,3)] <- c(paste0(nameAdd,colnames(tmp)[2]),
                               paste0(nameAdd,colnames(tmp)[3]))
  } else if(grepl(x = x,
                  pattern = 'CIFOR')){
    tmp <- tmp[,c('OBJECTID_1',
                  grep(x = colnames(tmp),
                       pattern = 'CIFOR',
                       value = T),
                  'PERCENTAGE')]
    tmp <- spread(tmp,'CIFOR Forest type class','PERCENTAGE',fill = 0)
    colnames(tmp)[2:4] <- c(paste0('Percent',
                                   gsub('-','',gsub(" ","",colnames(tmp)[2])),
                                   str_extract(nameAdd,'(100|500|1000|1500)m')),
                            paste0('Percent',
                                   gsub('-','',gsub(" ","",colnames(tmp)[3])),
                                   str_extract(nameAdd,'(100|500|1000|1500)m')),
                            paste0('Percent',
                                   gsub('-','',gsub(" ","",colnames(tmp)[4])),
                                   str_extract(nameAdd,'(100|500|1000|1500)m')))
    
  }else if(any(grep(x = colnames(tmp),
                    pattern = 'Percent',
                    value = F))){
    tmp <- tmp[,c('OBJECTID_1',grep(x = colnames(tmp),
                                    pattern = 'Percent',
                                    value = T))]
    colnames(tmp)[2] <- paste0(nameAdd,'Percent')
  }
  
  ref[,'OBJECTID_1'] <- as.character(ref[,'OBJECTID_1'])
  tmp[,'OBJECTID_1'] <- as.character(tmp[,'OBJECTID_1'])
  ref <- left_join(ref,tmp,by = "OBJECTID_1")
  # if("MAJORITY" %in% colnames(reference)){
  #   colnames(reference) <- c(colnames(reference)[1:(length(colnames(reference))-3)],
  #                          paste0(nameAdd,colnames(reference[(length(colnames(reference))-2)])),
  #                          paste0(nameAdd,colnames(reference[(length(colnames(reference))-1)])),
  #                          paste0(nameAdd,colnames(reference[length(colnames(reference))])))
  #   } else {
  #   colnames(reference) <- c(colnames(reference)[1:(length(colnames(reference))-4)],
  #                            paste0(nameAdd,colnames(reference[(length(colnames(reference))-3)])),
  #                            paste0(nameAdd,colnames(reference[(length(colnames(reference))-2)])),
  #                            paste0(nameAdd,colnames(reference[(length(colnames(reference))-1)])),
  #                            paste0(nameAdd,colnames(reference[length(colnames(reference))])))
  #   }
  return(ref)
}

exportFitted <- function(run.models,destination,spp.name,focus.param){
  dir.create(
    file.path(
      paste0(destination,"/",spp.name),
      paste0("fitted",focus.param)
    )
  )
  lapply(
    run.models$models,
    function(x) export(
      fitted(x,param = focus.param),
      file = paste0(
        destination,
        "/",
        spp.name,
        "/",
        "fitted",
        focus.param,
        "/",
        x$modname,
        ".csv"
      )
    )
    #,cl = cl
  )
}

# access parameter estimates
extract.model.averages <- function(path) {
  ### Extract model averages from aic tables stored as .rds objects. To avoid 
  ### overtaxing the memory used by RStudio, files processed in a sequential 
  ### manner, with each rds object imported, processed, then deleted.
  
  psi.avg <- 
    path %>% 
    readRDS %>%
    modAvg(param = 'psi')
  gc()
  return(psi.avg)
  
}

# access beta values
extract.beta <- function(path) {
  ### Extract beta from aic tables stored as .rds objects. To avoid 
  ### overtaxing the memory used by RStudio, files processed in a sequential 
  ### manner, with each rds object imported, processed, then deleted.
  
  rds <- 
    path %>% 
    readRDS
  beta <- lapply(rds$models,FUN = coef,param = 'psi')
  gc()
  return(beta)
  
}

extract.psi.est <- function(path) {
  ### Extract mean & sd of site-specific estimated psi from aic tables stored as
  ### .rds objects. To avoid overtaxing the memory used by RStudio, files 
  ### processed in a sequential manner, with each rds object imported, 
  ### processed, then deleted.
  
  rds <- 
    path %>% 
    readRDS;
  psi.est <- lapply(rds$models,FUN = fitted,param = 'psi');
  avg.psi <- sapply(psi.est,
                    function(x) return(c(mean(x$est),
                                         sd(x$est)
                                         )
                                       )
                    )
  gc()
  return(avg.psi)
  
}

extract.m <- function(i,data,index,color = 1){
  #### Add lines to plot intended to show how traded and non-traded spp differ 
  #### in their response to tree height and road distance.
  # model regression
  reg <- lm(data[[i]][index[[i]],1]~seq(from = 0, to = 1, length.out = 30))
  return(c(reg$coefficients[2],color[[i]]))
}

plot.one.spp <- function(i,index,cov,color = 1){
  #### Add lines to plot intended to show how traded and non-traded spp differ 
  #### in their response to tree height and road distance.
  # model regression
  reg <- lm(psi.mod.avg[[i]][index[[i]],1]~seq(from = 0, to = 1, length.out = 30))
  new.psi <- psi.mod.avg[[i]][index[[i]],1]
  plot(new.psi~cov,col = color[[i]],type = 'b',xlim = c(min(cov),max(cov)),
       ylim = c(0,1), xlab = paste0('m = ',reg$coefficients[2]),
       main = paste0(names(psi.mod.avg)[i]))
}

# add lines to plot
add.spp.to.plot <- function(i,index,cov,color = 1){
  #### Add lines to plot intended to show how traded and non-traded spp differ 
  #### in their response to tree height and road distance.
  # model regression
  reg <- lm(psi.mod.avg[[i]][index[[i]],1]~seq(from = 0, to = 1, length.out = 30))
  new.psi <- psi.mod.avg[[i]][index[[i]],1]-reg$coefficients[1]
  lines(new.psi~cov,col = color[[i]])
}

# make model tables per species
model.tables <- function(x,tables,type,aic.tables = NULL,psi.est = NULL){
  out <- paste0(PROJECT_DIRECTORY,'/results/graphics/',type)
  dir.create(out)
  if(type == 'aic'){
  stargazer(tables[[x]][which(tables[[x]]$DAIC <= 3),],
            type = 'html',
            out = paste0(out,'/',x,'.doc'),
            summary = F)
  } else if(type == 'beta'){
    # extract one set of betas
    temp <- tables[[x]][which(aic.tables[[x]]$DAIC <= 3)]
    # extract AIC-relevant set of est psi
    psi <- psi.est[[x]][,which(aic.tables[[x]]$DAIC <= 3)]
    # set up model name and beta value columns
    table <- cbind(Model = aic.tables[[x]]$Model[which(aic.tables[[x]]$DAIC <= 3)],
                   Psi = apply(psi,1,function(x) tryCatch(paste0(x[1],' \U00B1 ',x[2]),
                                                          finally = return('NA'))),
                   matrix(ncol = length(unique(unlist(sapply(temp,FUN = rownames))))-1,
                          nrow = length(temp),
                          dimnames = list(NULL,unique(unlist(sapply(temp,FUN = rownames)))[-1])),
                   deparse.level = 0)
    # transfer values from temp to table, where each beta value is written as mean +/- se
    for(i in seq(length(temp))){
      # transfer the mean & se to the correct cell of the table
      for(j in rownames(temp[[i]])[-1]){
        table[i,which(j == colnames(table))] <- 
          paste(round(temp[[i]][j,1],digits = 2),
                ' \U00B1 ',
                round(temp[[i]][j,2],digits = 2))
      }
    }
    
    stargazer(table,
              type = 'html',
              out = paste0(out,'/',x,'.doc'),
              summary = F)
  }
}

