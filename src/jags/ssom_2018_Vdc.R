#### Single-season occupancy model using 2018 data collected by PII in Cagar Alam Gunung Nyiut
#### Author: Katherine Lauck
#### Email: kslauck@ucdavis.edu
#### Last updated: 27 May 2020
#### 
#### This version uses %disturbed canopy (dc) as a measure of disturbance.


ms.ms <- function(d,
                  ni=1100,
                  nt=100,
                  nb=100,
                  nc=3,
                  init.model=res) {
  
  model.jags<-function() {
    ## *******************************************************
    ## Detection priors
    ## *******************************************************
    
    ## Random effects
    for (sp in 1:nsp) {
      p.sp[sp] ~ dnorm(0,tau.p.sp)
      # p.obs[sp] ~ dnorm(mu.p.obs,tau.p.obs)
      # p.date[sp] ~ dnorm(mu.p.date,tau.p.date)
    }
    for (site in 1:nsite) {
      p.site[site] ~ dnorm(0,tau.p.site)
      # for (rep in 1:nrep.run[site]) {
      #   p.rep[site,rep] ~ dnorm(0,tau.p.rep)
      # }
    }
    
    ## Hyperparameters
    sigma.p.sp ~ dunif(0.001,10)
    tau.p.sp <- 1/(sigma.p.sp*sigma.p.sp)
    
    # mu.p.obs ~ dnorm(0, 0.01)
    # sigma.p.obs ~ dunif(0.001,10)
    # tau.p.obs <- 1/(sigma.p.obs*sigma.p.obs)
    
    # mu.p.date ~ dnorm(0, 0.01)
    # sigma.p.date ~ dunif(0.001,10)
    # tau.p.date <- 1/(sigma.p.date*sigma.p.date)
    
    sigma.p.site ~ dunif(0.001,10)
    tau.p.site <- 1/(sigma.p.site*sigma.p.site)
    
    # sigma.p.rep ~ dunif(0.001,10)
    # tau.p.rep <- 1/(sigma.p.rep*sigma.p.rep)
    
    ## Fixed effects
    
    p.obs ~ dnorm(0,0.01)
    p.time ~ dnorm(0, 0.01)
    # p.cloud ~ dnorm(0, 0.01)
    
    ## *******************************************************
    ## Detection estimation
    ## *******************************************************
    
    for (sp in 1:nsp){	    
      for (site in 1:nsite){	
          for (rep in 1:5){
            logit(p[site, sp, rep]) <- 
              # Random effects
              p.site[site] +
              p.sp[sp] +
              # p.rep[site,rep] +
              p.obs*obs[site,rep] + # should observer have a per sp effect? See Q2
              # p.date[sp]*date[site,rep] +
              # Fixed effects
              p.time*time[site,rep]
              # p.cloud*cloud[site,rep]
            
            
            p.eff[site,sp,rep] <- p[site,sp,rep]*visited[site,rep] # see Q1
          } #/rep
      } #/site
    } #/sp
    
    ## *******************************************************
    ## Occupancy priors
    ## *******************************************************
    
    ## Random slope
    for (sp in 1:nsp) {
      lambda.0[sp] ~ dnorm(mu.lambda.0,tau.lambda.0)
      # lambda.us[sp] ~ dnorm(mu.lambda.us,tau.lambda.us)
      lambda.water[sp] ~ dnorm(mu.lambda.water,tau.lambda.water)
      # lambda.grass[sp] ~ dnorm(mu.lambda.grass,tau.lambda.grass)
      # lambda.shrubs[sp] ~ dnorm(mu.lambda.shrubs,tau.lambda.shrubs)
      # lambda.tf[sp] ~ dnorm(mu.lambda.tf,tau.lambda.tf)
      # lambda.ele[sp] ~ dnorm(mu.lambda.ele,tau.lambda.ele)
      # lambda.slope[sp] ~ dnorm(mu.lambda.slope,tau.lambda.slope)
      lambda.ch[sp] ~ dnorm(mu.lambda.ch,tau.lambda.ch)
      # lambda.intf[sp] ~ dnorm(mu.lambda.intf,tau.lambda.intf)
      # lambda.nf[sp] ~ dnorm(mu.lambda.nf,tau.lambda.nf)
      # lambda.rf[sp] ~ dnorm(mu.lambda.rf,tau.lambda.rf)
      # lambda.ndvi[sp] ~ dnorm(mu.lambda.ndvi,tau.lambda.ndvi)
      # lambda.tc[sp] ~ dnorm(mu.lambda.tc,tau.lambda.tc)
      # Parameter of interest
      lambda.dr[sp] ~ dnorm(mu.lambda.dr,tau.lambda.dr)
      lambda.dc[sp] ~ dnorm(mu.lambda.dc,tau.lambda.dc)
      # lambda.intf.com[sp] ~ dnorm(mu.lambda.intf.com,tau.lambda.intf.com)
      lambda.dr.com[sp] ~ dnorm(mu.lambda.dr.com,tau.lambda.dr.com)
      lambda.dc.com[sp] ~ dnorm(mu.lambda.dc.com,tau.lambda.dc.com)
      # for (site in 1:nsite) {
      # lambda.site.sp[site,sp] ~ dnorm(mu.lambda.site.sp,tau.lambda.site.sp)
      # }
    }
    for (site in 1:nsite){
      lambda.site[site] ~ dnorm(0,tau.lambda.site)
    }
    
    for (transect in 1:ntransect){
      lambda.transect[transect] ~ dnorm(0,tau.lambda.transect)
    }
    
    ## Hyperparameters
    
    # species random variables
    mu.lambda.0 ~ dnorm(0, 0.01)
    sigma.lambda.0 ~ dunif(0.001,10)
    tau.lambda.0 <- 1/(sigma.lambda.0*sigma.lambda.0)
    
    # mu.lambda.us ~ dnorm(0, 0.01)
    # sigma.lambda.us ~ dunif(0.001,10)
    # tau.lambda.us <- 1/(sigma.lambda.us*sigma.lambda.us)
    
    mu.lambda.water ~ dnorm(0, 0.01)
    sigma.lambda.water ~ dunif(0.001,10)
    tau.lambda.water <- 1/(sigma.lambda.water*sigma.lambda.water)
    
    # mu.lambda.grass ~ dnorm(0, 0.01)
    # sigma.lambda.grass ~ dunif(0.001,10)
    # tau.lambda.grass <- 1/(sigma.lambda.grass*sigma.lambda.grass)
    
    # mu.lambda.shrubs ~ dnorm(0, 0.01)
    # sigma.lambda.shrubs ~ dunif(0.001,10)
    # tau.lambda.shrubs <- 1/(sigma.lambda.shrubs*sigma.lambda.shrubs)
    
    # mu.lambda.tf ~ dnorm(0, 0.01)
    # sigma.lambda.tf ~ dunif(0.001,10)
    # tau.lambda.tf <- 1/(sigma.lambda.tf*sigma.lambda.tf)
    
    # mu.lambda.ele ~ dnorm(0, 0.01)
    # sigma.lambda.ele ~ dunif(0.001,10)
    # tau.lambda.ele <- 1/(sigma.lambda.ele*sigma.lambda.ele)
    
    # mu.lambda.slope ~ dnorm(0, 0.01)
    # sigma.lambda.slope ~ dunif(0.001,10)
    # tau.lambda.slope <- 1/(sigma.lambda.slope*sigma.lambda.slope)
    
    mu.lambda.ch ~ dnorm(0, 0.01)
    sigma.lambda.ch ~ dunif(0.001,10)
    tau.lambda.ch <- 1/(sigma.lambda.ch*sigma.lambda.ch)

    # mu.lambda.intf ~ dnorm(0, 0.01)
    # sigma.lambda.intf ~ dunif(0.001,10)
    # tau.lambda.intf <- 1/(sigma.lambda.intf*sigma.lambda.intf)
    
    # mu.lambda.nf ~ dnorm(0, 0.01)
    # sigma.lambda.nf ~ dunif(0.001,10)
    # tau.lambda.nf <- 1/(sigma.lambda.nf*sigma.lambda.nf)
    
    # mu.lambda.rf ~ dnorm(0, 0.01)
    # sigma.lambda.rf ~ dunif(0.001,10)
    # tau.lambda.rf <- 1/(sigma.lambda.rf*sigma.lambda.rf)
    
    # mu.lambda.ndvi ~ dnorm(0, 0.01)
    # sigma.lambda.ndvi ~ dunif(0.001,10)
    # tau.lambda.ndvi <- 1/(sigma.lambda.ndvi*sigma.lambda.ndvi)
    
    # mu.lambda.tc ~ dnorm(0, 0.01)
    # sigma.lambda.tc ~ dunif(0.001,10)
    # tau.lambda.tc <- 1/(sigma.lambda.tc*sigma.lambda.tc)
    
    # parameter of interest
    mu.lambda.dr ~ dnorm(0, 0.01)
    sigma.lambda.dr ~ dunif(0.001,10)
    tau.lambda.dr <- 1/(sigma.lambda.dr*sigma.lambda.dr)
    
    mu.lambda.dc ~ dnorm(0, 0.01)
    sigma.lambda.dc ~ dunif(0.001,10)
    tau.lambda.dc <- 1/(sigma.lambda.dc*sigma.lambda.dc)
    
    # mu.lambda.intf.com ~ dnorm(0, 0.01)
    # sigma.lambda.intf.com ~ dunif(0.001,10)
    # tau.lambda.intf.com <- 1/(sigma.lambda.intf.com*sigma.lambda.intf.com)
    
    mu.lambda.dr.com ~ dnorm(0, 0.01)
    sigma.lambda.dr.com ~ dunif(0.001,10)
    tau.lambda.dr.com <- 1/(sigma.lambda.dr.com*sigma.lambda.dr.com)
    
    mu.lambda.dc.com ~ dnorm(0, 0.01)
    sigma.lambda.dc.com ~ dunif(0.001,10)
    tau.lambda.dc.com <- 1/(sigma.lambda.dc.com*sigma.lambda.dc.com)
    
    # site random variables
    # mu.lambda.site.sp ~ dnorm(0, 0.01)
    # sigma.lambda.site.sp ~ dunif(0.001,10)
    # tau.lambda.site.sp <- 1/(sigma.lambda.site.sp*sigma.lambda.site.sp)
    
    sigma.lambda.site ~ dunif(0.001,10)
    tau.lambda.site <- 1/(sigma.lambda.site*sigma.lambda.site)
    
    sigma.lambda.transect ~ dunif(0.001,10)
    tau.lambda.transect <- 1/(sigma.lambda.transect*sigma.lambda.transect)
    
    
    ## Fixed effects
    
    
    ## *******************************************************
    ## Occupancy estimation
    ## *******************************************************
    
    for (sp in 1:nsp){
      for(site in 1:nsite) {
          logit(psi[site,sp]) <- 
            # Random effects
            lambda.0[sp] + # each sp has a different base occupancy (some are rare)
            # lambda.site.sp[site,sp] + # each sp has a different base occ at each site (maybe don't include? Since this is such a small area? see Q3)
            lambda.site[site] + # each site has residual quality differences that are unexplained by microhabitat variables (see Q4)
            lambda.transect[transect[site]] + # spatial autocorrelation random effect Q7
            # lambda.us[sp]*us[site] + # each sp has a different relationship to understory
            lambda.water[sp]*water[site] + # each sp has a different relationship to water
            # lambda.grass[sp]*grass[site] + # each sp has a different relationship to grass
            # lambda.shrubs[sp]*shrubs[site] + # each sp has a different relationship to shrubs
            # lambda.tf[sp]*tf[site] + # each sp has a different relationship to treefall
            # lambda.ele[sp]*ele[site] + # each sp has a different relationship to elevation
            # lambda.slope[sp]*slope[site] + # each sp has a different relationship to slope
            lambda.ch[sp]*ch[site] + # each sp has a different relationship to canopy height

            # lambda.intf[sp]*intf[site] + # each sp has a different relationship to intact forest (500 m)
            # lambda.nf[sp]*nf[site] + # each sp has a different relationship to nonforest
            # lambda.rf[sp]*rf[site] + # each sp has a different relationship to regrowth
            # lambda.ndvi[sp]*ndvi[site] + # each sp has a different relationship to NDVI
            # lambda.tc[sp]*tc[site] + # each sp has a different relationship to tree cover
            # Parameters of interest
            lambda.dr[sp]*dist.road[site] +
            lambda.dc[sp]*dist.cano[site] +
            # lambda.intf.com[sp]*intf[site]*commercial[sp] + # commercial sp have a different sloped relationship with intact forest than non-commercial sp
            lambda.dr.com[sp]*dist.road[site]*commercial[sp] + # each sp has a different relationship to distance to roads
            lambda.dc.com[sp]*dist.cano[site]*commercial[sp] # each sp has a different relationship to disturbed canopy. Should the commercial factor be swapped with a forest preference factor? Also I think I can extract the prices for most of these sp from Rentschlar et al 2018, should I use that instead, and exclude the spp for which I don't have prices? I might also be able to find out market prices for the other ones from contacts from my collaborator. Q9
            
            
          Z[site, sp] ~ dbern(psi[site,sp]) # Bernoulli realization of true occupancy
          
          ## *******************************************************
          ## Detection process
          ## *******************************************************
          
          for(rep in 1:nrep.run[site]) { 
            X[site,sp,rep] ~ dbern(p.eff[site,sp,rep]*Z[site,sp]) # Given presence, detection
          } #/rep
      } #/site
    } #/sp
    
    ## *******************************************************
    ## Derived parameters
    ## *******************************************************
    
    
    
  } #/model


## specify the parameters to be monitored
params <- 
  c(
    'Z',
    'sigma.p.site',
    'sigma.p.sp',
    'p.obs',
    'p.time',
    #'lambda.0',
    'mu.lambda.0',
    #'lambda.site',
    'sigma.lambda.site',
    #'lambda.transect',
    'sigma.lambda.transect',
    'lambda.water',
    'mu.lambda.water',
    'sigma.lambda.water',
    # 'lambda.ele',
    # 'mu.lambda.ele',
    # 'sigma.lambda.ele',
    'lambda.dr',
    'mu.lambda.dr',
    'sigma.lambda.dr',
    'lambda.dc',
    'mu.lambda.dc',
    'sigma.lambda.dc',
    'lambda.dr.com',
    'mu.lambda.dr.com',
    'sigma.lambda.dr.com',
    'lambda.dc.com',
    'mu.lambda.dc.com',
    'sigma.lambda.dc.com',
    'lambda.ch',
    'mu.lambda.ch',
    'sigma.lambda.ch'
    # 'lambda.intf',
    # 'mu.lambda.intf',
    # 'sigma.lambda.intf',
    # 'lambda.intf.com',
    # 'mu.lambda.intf.com',
    # 'sigma.lambda.intf.com'
  )

## Specify initial values
XX<-d$data$X

my.inits <- function() {
  list(Z=apply(XX, c(1,2), max)
  )
}


d$data.use<-d$data[c('X',
                     'nsp',
                     'nsite',
                     'nrep.run',
                     'obs',
                     'time',
                     'ntransect',
                     'transect',
                     'water',
                     # 'ele',
                     'dist.road',
                     'dist.cano',
                     'commercial',
                     'visited',
                     'ch',
                     'intf'
)]

# attach(d$data)


jags.parallel(data=d$data.use,
              inits= my.inits,
              parameters.to.save= params,
              model.file=model.jags,
              n.chains=nc,
              n.thin=nt,
              n.iter=ni,
              n.burnin=nb,
              working.directory=NULL)
} #/ms.ms