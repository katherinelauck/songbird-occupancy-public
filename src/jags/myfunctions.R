##### Functions for use in building data for and analyzing results from MCMC runs
# Author: Katherine Lauck
# Last updated: 16 May 2020
#
# These functions are personal helper functions used in main.R.

plot.sp.params<-function(param) {
  search<-paste(param,"\\[", sep="")	
  lam<-ss[grep(search, rownames(ss)),1]
  lam.ci<-ss[grep(search, rownames(ss)),c('2.5%','97.5%')]
  ii<-order(lam)
  plot(lam[ii], ylim=c(-3,3))
  arrows(x0=1:length(lam), y0=lam.ci[ii,1],y1=lam.ci[ii,2], length=0)
  abline(h=0, lty=2)
}


pmcmc<-function(var, sim.mat=res){
  sm1<-sim.mat$BUGSoutput$sims.matrix
  init.p<-length(which(sm1[,var]<=0))/length(sm1[,1])	
  p.val<-2*(0.5-abs(0.5-init.p))
  p.val
}