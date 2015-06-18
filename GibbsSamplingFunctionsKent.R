library(mlbench)
library(MASS)
library(lars)
library(Matrix)
library(lattice)
library(truncnorm)
library(MCMCpack)

# Gibb Sampling Functions -------------------------------------------------

# Linear Model Functions

sampleB = function(y,X,sig2,tau2)
{
  p = dim(X)[2]
  SIG1inv = (1/sig2)*((t(X))%*%X) + (1/tau2)*diag(p)
  SIG1 = solve(SIG1inv)
  m = (1/sig2)*SIG1%*%t(X)%*%y
  draw = mvrnorm(1,m,SIG1)
  return(draw)
}

samplesig2 = function(y,X,B,a,b)
{
  n = length(y)
  p1 = (n/2) + (a/2)
  p2 = as.numeric(( (t(y - (X%*%B)))%*%(y - (X%*%B)) + b )/2)
  draw = rgamma(1,p1,p2)
  return(1/draw)
}

sampletau2 = function(B,c,d)
{
  p = length(B)
  p1 = (p/2) + (c/2)
  p2 = ( (t(B))%*%B + d )/2
  draw = rgamma(1,p1,p2)
  return(1/draw)
}

reportMCMC = function(obj)
{
  draws = obj
  if(!is.null(dim(draws)))
  {
    for (i in 1:dim(draws)[2])
    {
      ts.plot(draws[,i],xlab="iterations",ylab="",main=colnames(draws)[i]) 
      abline(h=mean(draws[,i]),col=2,lwd=2)
      acf(draws[,i],main="") 
      hist(draws[,i],prob=T,main="",xlab="") 
      abline(v=mean(draws[,i]),col=2,lwd=2)
    }
  }
  else
  {
    ts.plot(draws,xlab="iterations",ylab="",main=names(draws)[1]) 
    abline(h=mean(draws),col=2,lwd=2)
    acf(draws,main="") 
    hist(draws,prob=T,main="",xlab="") 
    abline(v=mean(draws),col=2,lwd=2)
    
  }
}

# Mean Model Functions

sampleBmean = function(y,X,sig2,tau2,mu)
{
  #   print(tau2)
  Tmat = diag(tau2)
  Tmatinv = diag(1/tau2)
  SIG1inv = (1/sig2)*((t(X))%*%X) + Tmatinv
  SIG1 = ginv(as.matrix(SIG1inv))
  m = (1/sig2)*SIG1%*%t(X)%*%y + SIG1%*%Tmatinv%*%mu
  draw = mvrnorm(1,m,SIG1)
  return(draw)
}

samplesig2mean = function(y,X,B)
{
  n = length(y)
  p1 = (n+4)/2
  p2 = as.numeric( ( (t(y - (X%*%B)))%*%(y - (X%*%B)) )/2 )
  draw = rgamma(1,p1,p2)
  return(1/draw)
}

sampletau2mean = function(y,B)
{
  NS = length(B)
  p1 = (NS+3)/2
  p2 = ( (t(B))%*%B )/2
  draw = rgamma(1,p1,p2)
  return(1/draw)
}

samplemumean = function(y,B,sig2,tau2,m,v2)
{
  NS = length(B)
  thetabar = mean(B)
  sigmu2 = 1/(NS/tau2 + 1/v2)
  mmu = sigmu2*((NS/tau2)*thetabar + (1/v2)*m)
  draw = rnorm(1,mmu,sqrt(sigmu2))
  return(draw)
}

# Data augmentation functions
sampleBaug = function(z,X,mu,sig20)
{
  numpred = dim(X)[2]
  Tmat = diag(sig20,numpred)
  Tmatinv = diag(1/sig20,numpred)
  SIGBinv = (t(X))%*%X + Tmatinv
  SIGB = ginv(as.matrix(SIGBinv))
  m = SIGB%*%(t(X)%*%(z-mu))
  
#   m = ginv(t(X)%*%X)%*%t(X)%*%(z-mu)
#   SIGB = ginv(t(X)%*%X)
#   
  draw = mvrnorm(1,m,SIGB)
  return(draw)
}

applyfunc = function(vec,B)
{
  y = vec[1]
  mu = vec[2]
  X = vec[3:length(vec)]

  if(y==1){ 
    zsamp=rtruncnorm(1,0,Inf,mean= mu + t(X)%*%B,sd=1) 
  }
  if(y==0){ 
    zsamp=rtruncnorm(1,-Inf,0,mean= mu + t(X)%*%B,sd=1)  
  }
  return(zsamp)
}

samplezaug = function(y,X,B,mu)
{
  N = length(y)
  V = cbind(y,mu,X)
  zsamp = apply(V,MARGIN = 1,applyfunc,B)
  return(zsamp)
}

sampletau2aug = function(mu)
{
  NS = length(mu)
  p1 = (NS+3)/2
  p2 = ( (t(mu))%*%mu )/2
  draw = rgamma(1,p1,p2)
  return(1/draw)
}

samplemuaug = function(zmxB,tau2)
{
  NS = length(zmxB)
  thetabar = mean(zmxB)
  sigmu2 = 1/(NS + 1/tau2)
  mmu = sigmu2*(NS*thetabar)
  draw = rnorm(1,mmu,sqrt(sigmu2))
  return(draw)
}

Gibbswrapperaug = function(loops,y,X,stateID)
{
  # prior on beta vector
  sig20 = 50
  
  #other stuff
  numstates = 49
  N = length(y)
  numpred = dim(X)[2]
  BMCMC = matrix(0,numpred,loops)
  zMCMC = matrix(0,N,loops)
  tau2MCMC = rep(0,loops)
  muMCMC = matrix(0,numstates,loops)
  names(tau2MCMC) = rep(paste('tau2'),loops)
  rownames(muMCMC) = paste('mu',1:numstates)
  
  BMCMC[,1] = rep(1,numpred)
  zMCMC[,1] = rep(1,N)
  tau2MCMC[1] = 1
  muMCMC[,1] =rep(1,numstates) 
  
  for(i in 2:loops)
  {
    if(i%%5==0){print(noquote(paste('MCMC iter =',i)))}
    
    # build current mu
    
    mucurrent = rep(0,N)
    for(j in 1:numstates)
    {
      ind = stateID[[j]]
      mucurrent[ind] = rep( muMCMC[j,(i-1)],length(ind) )  
    }
    
    #construct tau2 and mu draws
    BMCMC[,i] = as.numeric(sampleBaug(zMCMC[,(i-1)],X,mucurrent,sig20))
    zMCMC[,i] = as.numeric(samplezaug(y,X,BMCMC[,i],mucurrent))
    tau2MCMC[i] = sampletau2aug(mucurrent)
    for(j in 1:numstates)
    {
      ind = stateID[[j]]
      zxmB = zMCMC[ind,i] - X[ind,]%*%BMCMC[,i]
      muMCMC[j,i] = samplemuaug(zxmB,tau2MCMC[i])
    }
    
  }
  return(list(BMCMC,zMCMC,tau2MCMC,muMCMC))
}

Gibbswrapper = function(loops,y,X,NS)
{
  # prior on mui
  m = 0
  sig02 = 50
  
  #other stuff
  size = dim(X)[2]
  numpred = size / NS
  BMCMC = matrix(0,size,loops)
  sig2MCMC = rep(0,loops)
  names(sig2MCMC) = rep('sig2',length(sig2MCMC))
  tau2MCMC = matrix(0,numpred,loops)
  muMCMC = matrix(0,numpred,loops)
  rownames(tau2MCMC) = paste('tau2',1:numpred)
  rownames(muMCMC) = paste('mu',1:numpred)
  
  BMCMC[,1] = rep(1,size)
  sig2MCMC[1] = 1
  tau2MCMC[,1] = rep(1,numpred)
  muMCMC[,1] = rep(1,numpred) 
  loopind = seq(from = 1,to = size,by = numpred)
  
  for(i in 2:loops)
  {
    if(i%%50==0){print(noquote(paste('MCMC iter =',i)))}
    
    BMCMC[,i] = as.numeric(sampleBmean(y,X,sig2MCMC[i-1],rep(tau2MCMC[,i-1],NS),rep(muMCMC[,i-1],NS)))
    sig2MCMC[i] = samplesig2mean(y,X,BMCMC[,i])
    
    for(j in 1:numpred)
    {
      predin = (loopind+j-1)
      tau2MCMC[j,i] = sampletau2mean(y,(BMCMC[predin,i]-muMCMC[j,i-1]))
      muMCMC[j,i] = samplemumean(y,BMCMC[predin,i],sig2MCMC[i],tau2MCMC[j,i],m,sig02)    
    }
  }
  return(list(BMCMC,sig2MCMC,tau2MCMC,muMCMC))
}
