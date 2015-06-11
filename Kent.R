## playing around with data ##
library(foreign)
library(zoo)
library(plm)
library(Matrix)

setwd("~/Documents/Kent/Macro")
mac = read.dta('quarterly.dta')

# Functions ---------------------------------------------------------------

L <- function(x, k) 
{
  c(rep(NA, k), x)[1 : length(x)] 
}

deseasonalizeQ <- function (x)
{
  x <- ts(x)
  #Step1: Centered moving averages: create cma time series having the same length with the original time series x
  # cma has 2 NAs on both ends.
  cma <- filter(x, filter = c(1/8, 1/4, 1/4, 1/4, 1/8), sides=2)
  
  #Step2: Ratios = Original time series / centered moving averages
  ratio <- x/cma
  
  #Step3: Unadjusted 4 seasonal indexes
  unadj4si <- ts(1:4)
  # floor((length(x)-4)/4)  #"-4" is 4 NA at both ends; below "-1" is due to starting "0:" in multiplication
  
  unadj4si[1] <- mean(ratio[3+4*(0:(floor((length(x)-4)/4) - 1))])
  unadj4si[2] <- mean(ratio[4+4*(0:(floor((length(x)-4)/4) - 1))])
  unadj4si[3] <- mean(ratio[5+4*(0:(floor((length(x)-4)/4) - 1))])
  unadj4si[4] <- mean(ratio[6+4*(0:(floor((length(x)-4)/4) - 1))])
  
  #Step4: Adjusted 4 seasonal indexes
  adj4si <- ts(1:4)
  adj4si[1] <- unadj4si[1]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
  adj4si[2] <- unadj4si[2]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
  adj4si[3] <- unadj4si[3]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
  adj4si[4] <- unadj4si[4]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
  
  #Step5: Propogated adjusted seasonal indexes
  propadjsi <- ts(1:length(x))
  
  propadjsi[3+4*(0:(floor((length(x)-4)/4) - 1))] <- adj4si[1]
  propadjsi[4+4*(0:(floor((length(x)-4)/4) - 1))] <- adj4si[2]
  propadjsi[5+4*(0:(floor((length(x)-4)/4) - 1))] <- adj4si[3]
  propadjsi[6+4*(0:(floor((length(x)-4)/4) - 1))] <- adj4si[4]
  
  propadjsi[1] <- adj4si[3]
  propadjsi[2] <- adj4si[4]
  propadjsi[length(x)-1] <- adj4si[1]
  propadjsi[length(x)] <- adj4si[2]
  
  #Step6: Deseasonalized values
  out <- x/propadjsi  # deseasonalized = x/propadjsi
  return(out)
}

deseasonalizeDavidQ = function(x)
{
  Time = 1:length(x)
  N = rep(1:4,length(x)/4)
  
  Q1 = (N==1)
  Q2 = (N==2)
  Q3 = (N==3)
  
  fit = lm(x~Time+Q1+Q2+Q3)
  
  alpha = fit$coefficients[1]
  beta = fit$coefficients[2]
  deseason = alpha + beta*Time + fit$residuals
  return(deseason)
}

# Data --------------------------------------------------------------------

N = dim(mac)[1]

# unique values
ucountj = unique(mac$country_j)
ucounti = unique(mac$country_i)
udate = unique(mac$date)

# order by country j
newmac = mac[order(mac$country_j),]

# adding some variables
newmac$exp_share = (newmac$exp / newmac$exp_wld)*100
newmac$fx = newmac$fx_i / newmac$fx_j
newmac$comp = newmac$reer_i / newmac$reer_j

# making change in export share, fx, comp, gdp variables variable (quarterly)
newmac$dexp_share = rep(0,N)
newmac$dfx = rep(0,N)
newmac$dcomp = rep(0,N)
newmac$dgdp = rep(0,N)

for(i in 1:length(ucounti))
{
  for(j in 1:length(ucountj))
  {
    ind = intersect(which(newmac$country_j==ucountj[j]),which(newmac$country_i==ucounti[i]))
    
    # export share change for all i and j
    exp_share = newmac$exp_share[ind]
    newmac$dexp_share[ind] = exp_share - L(exp_share,1)
    
    # fx change for all i and j
    fx = newmac$fx[ind]
    newmac$dfx[ind] = (1 - (fx/L(fx,1)))*100
    
    # comp change for all i and j
    comp = newmac$comp[ind]
    newmac$dcomp[ind] = comp - L(comp,1)
    
    # gdp change for all i and j
    gdp = newmac$comp[ind]
    newmac$dgdp[ind] = (gdp/L(gdp,1) - 1)*100
  }
}

# Building design matrix --------------------------------------------------

# construct X
numcovar = 4
size = numcovar*length(ucountj)
loopind = seq(from = 1,to = size,by = numcovar)
BIGX = Matrix(matrix(0,N,size),sparse=T) #using sparse matrix class
storenum=1

for(i in loopind)
{
  TF = ustore[storenum]==store
  ind = which(TF==TRUE)
  BIGX[ind,i] = const[ind]
  BIGX[ind,i+1] = log(price[ind])
  BIGX[ind,i+2] = disp[ind]
  BIGX[ind,i+3] = dispIlogprice[ind]
  storenum = storenum+1
}
