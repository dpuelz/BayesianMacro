## playing around with data ##
library(foreign)
library(zoo)
library(plm)

setwd("~/Documents/Kent/Macro")
mac = read.dta('quarterly.dta')

# Functions ---------------------------------------------------------------

L <- function(x, k) 
{
  c(rep(NA, k), x)[1 : length(x)] 
}

deseasonalizeQ <- function (x){
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
  out
}

# Data --------------------------------------------------------------------

N = dim(mac)[1]

# unique values
ucountj = unique(mac$country_j)
ucounti = unique(mac$country_i)
udate = unique(mac$date)

newmac = mac[order(mac$country_j),]

 = (newmac$exp / newmac$exp_wld)*100

for(i in 1:N)
{
  
}


# Building design matrix --------------------------------------------------

