## Set of functions to calculate COAT climate state variables
## Ole Einar, 2025-

# Dependencies
# lubridate,

# Loading required libraries
library(lubridate)

#=================================================================
# climate.data # Load a (single point) test data set, make test date vector 
#=================================================================
#load("climate.data.rda")
#dato<-as.Date(paste(climate.data$Year,climate.data$Month,climate.data$Day,sep="-"),"%Y-%m-%d")
#=================================================================

#=================================================================
# fmd: Freeze-melt days
#=================================================================
# Input:
# - tn,tx= annual series of daily minimum (tn) and  daily maximum (tx) temperature series
fmd<-function(tn,tx){length(tx[tx>0&tn<0])}
#_________________________________________________________________



#=================================================================
# fmds: Freeze-melt days on snow
#=================================================================
# Input:
# - tn,tx=annual series of daily minimum (tn) or daily maximum (tx) temperature series#
# - sd= annual series of daily snow depth (sd)
fmds<-function(tn,tx,sd){length(tx[tx>0&tn<0&sd>0])}
#_________________________________________________________________



#=================================================================
# md: Melting days (tg>0) 
#=================================================================
# Input: 
# - tg = annual series of mean daily temperature
md<-function(tg){length(tg[tg>0])}
#_________________________________________________________________



#=================================================================
# md.snow: Melting days (tg>0) when snow 
#=================================================================
# Input: 
# - tg = annual series of mean daily temperature
# - swe = snow depth (as water equivalent)
md.snow<-function(tg,swe){length(tg[tg>0&swe>0])}
#_________________________________________________________________



#=================================================================
# md.winter: Melting days (tg>0) in winter (nov-apr) 
#=================================================================
# Input: 
# - tg = annual series of mean daily temperature
# - swe = snow depth (as water equivalent)
md.winter<-function(tg,dato){
  indx<-c(1:length(dato))[month(dato)==11|month(dato)==12|month(dato)==1|month(dato)==2|month(dato)==3|month(dato)==4]
#  print(cbind(indx,tg[indx]))
  length(tg[indx][tg[indx]>0])
  }
#_________________________________________________________________



#=================================================================
# efd: Extreme frost days (tg < -30) 
#=================================================================
# Input: 
# - tg = annual series of mean daily temperature
efd<-function(tg){length(tg[tg< -30])}
#_________________________________________________________________



#=================================================================
# fd: Extreme frost days (tg < 0) 
#=================================================================
# Input: 
# - tg = annual series of mean daily temperature
fd<-function(tg){length(tg[tg<0])}
#_________________________________________________________________


#=================================================================
# gs: Growing days over a certain threshold
#=================================================================
# Input:
# - tg = annual daily mean temperature series
# - thres = temperaure threshold indicating lowest growing temperature (in degC)
gs<-function(tg,thres){length(tg[tg>thres])}
#_________________________________________________________________



#=================================================================
# gdd: Growing degree days over a certain threshold
#=================================================================
# Input:
# - tg = annual daily mean temperature series
# - thres = temperaure threshold indicating lowest growing temperature (in degC)
gdd<-function(tg,thres){sum(tg[tg>thres])}
#_________________________________________________________________



#=================================================================
# tm.month: Mean monthly temperature
#=================================================================
# Input:
# - tg=annual daily temperature series
# - imnd=month
# - date-vector
tm.month<-function(tg,dato,imnd){round(mean(tg[month(dato)==imnd],na.rm=T),digits=1)}
#_________________________________________________________________



#=================================================================
# tm.ses: Mean seasonal temperature
#=================================================================
# Input:
# - tg=annual daily temperature series
# - ses=season, character (djf,mam,jja,son,ann)
# - date-vector
tm.ses<-function(tg,dato,ses){
  if (ses=="djf"){indx<-c(1:length(dato))[month(dato)==12|month(dato)==1|month(dato)==2]}
  if (ses=="mam"){indx<-c(1:length(dato))[month(dato)==3|month(dato)==4|month(dato)==5]}
  if (ses=="jja"){indx<-c(1:length(dato))[month(dato)==6|month(dato)==7|month(dato)==8]}
  if (ses=="son"){indx<-c(1:length(dato))[month(dato)==9|month(dato)==10|month(dato)==11]}
  if (ses=="ann"){indx<-c(1:length(dato))}
#  print(indx)
  tm.ses<-round(mean(tg[indx],na.rm=T),digits=1)
}
#_________________________________________________________________




#=================================================================
# rr.month: Monthly precipitation sum
#=================================================================
# Input:
# - rr=annual daily temperature series
# - imnd=month
# - date-vector

rr.month<-function(rr,dato,imnd){round(sum(tg[month(dato)==imnd],na.rm=T),digits=1)}
#_________________________________________________________________



#=================================================================
# rr.ses: Seasonal precipitiation sum 
#=================================================================
# Input:
# - rr=annual daily temperature series
# - ses=season, character (djf,mam,jja,son,ann)
# - date-vector
rr.ses<-function(rr,dato,ses){
  ses<-tolower(ses)
  if (ses=="djf"){indx<-c(1:length(dato))[month(dato)==12|month(dato)==1|month(dato)==2]}
  if (ses=="mam"){indx<-c(1:length(dato))[month(dato)==3|month(dato)==4|month(dato)==5]}
  if (ses=="jja"){indx<-c(1:length(dato))[month(dato)==6|month(dato)==7|month(dato)==8]}
  if (ses=="son"){indx<-c(1:length(dato))[month(dato)==9|month(dato)==10|month(dato)==11]}
  if (ses=="ann"){indx<-c(1:length(dato))}
#  print(indx)
#  print(length(indx))
  rr.ses<-round(sum(rr[indx],na.rm=T),digits=1)
}
#_________________________________________________________________

#=================================================================
# ros: Rain on snow events
#=================================================================
# Input:
# - rr=annual daily precipitation series
# - tg=annual daily temperature series
# - swe=annual snow depth (as snow water equivalent) time serie
ros<-function(rr,tg,swe){length(swe[rr>0&tg>0&swe>0])}
#_________________________________________________________________



#=================================================================
# treeline: Paulsen&Körner tree growth indicator. 
#=================================================================
# Input:
# - tg=annual daily temperature series

# Return: 
# - pk.gs = Growing days above 0.9 degC
# - pk.tam = mean temperature in pk.gs
# - pk.treegrowth = binary indictor whether tree growing conditions are
#                   fulfilled (1) or not (0). 
treeline<-function(tg){
  cnt<-length(tg[tg>0.9])
  tm<-round(mean(tg[tg>0.9]),digits=1)
  tree<-ifelse(tm>6.4&cnt>94,1,0)
  treel<-c(cnt,tm,tree)
  names(treel)<-c("pk.gs","pk.tam","pk.treegrowth")
  treeline<-treel
  }
#_________________________________________________________________


# ****************************************************************
