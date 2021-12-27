rm(list=ls())
library(lubridate)
library(vioplot)
library(plotrix)
library(readxl)
library(stats)

setwd("C:\\ChindwinHMS\\Draft\\Figures")
allflows=read_excel("Allflowssimulated.xlsx", "Monywa")
allflows=allflows[,-1]
allflows=allflows[,c(2,1,3,4)]
par(mar=c(2,2,2,0.1))
par(family = "serif")
vioplot(allflows, col=c("gray","cyan","orange","green"),main="Monywa")



allflows=read_excel("Allflowssimulated.xlsx", "Kalewa")
allflows$year=year(allflows$Date)
yearname=unique(allflows$year)
all_a=all_b=all_c=all_d=NULL
for (i in 1:length(yearname)){
  yearly=allflows[allflows$year==yearname[i],]
  a=quantile(yearly$Obs,c(0.25,0.5,0.95))
  b=quantile(yearly$Lumped,c(0.25,0.5,0.95))
  c=quantile(yearly$Semidistributed,c(0.25,0.5,0.95))
  d=quantile(yearly$Gridded,c(0.25,0.5,0.95))
  
  all_a=rbind(all_a,a)
  all_b=rbind(all_b,b)
  all_c=rbind(all_c,c)
  all_d=rbind(all_d,d)
}

Q10=cbind(all_a[,1],all_b[,1],all_c[,1],all_d[,1])
Q50=cbind(all_a[,2],all_b[,2],all_c[,2],all_d[,2])
Q90=cbind(all_a[,3],all_b[,3],all_c[,3],all_d[,3])
colnames(Q10)=colnames(Q50)=colnames(Q90)=c("Obs","Lumped","Semidistributed","Gridded")

vioplot(Q10, col=c("gray","cyan","orange","green"),main="HkamtiQ25")
vioplot(Q50, col=c("gray","cyan","orange","green"),main="HkamtiQ50")
vioplot(Q90, col=c("gray","cyan","orange","green"),main="HkamtiQ90")






unique(allflows$year)
quantile(allflows$Lumped, c(0.1,0.5,0.9))
summary(allflows$Lumped)
aggregate.data.frame(allflows$Obs,by=unique(allflows$year),FUN=quantile(allflows$Obs))

a=quantile(allflows$Obs,c(0.1,0.5,0.9))


allflows=read.xlsx("Allflowssimulated.xlsx", sheet="Hkamti")

HMSmodel=read.xlsx("Forecastedflows.xlsx",sheet="HMS")
GR4Jmodel=read.xlsx("Forecastedflows.xlsx",sheet="GR4J")
IHACRESmodel=read.xlsx("Forecastedflows.xlsx",sheet="IHACRES")
SWATmodel=read.xlsx("Forecastedflows.xlsx",sheet="SWAT")
OBSERVED=read.csv("ARB_QOBS.csv",header=T)

tiff('Violinforecasts.tiff', units="in", width=9, height=9, res=300,compression="lzw")
par(mfrow=c(3,3))
par(mar=c(2,2,2,0))
par(mgp = c(6, 0.5, 0))

#Myitkina station
OBS=OBSERVED$Myitkina
SWAT=SWATmodel$Myitkina/OBS-1
HMS=HMSmodel$Myitkina/OBS-1
IHACRES=IHACRESmodel$Myitkina/OBS-1
GR4J=GR4Jmodel$Myitkina/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Myitkina",labels=Model)
abline(h=0,col="blue",lty=2,lwd=2)


#Hkamti station
OBS=OBSERVED$Hkamti
SWAT=SWATmodel$Hkamti/OBS-1
HMS=HMSmodel$Hkamti/OBS-1
IHACRES=IHACRESmodel$Hkamti/OBS-1
GR4J=GR4Jmodel$Hkamti/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Hkamti")
abline(h=0,col="blue",lty=2,lwd=2)

#Sagaing station
OBS=OBSERVED$Sagaing
SWAT=SWATmodel$Sagaing/OBS-1
HMS=HMSmodel$Sagaing/OBS-1
IHACRES=IHACRESmodel$Sagaing/OBS-1
GR4J=GR4Jmodel$Sagaing/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Sagaing")
abline(h=0,col="blue",lty=2,lwd=2)

#Monywa station
OBS=OBSERVED$Monywa
SWAT=SWATmodel$Monywa/OBS-1
HMS=HMSmodel$Monywa/OBS-1
IHACRES=IHACRESmodel$Monywa/OBS-1
GR4J=GR4Jmodel$Monywa/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Monywa")
abline(h=0,col="blue",lty=2,lwd=2)

#Chauk station
OBS=OBSERVED$Chauk
SWAT=SWATmodel$Chauk/OBS-1
HMS=HMSmodel$Chauk/OBS-1
IHACRES=IHACRESmodel$Chauk/OBS-1
GR4J=GR4Jmodel$Chauk/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Chauk")
abline(h=0,col="blue",lty=2,lwd=2)

#Pyay station
OBS=OBSERVED$Pyay
SWAT=SWATmodel$Pyay/OBS-1
HMS=HMSmodel$Pyay/OBS-1
IHACRES=IHACRESmodel$Pyay/OBS-1
GR4J=GR4Jmodel$Pyay/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Pyay")
abline(h=0,col="blue",lty=2,lwd=2)

#Magway station
OBS=OBSERVED$Magway
SWAT=SWATmodel$Magway/OBS-1
HMS=HMSmodel$Magway/OBS-1
IHACRES=IHACRESmodel$Magway/OBS-1
GR4J=GR4Jmodel$Magway/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Magway")
abline(h=0,col="blue",lty=2,lwd=2)

#Seiktha station
OBS=OBSERVED$Seiktha
SWAT=SWATmodel$Seiktha/OBS-1
HMS=HMSmodel$Seiktha/OBS-1
IHACRES=IHACRESmodel$Seiktha/OBS-1
GR4J=GR4Jmodel$Seiktha/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Seiktha")
abline(h=0,col="blue",lty=2,lwd=2)

#Zalun station
OBS=OBSERVED$Zalun
SWAT=SWATmodel$Zalun/OBS-1
HMS=HMSmodel$Zalun/OBS-1
IHACRES=IHACRESmodel$Zalun/OBS-1
GR4J=GR4Jmodel$Zalun/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Zalun")
abline(h=0,col="blue",lty=2,lwd=2)

dev.off()


#Katha station
OBS=OBSERVED$Katha
SWAT=SWATmodel$Katha/OBS-1
HMS=HMSmodel$Katha/OBS-1
IHACRES=IHACRESmodel$Katha/OBS-1
GR4J=GR4Jmodel$Katha/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Katha")
abline(h=0,col="blue",lty=2,lwd=2)

#Homalin station
OBS=OBSERVED$Homalin
SWAT=SWATmodel$Homalin/OBS-1
HMS=HMSmodel$Homalin/OBS-1
IHACRES=IHACRESmodel$Homalin/OBS-1
GR4J=GR4Jmodel$Homalin/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Homalin")
abline(h=0,col="blue",lty=2,lwd=2)

#Kalewa station
OBS=OBSERVED$Kalewa
SWAT=SWATmodel$Kalewa/OBS-1
HMS=HMSmodel$Kalewa/OBS-1
IHACRES=IHACRESmodel$Kalewa/OBS-1
GR4J=GR4Jmodel$Kalewa/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Kalewa")
abline(h=0,col="blue",lty=2,lwd=2)

#Nyaungu station
OBS=OBSERVED$Nyaungu
SWAT=SWATmodel$Nyaungu/OBS-1
HMS=HMSmodel$Nyaungu/OBS-1
IHACRES=IHACRESmodel$Nyaungu/OBS-1
GR4J=GR4Jmodel$Nyaungu/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Nyaungu")
abline(h=0,col="blue",lty=2,lwd=2)

#Chauk station
OBS=OBSERVED$Chauk
SWAT=SWATmodel$Chauk/OBS-1
HMS=HMSmodel$Chauk/OBS-1
IHACRES=IHACRESmodel$Chauk/OBS-1
GR4J=GR4Jmodel$Chauk/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Chauk")
abline(h=0,col="blue",lty=2,lwd=2)

#Aunglan station
OBS=OBSERVED$Aunglan
SWAT=SWATmodel$Aunglan/OBS-1
HMS=HMSmodel$Aunglan/OBS-1
IHACRES=IHACRESmodel$Aunglan/OBS-1
GR4J=GR4Jmodel$Aunglan/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Aunglan")
abline(h=0,col="blue",lty=2,lwd=2)

#Seiktha station
OBS=OBSERVED$Seiktha
SWAT=SWATmodel$Seiktha/OBS-1
HMS=HMSmodel$Seiktha/OBS-1
IHACRES=IHACRESmodel$Seiktha/OBS-1
GR4J=GR4Jmodel$Seiktha/OBS-1
Model=data.frame(SWAT,HMS,IHACRES,GR4J)
violin_plot(Model,col="gray",main="Seiktha")
abline(h=0,col="blue",lty=2,lwd=2)

