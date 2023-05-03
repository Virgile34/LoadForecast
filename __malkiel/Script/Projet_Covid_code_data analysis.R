rm(list=objects())

install.packages("tidyverse")
install.packages("lubridate")
install.packages("forecast")
library(tidyverse)#analyse de données
library(lubridate)#gérer les formats date
library(forecast)
setwd("~/Project_data_challenge")
Data0<- read_delim("train.csv", delim=',')
Data1<- read_delim("test.csv", delim=',')

#L'unité est en MégaW (nucléaire: 1000 MW, Consommation Max: 10e5 MW , TGV: 20 MW)
#Piste avec Température Météo France (Moyenne des températures spatiales)
#Température lissée s95 (TL_t=alphaT_t+(1-alpha)TL_t-1)
#Optimiser ce coefficient alpha (soit s_alpha) peut être intéressant
#Temp_s95_min et Temp_s95_Max le min et le max de l'amplitude de la temp lissée
# Une baisse de 1 degré vaut 2500 MW en plus
#toy: time of year on pondère 
#

summary(Data0)

range(Data0$Date)
range(Data1$Date)

names(Data0)
####################################################################trend
plot(Data0$Date, Data0$Load, type='l', xlim=range(Data0$Date,Data1$Date))
hist(Data0$Load, breaks=100)
plot(Data0$Date, Data0$Temp, type='l')
Data0$BH=factor(Data0$BH)
Data0$Summer_break=factor(Data0$Summer_break)
Data0$Christmas_break=factor(Data0$Christmas_break)

####################################################################yearly cycle
a<-1
b<-a+365
plot(Data0$Date[a:b],Data0$Load[a:b],type='l')
plot(Data0$toy)
col.tr<-adjustcolor(col='black', alpha=0.2)
plot(Data0$toy,Data0$Load,pch=16, col=col.tr)
plot(Data0$Load[which(Data0$Month==6)],type='l')
boxplot(Load~WeekDays, data=Data0)
par(mfrow=c(1,2))
Acf(Data0$Load,lag.max=7*60, type=c("correlation"))#Autocorrelation 
Acf(Data0$Load,lag.max=7*60, type=c("partial"))#on enlève les correlations conditionnelles
Acf(Data0$Load[which(Data0$Month==2)], type=c("partial"))




###########################################################Meteo effect/covariates
par(mar=c(5,5,2,5))
par(mfrow=c(1,1))
plot(Data0$Date,Data0$Load,type='l')
par(new=T)

plot(Data0$Date, Data0$Temp, type='l', col='red', axes=F, xlab='',ylab='')
axis(side=4, col='red',col.axis='red')
mtext(side=4,line=3, 'Temperature',col='red')
legend('top',c("load","Temperature"),col=c("black","red"),lty=1,ncol=1,bty="n")
col.tr<- adjustcolor(col='black', alpha=0.25)#phénomène bivarié
plot(Data0$Temp,Data0$Load,pch=3,col=col.tr)

plot(Data0$Date%>%head(,n=7*3),Data0$Temp%>%head(,n=7*3),type='l')
lines(Data0$Date%>%head(,n=7*3),Data0$Temp_s95%>%head(,n=7*3),col='blue')
lines(Data0$Date%>%head(,n=7*3),Data0$Temp_s99%>%head(,n=7*3),col='red')#la syntaxe %>% est un pipe par tidyverse (on applique la fonction en se passant des parenthèses)

plot(Data0$Date%>%head(,n=7*5),Data0$Temp_s99%>%head(,n=7*5),type='l')
lines(Data0$Date%>%head(,n=7*5),Data0$Temp_s99_min%>%head(,n=7*5),col='blue')
lines(Data0$Date%>%head(,n=7*5),Data0$Temp_s99_max%>%head(,n=7*5),col='red')
par(mfrow=c(1,1))
col.tr1<-adjustcolor(col='black',alpha=0.25)
col.tr2<-adjustcolor(col='red',alpha=0.25)
plot(Data0$Temp,Data0$Load,pch=3,col=col.tr1)
points(Data0$Temp_s99,Data0$Load,pch=3,col=col.tr2)
################################"""
names(Data0)
plot(Data0$Load.7,Data0$Load,pch=3)
plot(Data0$Load.1,Data0$Load,pch=3)#on observe des clusters (transition semaine-we)


cor(Data0$Load.7,Data0$Load)
cor(Data0$Load.1,Data0$Load)
#########################"
boxplot(Load~Christmas_break,data=Data0[which(Data0$DLS==1),])
boxplot(Load~Summer_break,data=Data0)
boxplot(Load~BH,data=Data0)

#######################################
boxplot(Load~DLS, data=Data0)
###################################train/test

par(mfrow=c(1,2))
hist(Data0$Temp)
hist(Data1$Temp)
#pas de pbs d'extrapollation

range(Data0$Temp)
range(Data1$Temp)
par(mfrow=c(1,1))

hist(Data0$Temp,xlim=range(Data0$Temp,Data1$Temp),col='lightblue',breaks=50)

par(new=T)
hist(Data1$Temp,xlim=range(Data0$Temp,Data1$Temp), col=adjustcolor('red',alpha.f=0.5),,breaks=50)


par(mfrow=c(1,2))
plot(tail(Data0$Date,365),tail(Data0$GovernmentResponseIndex,365), type='l',ylim=c(0,80))
plot(Data1$Date, Data1$GovernmentResponseIndex,type='l', ylim=c(0,80))

par(mfrow=c(1,1))
plot(Data0$GovernmentResponseIndex,Data0$Load)
#temps d'ensolleillement par jour
#Synop (météofrance) (vent,neige, humidité)
#Prix
#
#


