setwd("~/University/M1/S2/modé_pred_new")

rm(list=objects())
library(tidyverse)
library(lubridate)
library(forecast)


Data0 <- read_delim("Data/train.csv", delim=",")
Data1<- read_delim("Data/test.csv", delim=",")

# summary(Data0)

range(Data0$Date) # "2012-01-01" "2020-04-15"
range(Data1$Date) # "2020-04-16" "2021-01-15"

names(Data0)


############################################trend
plot(Data0$Date, Data0$Load, type='l', xlim=range(Data0$Date, Data1$Date))

hist(Data0$Load, breaks=100)

plot(Data0$Date, Data0$Temp, type='l')
############################################yearly cycle
a <- 1
b <- a+365
plot(Data0$Date[a:b], Data0$Load[a:b], type='l')

plot(Data0$toy)

col.tr <- adjustcolor(col='black', alpha=0.2)
plot(Data0$toy, Data0$Load, pch=16,  col=col.tr)


############################################Weekly cycle
plot(Data0$Load[which(Data0$Month==6)], type='l')

boxplot(Load~WeekDays, data=Data0)

par(mfrow=c(1,1))
Acf(Data0$Load, lag.max=7*3, type = c("correlation"))
Acf(Data0$Load, lag.max=7*3, type = c("partial"))


par(mfrow=c(1,2))
Acf(Data0$Load, lag.max=7*60, type = c("correlation"))
Acf(Data0$Load, lag.max=7*60, type = c("partial"))

Acf(Data0$Load[which(Data0$Month==6)],  type = c("partial"))

Acf(Data0$Load[which(Data0$Month==2)],  type = c("partial"))


############################################Meteo effect/covariates
par(mar = c(5,5,2,5))
par(mfrow=c(1,1))
plot(Data0$Date, Data0$Load, type='l')
par(new=T)
plot(Data0$Date, Data0$Temp, type='l', col='red', axes=F,xlab='',ylab='')
plot(Data0$Temp%>%tail(1000), type='l', col='red', axes=F,xlab='',ylab='')
axis(side = 4,col='red', col.axis='red')
mtext(side = 4, line = 3, 'Temperature', col='red')
legend("top",c("Load","Temperature"),col=c("black","red"),lty=1,ncol=1,bty="n")


col.tr <- adjustcolor(col='black', alpha=0.25)
plot(Data0$Temp, Data0$Load, pch=3,  col=col.tr)


plot(Data0$Date%>%head(,n=7*3), Data0$Temp%>%head(,n=7*3), type='l')
lines(Data0$Date%>%head(,n=7*3), Data0$Temp_s95%>%head(,n=7*3), col='blue')
lines(Data0$Date%>%head(,n=7*3), Data0$Temp_s99%>%head(,n=7*3), col='red')

plot(Data0$Date%>%head(,n=7*5), Data0$Temp_s99%>%head(,n=7*5), type='l')
lines(Data0$Date%>%head(,n=7*5), Data0$Temp_s99_min%>%head(,n=7*5), col='blue')
lines(Data0$Date%>%head(,n=7*5), Data0$Temp_s99_max%>%head(,n=7*5), col='red')

par(mfrow=c(1,1))
col.tr1 <- adjustcolor(col='black', alpha=0.25)
col.tr2 <- adjustcolor(col='red', alpha=0.25)
plot(Data0$Temp, Data0$Load, pch=3,  col=col.tr1)
points(Data0$Temp_s99, Data0$Load, pch=3, col=col.tr2)



############################################Lag
names(Data0)

plot(Data0$Load.7, Data0$Load, pch=3)

plot(Data0$Load.1, Data0$Load, pch=3)

cor(Data0$Load.1, Data0$Load)
cor(Data0$Load.7, Data0$Load)


############################################Holidays
boxplot(Load~Christmas_break, data=Data0[which(Data0$DLS==1),])
boxplot(Load~Summer_break, data=Data0[which(Data0$DLS==2),])
boxplot(Load~BH, data=Data0)

############################################DLS
boxplot(Load~DLS, data=Data0)

#########################################train/Test

par(mfrow=c(1,2))
hist(Data0$Temp)
hist(Data1$Temp)

range(Data0$Temp)
range(Data1$Temp)

par(mfrow=c(1,1))
hist(Data0$Temp, xlim=range(Data0$Temp, Data1$Temp), col='lightblue', breaks=50)
par(new=T)
hist(Data1$Temp, xlim=range(Data0$Temp, Data1$Temp), col=adjustcolor('red', alpha.f=0.5), , breaks=50)



par(mfrow=c(1,2))
plot(tail(Data0$Date, 365), tail(Data0$GovernmentResponseIndex, 365), type='l', ylim=c(0,80))
plot(Data1$Date, Data1$GovernmentResponseIndex, type='l', ylim=c(0,80))

par(mfrow=c(1,1))
plot(Data0$GovernmentResponseIndex, Data0$Load)







