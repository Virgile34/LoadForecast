setwd("C:/Users/virgi/Documents/UPSaclay/M1/S2/modelisation prédictive")

rm(list=objects())

library(tidyverse)
library(lubridate)
library(forecast)

data0 <- read_delim("DATA/train.csv", delim=",")
data1 <- read_delim("DATA/test.csv", delim=",")

summary(data0)
# rq Temp est la moyenne uniforme sur 39 stations météo
# -> moyenne non uniforme ?? (plus de gens a Paris qu'en Meurthe et Moselle)


# factorize wished data
for (var in c("WeekDays", "BH", "DLS")) {
  data0[ ,var] <- factor(data0[, var])
  data1[ ,var] <- factor(data1[, var])
}



range(data0$Date)
range(data1$Date)


names(data0)



##########################################################################
# a few plots
plot(data0$Date, data0$Load, type='l', xlim=range(data0$Date, data1$Date))

hist(data0$Load, breaks=100)

plot(data0$Date, data0$Temp, type='l')
##########################################################################









plot(data0$Load[which(data0$Month==6)], type='l')

boxplot(Load~WeekDays, data=data0)


par(mfrow=c(1,1))
Acf(data0$Load, log.max=7*3, type=c('correlation'))
Acf(data0$Load, log.max=7*3, type=c('partial'))


par(mfrow=c(1,2))
Acf(data0$Load, log.max=7*60, type=c('correlation'))
Acf(data0$Load, log.max=7*60, type=c('partial'))



############################
par(mar = c(5,5,2,5))
par(mfrow=c(1,1))
plot(data0$Date, data0$Load, type='l')
par(new=T)
plot(data0$Date, data0$Temp, type='l', col='red', axes=F, xlab='', ylab='')
axis(side = 4, col='red', col.axis='red')
mtext(side=4, line=3, 'Temperature', col='red')
legend('top', c('load', 'Temperature'), col=c('black', 'red'), lty=1, ncol=1, bty='n')


col.tr <- adjustcolor(col='black', alpha=.05)
plot(data0$Temp, data0$Load, pch=3, col=col.tr)


plot(data0$Date%>%head( ,n=7*3), data0$Temp%>%head( ,n=7*3), type='l')
lines(data0$Date%>%head( ,n=7*3), data0$Temp_s95%>%head( ,n=7*3), col='blue')
lines(data0$Date%>%head( ,n=7*3), data0$Temp_s99%>%head( ,n=7*3), col='red')

plot(data0$Date%>%head( ,n=7*5), data0$Temp_s99%>%head( ,n=7*5), type='l')
lines(data0$Date%>%head( ,n=7*5), data0$Temp_s99_min%>%head( ,n=7*5), col='blue')
lines(data0$Date%>%head( ,n=7*5), data0$Temp_s99_max%>%head( ,n=7*5), col='red')

par(mfrow=c(1,1))
col.tr1 <- adjustcolor(col='black', alpha=0.05)
col.tr2 <- adjustcolor(col='red', alpha=0.05)
plot(data0$Temp, data0$Load, pch=3, col=col.tr1)
plot(data0$Temp_s99, data0$Load, pch=3, col=col.tr2)



###############################################
plot(data0$Load.7, data0$Load, pch=3)
plot(data0$Load.1, data0$Load, pch=3)


cor(data0$Load.1, data0$Load)
cor(data0$Load.7, data0$Load)
#################################################
boxplot(Load~Christmas_break, data=data0[which(data0$DLS==1), ])
boxplot(Load~Summer_break, data=data0[which(data0$DLS==2), ])
boxplot(Load~BH, data=data0)


##########################
boxplot(Load~DLS, data=data0)
#########################################


par(mfrow=c(1,2))
hist(data0$Temp)
hist(data1$Temp)

range(data0$Temp)
range(data1$Temp)


par(mfrow=c(1,1))
hist(data0$Temp, xlim=range(data0$Temp, data1$Temp), col='lightblue', breaks=50)
par(new=T)
hist(data1$Temp, xlim=range(data0$Temp, data1$Temp), col=adjustcolor('red', alpha=0.5), breaks=50)









