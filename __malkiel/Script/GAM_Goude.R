rm(list=objects())
###############packages
library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(tidyverse)
source('R/score.R')

#Data <- readRDS("Data/Data_elec.RDS")

Data0 <- read_delim("Data/train.csv", delim=",")
Data1<- read_delim("Data/test.csv", delim=",")
Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

###Exclude the Covid period
# d1 <- which(as.character(Data0$Date)=="2020-03-15")
# Data0 <- Data0[1:d1,]

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

Data1 <- Data0[sel_b, ]
range(Data1$Date)
Data0 <- Data0[sel_a, ]
range(Data0$Date)


par(mfrow=c(1,1))
g0 <- gam(Load~s(Temp, k=3, bs="cr"), data=Data0)
plot(g0, residuals=T)

summary(g0)
plot(Data0$Temp, g0$residuals, pch=16)

g_prov <- gam(g0$residuals~ s(Data0$Temp, k=5, bs="cr"))
summary(g_prov)

g1 <- gam(Load~s(Temp, k=10, bs="cr"), data=Data0)
summary(g1)
plot(Data0$Temp, g0$residuals, pch=16, col='grey')
points(Data0$Temp, g1$residuals, pch=16)

(g0$gcv.ubre-g1$gcv.ubre)/g0$gcv.ubre
sqrt(g0$gcv.ubre)
sqrt(g1$gcv.ubre)


plot(g1)



Nblock<-10
borne_block<-seq(1, nrow(Data0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))






############################################################################
##############GAM model considering all predictors
############################################################################
gam1<-gam(Load~s(as.numeric(Date), k=3,  bs='cr')+s(toy,k=30, bs='cr')+s(Temp,k=10, bs='cr'), data=Data0)
summary(gam1)  
plot(gam1)


blockRMSE<-function(equation, block)
{
  g<- gam(as.formula(equation), data=Data0[-block,])
  forecast<-predict(g, newdata=Data0[block,])
  return(forecast)
} 


#####model 1
equation <- Load~s(as.numeric(Date),k=3, bs='cr')+s(toy,k=30, bs='cr')+s(Temp,k=10, bs='cr')
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse1<-rmse(Data0$Load, Block_forecast)
rmse1
gam1<-gam(equation, data=Data0)

boxplot(Block_residuals)
plot(Block_residuals, type='l')
hist(Block_residuals)

boxplot(Block_residuals~Data0$WeekDays)
plot(Data0$Temp, Block_residuals, pch=16)
plot(Data0$toy, Block_residuals, pch=16)
plot(Data0$Load.1, Block_residuals, pch=16)

gam1.forecast<-predict(gam1,  newdata= Data1)
rmse1.forecast <- rmse(Data1$Load,gam1.forecast)


#####model 2
equation <- Load~s(as.numeric(Date),k=3, bs='cr')+s(toy,k=30, bs='cc')+s(Temp,k=10, bs='cr')+WeekDays
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast

hist(Block_residuals, breaks=20)
rmse2 <- rmse(Data0$Load, Block_forecast)
rmse2
gam2<-gam(equation, data=Data0)

boxplot(Block_residuals~Data0$WeekDays)

plot(Data0$Temp, Block_residuals, pch=16)
plot(Data0$Load.1, Block_residuals, pch=16)

gam2.forecast<-predict(gam2,  newdata= Data1)
rmse2.forecast <- rmse(Data1$Load,gam2.forecast)


#####model 3
equation <- Load~s(as.numeric(Date),k=3, bs='cr')+s(toy,k=30, bs='cc')+s(Temp,k=10, bs='cr')+
  s(Load.1, bs='cr')+WeekDays
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse3 <- rmse(Data0$Load, Block_forecast)
rmse3
gam3<-gam(equation, data=Data0)

hist(Block_residuals, breaks=20)
plot(Block_residuals,  type='l')
boxplot(Block_residuals~Data0$WeekDays)
acf(Block_residuals)
plot(Data0$Load.7, Block_residuals, pch=16)
cor(Data0$Load.7,Block_residuals)

gam3.forecast<-predict(gam3,  newdata= Data1)
rmse3.forecast <- rmse(Data1$Load,gam3.forecast)

#####model 4
equation <- Load~s(as.numeric(Date),k=3,  bs='cr')+s(toy,k=30, bs='cc')+s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') + WeekDays
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse4 <- rmse(Data0$Load, Block_forecast)
rmse4
gam4<-gam(equation, data=Data0)
summary(gam4)


plot(Data0$Date, Block_residuals, pch=16)

boxplot(Block_residuals~Data0$BH)

boxplot(Block_residuals~Data0$Christmas_break)
boxplot(Block_residuals~Data0$Summer_break)

gam4.forecast<-predict(gam4,  newdata= Data1)
rmse4.forecast <- rmse(Data1$Load,gam4.forecast)


#####model 5
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') + WeekDays +BH
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse5 <- rmse(Data0$Load, Block_forecast)
rmse5

gam5<-gam(equation, data=Data0)
summary(gam5)
plot(Data0$Date, Block_residuals, pch=16)

plot(Data0$Temp_s95, Block_residuals, pch=16)
test <- gam(Block_residuals~s(Data0$Temp_s95))
summary(test)

plot(Data0$Temp_s99, Block_residuals, pch=16)
test <- gam(Block_residuals~s(Data0$Temp_s99))
summary(test)

sqrt(gam5$gcv.ubre)

gam5.forecast<-predict(gam5,  newdata= Data1)
rmse5.forecast <- rmse(Data1$Load,gam5.forecast)

#####model 6
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +s(Temp_s99,k=10, bs='cr') + WeekDays +BH
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse6 <- rmse(Data0$Load, Block_forecast)
rmse6

gam6<-gam(equation, data=Data0)
sqrt(gam6$gcv.ubre)
summary(gam6)

plot(Data0$Date, Block_residuals, pch=16)
acf(Block_residuals)
plot(Data0$Temp, Block_residuals, pch=16)

plot(Data0$Temp_s95_max, Block_residuals, pch=16)

test <- gam(Block_residuals~s(Data0$Temp_s95_max))
summary(test)
test <- gam(Block_residuals~s(Data0$Temp_s99_max))
summary(test)

test <- gam(Block_residuals~te(Data0$Temp_s95_max, Data0$Temp_s99_max))
summary(test)

gam6.forecast<-predict(gam6,  newdata= Data1)
rmse6.forecast <- rmse(Data1$Load,gam6.forecast)

#####model 7
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) 
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse7 <- rmse(Data0$Load, Block_forecast)
rmse7


gam7<-gam(equation, data=Data0)
sqrt(gam7$gcv.ubre)
summary(gam7)

gam7.forecast<-predict(gam7,  newdata= Data1)
rmse7.forecast <- rmse(Data1$Load,gam7.forecast)


#####model 8
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse8 <- rmse(Data0$Load, Block_forecast)
rmse8

gam8<-gam(equation, data=Data0)
sqrt(gam8$gcv.ubre)
summary(gam8)

boxplot(Block_residuals ~ Data0$DLS)

plot(Data0$GovernmentResponseIndex,  Block_residuals)

test <- gam(Block_residuals~te(Data0$Temp_s95_min, Data0$Temp_s99_min))
summary(test)

gam8.forecast<-predict(gam8,  newdata= Data1)
rmse8.forecast <- rmse(Data1$Load,gam8.forecast)


#####model 9
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse9 <- rmse(Data0$Load, Block_forecast)
rmse9

gam9<-gam(equation, data=Data0)
sqrt(gam9$gcv.ubre)

summary(gam9)

par(mfrow=c(1,2))
acf(Block_residuals)
pacf(Block_residuals)

par(mfrow=c(1,1))
hist(Block_residuals, breaks=50)


gam9.forecast<-predict(gam9,  newdata= Data1)
rmse9.forecast <- rmse(Data1$Load,gam9.forecast)

plot(gam9, scheme=c(2,1))


par(mfrow=c(2,2))
gam.check(gam9)


###another way to obtain residual plots
linear_terms <- predict(gam9, type='terms')[,1:4]
res <- residuals(gam9)

plot(gam9$fitted.values, res)

####################################################################################################
######################### residual correction
####################################################################################################
Block_residuals.ts <- ts(Block_residuals, frequency=7)

fit.arima.res <- auto.arima(Block_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2, trace=T,ic="aic", method="CSS")
#Best model: ARIMA(3,0,4)(1,0,0)[7] with zero mean   
#saveRDS(fit.arima.res, "Results/tif.arima.res.RDS")

ts_res_forecast <- ts(c(Block_residuals.ts, Data1$Load-gam9.forecast),  frequency= 7)
refit <- Arima(ts_res_forecast, model=fit.arima.res)
prevARIMA.res <- tail(refit$fitted, nrow(Data1))

rmse9.arima <- rmse(Data0$Load, gam9$fitted.values+fit.arima.res$fitted)

gam9.arima.forecast <- gam9.forecast + prevARIMA.res



rmse(Data1$Load,gam9.forecast)
rmse(Data1$Load,gam9.arima.forecast)
#mape(Data1$Load, gam9.arima.forecast)
rmse9.arima.forecast <- rmse(Data1$Load,gam9.arima.forecast)



################################################################################
##########synthèse
################################################################################

rmseCV <- c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6, rmse7, rmse8, rmse9, rmse9.arima)
rmse.forecast<- c(rmse1.forecast, rmse2.forecast, rmse3.forecast, rmse4.forecast, rmse5.forecast, rmse6.forecast, 
                  rmse7.forecast, rmse8.forecast, rmse9.forecast, rmse9.arima.forecast)

rgcv <- c(gam1$gcv.ubre, gam2$gcv.ubre, gam3$gcv.ubre, gam4$gcv.ubre, gam5$gcv.ubre, gam6$gcv.ubre, gam7$gcv.ubre,
          gam8$gcv.ubre, gam9$gcv.ubre)%>%sqrt

par(mfrow=c(1,1))
plot(rmseCV, type='b', pch=20, ylim=range(rmseCV,rgcv, rmse.forecast))
lines(rmse.forecast, type='b', pch=20, col='blue')
lines(rgcv, col='red', type='b', pch=20)
points(10, rmse9.arima.forecast)
points(10, rmse9.arima)
legend("topright", col=c("red","black","blue"), c("gcv","blockCV","test"), pch=20, ncol=1, bty='n', lty=1)



################################################################################
##########submission
################################################################################

Data0 <- read_delim("Data/train.csv", delim=",")
Data1<- read_delim("Data/test.csv", delim=",")

#####model 9
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)
gam9<-gam(equation, data=Data0)
gam9.forecast<-predict(gam9,  newdata= Data1)

submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- gam9.forecast
write.table(submit, file="Data/submission_gam.csv", quote=F, sep=",", dec='.',row.names = F)







##################################################
##########variance modeling
##################################################

#####model 9
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)
equation2 <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=60, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays2 +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)

Block_forecast<-lapply(block_list, blockRMSE, equation=equation2)%>%unlist
Block_residuals <- Data0$Load-Block_forecast


plot(Block_residuals, type='l')

plot(Data0$Temp, Block_residuals%>%abs)


equation <- Load~s(as.numeric(Date),k=4, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)
gam10<-gam(list(equation2, ~s(Temp)), data=Data0, family="gaulss")

summary(gam10)
summary(gam9)
        
        gam10.forecast<-predict(gam10,  newdata= Data1)[,1]
        predict<-1/2*gam10.forecast+gam9.forecast*1/2
        plot(Data1$Load.1, type='l')
        lines(predict)
        rmse9.forecast
        rmse10.forecast <- rmse(Data1$Load,gam10.forecast)
    
        ####################
        
        