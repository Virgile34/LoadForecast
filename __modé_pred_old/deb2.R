setwd("C:/Users/virgi/Documents/UPSaclay/M1/S2/modelisation prédictive/Project")

rm(list=objects())

library(tidyverse)
library(lubridate)
library(forecast)

data0 <- read_delim("Data_raw/train.csv", delim=",")
data1 <- read_delim("Data_raw/test.csv", delim=",")

data0$Time <- as.numeric(data0$Date)
data1$Time <- as.numeric(data1$Date)

sel_a <- which(data0$Year <= 2019)
sel_b <- which(data0$Year >  2019)


source('R/score.R')

################################################################################
############################# Feature Engineering ############################# 
################################################################################

################################# Cycle Hebdo ################################# 

mod0 <- lm(Load ~ WeekDays, data=data0[sel_a, ])
summary(mod0)
mod0.forecast <- predict(mod0, newdata=data0[sel_b, ])
rmse(y=data0$Load[sel_b], ychap=mod0.forecast)



#### bloc CV
Nblock <- 8
borne_block <- seq(1, nrow(data0), length=Nblock+1)%>%floor # Rq something %>% func <=> func(something)
block_list <- list()
l <- length(borne_block)
for (i in c(2:(l-1))){
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]] <- c(borne_block[l-1]:borne_block[l])


fitmod <- function(eq, block){
  mod <- lm(eq, data=data0[-block, ])
  mod.cvpred <- predict(mod, newdata=data0[block, ])
  return(mod.cvpred)
}


mod0.cvpred <- lapply(block_list, fitmod, eq="Load ~ WeekDays")%>%unlist
rmse(y=data0$Load, ychap=mod0.cvpred, digits=2)


#### Regroupement de modalités
data0$WeekDays2 <- forcats::fct_recode(data0$WeekDays, 'WorkDay'='Thursday', 'WorkDay'='Tuesday', 'WorkDay'='Wednesday')
mod0 <- lm(Load ~ WeekDays2, data=data0[sel_a, ])
summary(mod0)
mod0.forecast <- predict(mod0, newdata=data0[sel_b, ])
rmse(y=data0$Load[sel_b], ychap=mod0.forecast)

mod0.cvpred = lapply(block_list, fitmod, eq="Load ~ WeekDays2")%>%unlist
rmse(y=data0$Load, ychap=mod0.cvpred, digits=2)



################################################################################
############################# Temperature ############################# 
################################################################################


#### Polynomial Transforms
mod1 <- lm(Load ~WeekDays2 + Temp, data=data0[sel_a, ])
summary(mod1)
mod1.forcast <- predict(mod1, newdata=data0[sel_b, ])
rmse(y=data0$Load[sel_b], ychap=mod1.forcast)
mod1.cvpred <- lapply(block_list, fitmod, eq='Load ~WeekDays2 + Temp')%>%unlist
rmse(y=data0$Load, ychap=mod1.cvpred)

plot(data0[sel_a, ]$Temp, data0[sel_a, ]$Load)
plot(data0[sel_a, ]$Temp, mod1$residuals)

mod2 <- lm(Load ~ WeekDays2 + Temp + I(Temp^2), data=data0[sel_a, ])
mod2.forcast <- predict(mod2, newdata=data0[sel_b, ])
summary(mod2)
rmse(y=data0$Load[sel_b], ychap=mod2.forcast)

mod2.cvpred <- lapply(block_list, fitmod, eq='Load ~ WeekDays2 + Temp + I(Temp^2)')%>%unlist
rmse(y=data0$Load, ychap=mod2.cvpred)

plot(data0[sel_a, ]$Temp, mod2$residuals)

plot(data0$Date, data0$Load - mod2.cvpred, type='l')
lines(data0$Date[sel_b], data0$Load[sel_b] - mod2.forcast, col='red')


#### variance des scores par bloc ?
mod1.rmse_bloc <- lapply(block_list, function(x){rmse(y=data0$Load[x], ychap=mod1.cvpred[x])})%>%unlist
mod2.rmse_bloc <- lapply(block_list, function(x){rmse(y=data0$Load[x], ychap=mod2.cvpred[x])})%>%unlist

col <- yarr::piratepal('basel')
boxplot(cbind(mod1.rmse_bloc, mod2.rmse_bloc))






####################### Truncated Power Functions

plot(data0$Temp, data0$Load, pch=20)
data0$Temp_trunc1 <- pmax(data0$Temp-15, 0)
data0$Temp_trunc2 <- pmax(data0$Temp-20, 0)

plot(data0$Temp, data0$Temp_trunc1)

