setwd("~/University/M1/S2/modé_pred")


rm(list=objects())

source("R/score.R")
source("R/CV.R")

### Loading datas ###
library(tidyverse)
library(forcats)

data0 <- read_delim("Data/train.csv", delim = ",")
data1 <- read_delim("Data/test.csv", delim = ",")

data1 <- subset(data1, selec = -Id)
data1 <- add_column(data1, Load=lead(data1$Load.1, default = mean(data1$Load.1)), .after = "Date")


data0$Date <- as.numeric(data0$Date)
data0$WeekDays2 <- forcats::fct_recode(data0$WeekDays,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday', 'WorkDay'='Thursday')

data1$Date <- as.numeric(data1$Date)
data1$WeekDays2 <- forcats::fct_recode(data1$WeekDays,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday', 'WorkDay'='Thursday')


experts <- data.frame(matrix(nrow=nrow(data1), ncol = 0))
experts <- readRDS('Experts/experts.RDS')

SEED = 10042000


## CV block
n_block <- 10
borne_block <- seq(1, nrow(data0), length = n_block + 1)%>%floor
CV_block_list <- list()
for (i in c(2:n_block)){
    CV_block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
CV_block_list[[n_block]] <- c(borne_block[n_block]:borne_block[n_block+1])


################################################################################
##################################### gams #####################################
################################################################################
library(mgcv)

blockRMSE <- function(equation, block) {
  g <- gam(as.formula(equation), data=data0[-block,])
  return(predict(g, newdata=data0[block,]))
}
# lapply(CV_block_list, blockRMSE, equation=equation)%>%unlist

equation0 <- Load ~ WeekDays + 
                    s(toy,k=30, bs='cc') + 
                    s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
                    s(Temp,k=10, bs='cr') + s(Temp_s99,k=10, bs='cr')


equation9 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                    s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                    s(Load.1, bs='cr') + s(Load.7, bs='cr') +
                    s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                    te(Temp_s95_max, Temp_s99_max) + te(Temp_s95_min, Temp_s99_min)


equation13 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cr') +
                     s(Load.1, bs='cr', by=as.factor(WeekDays2)) + s(Load.7, bs='cr', by=as.factor(WeekDays2)) +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                     ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max) + 
                     ti(Temp_s95_min, Temp_s99_max) + ti(Temp_s99_min, Temp_s95_max)


library(forecast)

make_gam.arima <-function(num) {
    eq <- get(paste0('equation', num))

    model = gam(eq, data=data0)
    model.forcast <- predict(model, newdata=data1)

    forcast_data0 <- lapply(CV_block_list, blockRMSE, equation=eq)%>%unlist

    residuals_data0.ts <- ts(data0$Load - forcast_data0, frequency = 7)
    arima.best <- auto.arima(residuals_data0.ts, max.p=3, max.q=4, 
                                                 max.P=2, max.Q=2, 
                                                 trace=T, ic="aic", method="CSS")


    residuals.ts <- ts(c(residuals_data0.ts, data1$Load - model.forcast), frequency = 7)
    
    refit <- Arima(residuals.ts, model=arima.best)
    ARIMA.residuals.forecast <- tail(refit$fitted, nrow(data1))

    model.arima.forecast <- model.forcast + ARIMA.residuals.forecast

    experts[, paste0('gam', num)] <<- model.forcast
    experts[, paste0('gam', num, '.arima')] <<- model.arima.forecast
    return(forcast_data0)
}

CV.preds = lapply(c(0, 9, 13), make_gam.arima)

plot(1:6, apply(experts, 2, rmse, y=data1$Load), type='l')



### sunday gam ###
equationSunday <- Load ~  BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cr') +
                     s(Load.1, bs='cr') + s(Load.7, bs='cr') +
                     s(Temp, k=10, bs='cr') + s(Temp_s95, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                    #  ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                     te(Temp_s95_min, Temp_s99_min) + te(Temp_s95_max, Temp_s99_max) + 
                     te(Temp_s95_min, Temp_s99_max) + te(Temp_s99_min, Temp_s95_max)

sel_sunday <- which(data0$WeekDays=="Sunday")
model = gam(equationSunday, data=data0[sel_sunday, ])

model.forcast <- predict(model, newdata=data1)

## 5 CV block otherwise it won't converge...
n_block_Sunday <- 5
borne_block <- seq(1, nrow(data0), length = n_block_Sunday + 1)%>%floor
CV_block_list_Sunday <- list()
for (i in c(2:n_block_Sunday)){
    CV_block_list_Sunday[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
CV_block_list_Sunday[[n_block_Sunday]] <- c(borne_block[n_block_Sunday]:borne_block[n_block_Sunday+1])

blockRMSE_Sunday <- function(equation, block) {
    train <- data0[-block, ]
    sel <- which(train$WeekDays=="Sunday")
    g <- gam(as.formula(equation), data=train[sel, ])
    return(predict(g, newdata=data0[block,]))
}

forcast_data0 <- lapply(CV_block_list_Sunday, blockRMSE_Sunday, equation=equationSunday)%>%unlist

residuals_data0.ts <- ts(data0$Load - forcast_data0, frequency = 7)
arima.best <- auto.arima(residuals_data0.ts, max.p=3, max.q=4, 
                                                max.P=2, max.Q=2, 
                                                trace=T, ic="aic", method="CSS")


residuals.ts <- ts(c(residuals_data0.ts, data1$Load - model.forcast), frequency = 7)

refit <- Arima(residuals.ts, model=arima.best)
ARIMA.residuals.forecast <- tail(refit$fitted, nrow(data1))

model.arima.forecast <- model.forcast + ARIMA.residuals.forecast

rmse(model.forcast, data1$Load) # 3788
rmse(model.arima.forecast, data1$Load) # 1636



##### sunday window #####
win_size = 121*7

train <- data0[1:(nrow(data0)-win_size), ]
sel_sunday <- which(train$WeekDays=="Sunday")

model_calibration = gam(equationSunday, data=train[sel_sunday, ])
model = gam(equationSunday, data=data0[which(data0$WeekDays=='Sunday'), ])

model_data0.forcast <- predict(model_calibration, newdata=data0)
model.forcast <- predict(model, newdata=data1)

residuals_data0 <- data0$Load - model_data0.forcast
residuals_data1 <- data1$Load - model.forcast

residuals_calibration.ts <- residuals_data0%>%tail(n=win_size)%>%ts(frequency=7)

arima.best <- auto.arima(residuals_calibration.ts, max.p=3, max.q=4, 
                                                   max.P=2, max.Q=2, 
                                                   trace=T, ic="aic", method="CSS",
                                                   seasonal.test = 'hegy')

residuals_forecast.ts <- ts(c(residuals_calibration.ts, residuals_data1), frequency = 7)

refit <- Arima(residuals_forecast.ts, model=arima.best)
ARIMA.residuals.forecast <- tail(refit$fitted, nrow(data1))

model.arima_window.forecast <- model.forcast + ARIMA.residuals.forecast
plot(data1$Load, type='l')
lines(as.vector(model.arima_window.forecast), type='l', col='blue')

rmse(data1$Load, model.arima_window.forecast) #1658
rmse(data1$Load, model.forcast) #3788


for (i in 1:nrow(data1)) {
    if (i %% 90 == 0) {
        residuals_calibration <- c(tail(residuals_data0, n=win_size-i+1), residuals_data1[1:i-1])
        residuals_calibration.ts <- ts(residuals_calibration, frequency = 7)

        arima.best <- auto.arima(residuals_calibration.ts, max.p=3, max.q=4, 
                                                           max.P=2, max.Q=2, 
                                                           trace=F, ic="aic", method="CSS")
        
        residuals_forecast.ts <- ts(c(tail(residuals_data0, n=win_size-i+1), residuals_data1), frequency = 7)
        # residuals_forecast.ts <- ts(residuals_data1, frequency = 7)

        refit <- Arima(residuals_forecast.ts, model=arima.best)
        
        ARIMA.residuals.forecast <- tail(refit$fitted, n=nrow(data1))
        model.arima_window.forecast[i:nrow(data1)] <- model.forcast[i:nrow(data1)] + ARIMA.residuals.forecast[i:nrow(data1)]
    }

}
rmse(data1$Load, model.arima_window.forecast) #1648
lines(as.vector(model.arima_window.forecast), type='l', col='#ff0000')




experts[, 'gamSunday'] <- model.forcast
experts[, 'gamSunday.arima'] <- model.arima.forecast

experts.rmse <- apply(experts, 2, rmse, y=data1$Load)
par(mfrow=c(1,1))
plot(1:ncol(experts), experts.rmse, type='l')
text(1:ncol(experts), min(experts.rmse), labels= colnames(experts), pos=3, srt=90, adj=1)
points(1:ncol(experts), experts.rmse, pch=20, cex=3)

# saveRDS(experts, 'Experts/experts.RDS')


################################################################################
#################################### forest ####################################
################################################################################
library(ranger)


#####extra trees
equation <- Load ~  Date + toy + Temp + Load.1 + Load.7 + Temp_s95 + Temp_s99 + 
                    WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break + 
                    Christmas_break + Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex

ntree=500
mtry=12
rf <-ranger::ranger(equation, data=data0, num.trees=ntree, mtry=mtry)
rf.forecast <- predict(rf, data=data1)$prediction

rf.random <- ranger::ranger(equation, data=data0, num.trees = ntree, 
                            splitrule = "extratrees", mtry=1, num.random.splits=1)
rf.random.forecast <- predict(rf.random, data=data1)$prediction

rf.forecast.update <- rf.forecast
rf.random.forecast.update <- rf.random.forecast

for(i in c(1:(nrow(data1)-1))) {
    rf <- ranger::ranger(equation, data=rbind(data0, data1[1:i,]), num.trees=ntree, mtry=mtry)
    rf.forecast.update[i+1] <- predict(rf, data=data1[1:i+1,])$prediction%>%tail(1)

    rf.random <- ranger::ranger(equation, data=rbind(data0, data1[1:i,]), num.trees=ntree, 
                                splitrule="extratrees", mtry=1, num.random.splits=1)
    rf.random.forecast.update[i+1] <- predict(rf.random, data=data1[1:i+1,])$prediction%>%tail(1)
    print(i)
}

experts[ ,'rf'] <- rf.forecast
experts[ ,'rf.online'] <- rf.forecast.update
experts[ ,'rf.random'] <- rf.random.forecast
experts[ ,'rf.random.online'] <- rf.random.forecast.update


experts.rmse <- apply(experts, 2, rmse, y=data1$Load)
plot(1:ncol(experts), experts.rmse, type='l')
text(1:ncol(experts), min(experts.rmse), labels= colnames(experts), pos=3, srt=90, adj=1)
points(1:ncol(experts), experts.rmse, pch=20, cex=3)

# saveRDS(experts, 'Experts/experts.RDS')



################################################################################
################################## forest gam ##################################
################################################################################
equation <- equation13
gam <- gam(equation, data=data0)
gam.forecast <- predict(gam, newdata=data1)
gam.forecast.CV <- lapply(CV_block_list, blockRMSE, equation=equation)%>%unlist

terms0 <- predict(gam, newdata=data0, type='terms')
terms1 <- predict(gam, newdata=data1, type='terms')
colnames(terms0) <- paste0("gterms_", c(1:ncol(terms0)))
colnames(terms1) <- paste0("gterms_", c(1:ncol(terms1)))


data0_rf <- data.frame(data0, terms0)
residualsCV <- data0$Load - gam.forecast.CV
data0_rf$residuals <- residualsCV
data0_rf$res.1 <- c(residualsCV[1], residualsCV[1:(length(residualsCV)-1)])
data0_rf$res.7 <- c(residualsCV[1:7], residualsCV[1:(length(residualsCV)-7)])

data1_rf <- data.frame(data1, terms1)
residuals <- data1_rf$Load - gam.forecast
data1_rf$residuals <- residuals
data1_rf$res.1 <- c(residuals[1], residuals[1:(length(residuals)-1)])
data1_rf$res.7 <- c(residuals[1:7], residuals[1:(length(residuals)-7)])

cov <- "Date + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + res.1 + res.7 +"
gterm <-paste0("gterms_", c(1:ncol(terms0)))
gterm <- paste0(gterm, collapse='+')
cov <- paste0(cov, gterm, collapse = '+')
formule_rf <- as.formula(paste0("residuals", "~", cov))

ntree=500
mtry=NULL
rf_gam<- ranger::ranger(formule_rf, data=data0_rf, num.trees=ntree, mtry=mtry, seed=SEED)

rf_gam.forecast <- predict(rf_gam, data=data1_rf)$predictions+ gam.forecast
rf_gam.forecast.update <- rf_gam.forecast

for (i in c(1:(nrow(data1)-1))) {
    rf_gam <- ranger::ranger(formule_rf, data=rbind(data0_rf, data1_rf[1:i, ]), num.trees=ntree, mtry=mtry, seed=SEED)
    rf_gam.forecast.update[i+1] <- predict(rf, data=data1[1:i+1,])$prediction%>%tail(1)
    cat(i, "/", nrow(data1)-1, "\r")
}

rmse(y=data1$Load, ychap=rf_gam.forecast)
rmse(y=data1$Load, ychap=rf_gam.forecast.update) 
# eq13 : 2626 2113
# eq9 : 2286 2141
# eq0 : 2346 2141

# experts <- readRDS('Experts/experts.RDS')

# experts[ ,'rf_gam'] <- rf_gam.forecast
# experts[ ,'rf_gam.online'] <- rf_gam.forecast.update

# saveRDS(experts, 'Experts/experts.RDS')



################################################################################
################################ kalman filters ################################
################################################################################
library(viking)

equation <- equation0
gam <- gam(equation, data=data0)
gam.forecast <- predict(gam, newdata=data1)

X <- predict(gam, newdata=rbind(data0, data1), type='terms')
###scaling columns
for (j in 1:ncol(X))
    X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
X <- cbind(X,1)
d <- ncol(X)

y <- rbind(data0, data1)$Load


# static 
ssm <- viking::statespace(X[1:nrow(data0), ], y[1:nrow(data0)])
gam.kalman.static <- ssm$pred_mean%>%tail(nrow(data1))

rmse(y=data1$Load, ychap=gam.forecast)
rmse(y=data1$Load, ychap=gam.kalman.static)

# dynamic
# using iterative grid search
ssm_dyn <- viking::select_Kalman_variances(ssm, X[1:nrow(data0), ], y[1:nrow(data0)],
                                           q_list = 2^(-30:0), p1 = 1, ncores = 1)
# saveRDS(ssm_dyn, file="Experts/ssm_dyn_gam0.RDS")

ssm_dyn.pred <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam.kalman.Dyn.it <- ssm_dyn.pred$pred_mean%>%tail(nrow(data1))



# using expectation-maximization
ssm_em  <- viking::select_Kalman_variances(ssm, X[1:nrow(data0), ], y[1:nrow(data0)], 
                                           method = 'em', n_iter = 10^3,
                                           Q_init = diag(d), verbose = 10, mode_diag = T)
# saveRDS(ssm_em, file="Experts/ssm_em_gam0.RDS") 

ssm_em.pred <- predict(ssm_em, X, y, type='model', compute_smooth = TRUE)
gam.kalman.Dyn.em <- ssm_em.pred$pred_mean%>%tail(nrow(data1))

rmse(y=data1$Load, ychap=gam.forecast)
rmse(y=data1$Load, ychap=gam.kalman.static)
rmse(y=data1$Load, ychap=gam.kalman.Dyn.it)
rmse(y=data1$Load, ychap=gam.kalman.Dyn.em)

par(mfrow=c(1,1))

experts <- readRDS('Experts/experts.RDS')
experts.rmse <- apply(experts, 2, rmse, y=data1$Load)
plot(1:ncol(experts), experts.rmse, type='l')
lines(c(1,ncol(experts)), c(min(experts.rmse), min(experts.rmse)), col='blue')
text(1:ncol(experts), min(experts.rmse), labels= colnames(experts), pos=3, srt=90, adj=1)
points(1:ncol(experts), experts.rmse, pch=20, cex=3)



experts[ ,'gam0.kalman.stat'] <- gam.kalman.static
experts[ ,'gam0.kalman.Dyn.it'] <- gam.kalman.Dyn.it
experts[ ,'gam0.kalman.Dyn.em'] <- gam.kalman.Dyn.em

# saveRDS(experts, 'Experts/experts.RDS')
################################################################################
################################## expert agg ##################################
################################################################################
library(opera)

experts <- readRDS('Experts/experts.RDS')
or <- oracle(Y=data1$Load, experts)
or

# experts <- experts[, 1:(ncol(experts)-3)] #no effect to the oracle

# experts <- subset(experts, selec=-c(gam13, gamSunday)) #no effect to the oracle
# experts <- subset(experts, selec=-c(gam9, gam9.arima, gam0, gam0.arima)) #slight increase mape
# experts <- subset(experts, selec=-c(rf, rf.random)) #increase rmse 10 but best aggreg
# experts <- subset(experts, selec=-c(gam.kalman.Dyn.it)) #increase rmse 10 but best aggreg
experts <- subset(experts, selec=c(gam13, gam13.arima, gamSunday, gamSunday.arima))#, gamSunday.arima, gam.kalman.Dyn.em)) #slight increase mape
or <- oracle(Y=data1$Load, experts)
or

models = c("EWA", "FS", "Ridge", "MLpol", "MLewa", "MLprod", "BOA", "OGD", "FTRL")

for (model in models){
    agg <- mixture(Y = data1$Load, experts = experts, model=model, loss.gradient=TRUE)
    print(paste0(model, " : ", rmse(agg$prediction, data1$Load)))
}

agg <- mixture(Y = data1$Load, experts = experts, model='MLprod', loss.gradient=TRUE)
rmse(agg$prediction, data1$Load)
plot(agg)
submit <- read_delim(file="Data/sample_submission.csv", delim=",")
submit$Load <- agg$prediction
write.table(submit, file="Submissions/agg_gam13_gamSunday.csv", quote=F, sep=",", dec='.',row.names = F)


plot(agg)


experts.names <- colnames(experts)
expertsM2000 <- experts-2000
# expertsP2000 <- experts+4000
experts <- cbind(experts, expertsM2000)#, expertsP2000)
colnames(experts) <-c(experts.names, paste0(experts.names,  "M"))#, paste0(experts.names,  "P"))

or <- oracle(Y=data1$Load, experts)
or

for (model in models){
    agg <- mixture(Y = data1$Load, experts = experts, model=model, loss.gradient=TRUE)
    print(paste0(model, " : ", rmse(agg$prediction, data1$Load)))
}

agg <- mixture(Y = data1$Load, experts = experts, model='BOA', loss.gradient=TRUE)
rmse(agg$prediction, data1$Load)
plot(agg)


### meta arima ###
eq <- equation13

model = gam(eq, data=data0, seed=SEED)
model.forcast <- predict(model, newdata=data1)

forcast_data0 <- lapply(CV_block_list, blockRMSE, equation=eq)%>%unlist

residuals_data0.ts <- ts(data0$Load - forcast_data0, frequency = 7)
arima.best <- auto.arima(residuals_data0.ts, max.p=3, max.q=4, 
                                                max.P=2, max.Q=2, 
                                                trace=T, ic="aic", method="CSS")


residuals.ts <- ts(c(residuals_data0.ts, data1$Load - model.forcast), frequency = 7)

refit <- Arima(residuals.ts, model=arima.best)
ARIMA.residuals.forecast <- tail(refit$fitted, nrow(data1))

model.arima.forecast <- model.forcast + ARIMA.residuals.forecast

model.meta.arima.forecast <- model.arima.forecast

for (i in 30:nrow(data1)-1) {
    if (i %% 30 == 0) {
        slice = max(1, i-90):i
        meta.residuals.ts <- (data1$Load[slice] - model.arima.forecast[slice])%>%ts(frequency=7)
        meta.arima.best <- auto.arima(meta.residuals.ts, max.p=3, max.q=4, 
                            max.P=2, max.Q=2, 
                            trace=T, ic="aic", method="CSS")
        
        refit <- Arima(data1$Load - model.arima.forecast, model=arima.best)$fitted
        model.meta.arima.forecast[(i+1):nrow(data1)] <- model.arima.forecast[(i+1):nrow(data1)] + refit[(i+1):nrow(data1)]

    }

}

rmse(data1$Load, model.forcast)
rmse(data1$Load, model.arima.forecast)
rmse(data1$Load, model.meta.arima.forecast)


par(mfrow=c(1,1))
plot(residuals[1:274]%>%as.vector(), type='l')
lines(4000*data1$GovernmentResponseIndex%>%as.vector())




gam <- model
gam.arima.forecast <- model.arima.forecast

terms1 <- predict(gam, newdata=data1, type='terms')
colnames(terms1) <- paste0("gterms_", c(1:ncol(terms1)))

# data1_rf <- data.frame(data1, terms1)
data1_rf <- data.frame(data1)
residuals <- data1_rf$Load - gam.arima.forecast
data1_rf$residuals <- residuals
data1_rf$res.1 <- c(residuals[1], residuals[1:(length(residuals)-1)])
data1_rf$res.7 <- c(residuals[1:7], residuals[1:(length(residuals)-7)])

cov <- "Date + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + res.1 + res.7"
# gterm <-paste0("gterms_", c(1:ncol(terms0)))
# gterm <- paste0(gterm, collapse='+')
# cov <- paste0(cov, gterm, collapse = '+')
formule_rf <- as.formula(paste0("residuals", "~", cov))

ntree=500
mtry=NULL

rf_gam.forecast.update <- gam.arima.forecast

for (i in c(260:(nrow(data1)-1))) {
    rf_gam <- ranger::ranger(formule_rf, data=data1_rf[1:i, ], num.trees=ntree, seed=SEED,
                            splitrule = "extratrees", mtry=1, num.random.splits=1)
    rf_gam.forecast.update[i+1] <- predict(rf_gam, data=data1[1:i+1,])$prediction%>%tail(1)
    # cat(i, "/", nrow(data1)-1, "\r")
    print(i)
}



rmse(y=data1$Load, ychap=rf_gam.forecast)
rmse(y=data1$Load, ychap=rf_gam.forecast.update) 






experts <- readRDS('Experts/experts.RDS')
experts.rmse <- apply(experts, 2, rmse, y=data1$Load)
plot(1:ncol(experts), experts.rmse, type='l')
lines(c(1,ncol(experts)), c(min(experts.rmse), min(experts.rmse)), col='blue')
text(1:ncol(experts), min(experts.rmse), labels= colnames(experts), pos=3, srt=90, adj=1)
points(1:ncol(experts), experts.rmse, pch=20, cex=3)

min(experts.rmse)
moy = (experts$gam13.arima + experts$gamSunday.arima) / 2
rmse(y=data1$Load, ychap=moy)

submit <- read_delim(file="Data/sample_submission.csv", delim=",")
submit$Load <- moy
write.table(submit, file="Submissions/moy_gam13_gamSunday.csv", quote=F, sep=",", dec='.',row.names = F)


rmse(y=data1$Load, ychap=rf_gam.forecast.update) 
