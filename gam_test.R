setwd("~/University/M1/S2/modé_pred")


rm(list=objects())

source("R/score.R")
source("R/CV.R")

### Loading datas ###
library(tidyverse)

data0 <- read_delim("Data/train.csv", delim = ",")
data1 <- read_delim("Data/test.csv", delim = ",")

##Exclude the Covid period from data0
# d1 <- which(as.character(data0$Date)=="2020-03-15")
# data0 <- data0[1:d1, ]

data0$Date <- as.numeric(data0$Date)
data1$Date <- as.numeric(data1$Date)


compute_rmse_CV <- FALSE
rmse.CV <- list()

list.model <- list()

# data1 <- data0[which(data0$Year > 2019), ]
# data0 <- data0[which(data0$Year <= 2019), ]

# numeric_vars <- c('Date', 'Load.1', 'Load.7', 'Temp', 'Temp_s95', 'Temp_s99', 'Temp_s95_min', 'Temp_s95_max', 'Temp_s99_min', 'Temp_s99_max', 'toy', 'Year', 'GovernmentResponseIndex')
# factor_vars <- c('WeekDays', 'BH', 'Month', 'DLS', 'Summer_break', 'Christmas_break')



library(mgcv)

##### model 9
equation9 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                    s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                    s(Load.1, bs='cr') + s(Load.7, bs='cr') +
                    s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                    te(Temp_s95_max, Temp_s99_max) + te(Temp_s95_min, Temp_s99_min)

if (compute_rmse_CV) {
    CV_forecast9 <- CV_pred(equation9)
    CV_residuals9 <- data0$Load - CV_forecast9
    rmse9 <- rmse(data0$Load, CV_forecast9)
    rmse.CV[["rmse9"]] <- rmse9
}

gam9 <- gam(equation9, data=data0)
gam9.forecast <- predict(gam9,  newdata=data1)
gam9$gcv.ubre%>%sqrt
gam.check(gam9)

list.model[["gam9"]] = gam9


##### model 9 variante ti
equation9_ti <- Load ~  WeekDays + BH + Summer_break + Christmas_break +
                        s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                        s(Load.1, bs='cr') + s(Load.7, bs='cr') +
                        s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                        ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                        ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max)

if (compute_rmse_CV) {
    CV_forecast9_ti <- CV_pred(equation9_ti)
    CV_residuals9_ti <- data0$Load - CV_forecast9_ti
    rmse9_ti <- rmse(data0$Load, CV_forecast9_ti)
    rmse.CV[["rmse9_ti"]] <- rmse9_ti
}

gam9_ti <- gam(equation9_ti, data=data0)
gam9_ti.forecast <- predict(gam9_ti,  newdata=data1)
gam9_ti$gcv.ubre%>%sqrt
gam.check(gam9_ti)

list.model[["gam9_ti"]] = gam9_ti


##### model 10 -> interaction between Temp_s95_min, Temp_s99_max and Temp_s95_max, Temp_s99_min
equation10 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                     s(Load.1, bs='cr') + s(Load.7, bs='cr') +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     te(Temp_s95_max, Temp_s99_max) + te(Temp_s95_min, Temp_s99_min) +
                     te(Temp_s95_min, Temp_s99_max) + te(Temp_s99_min, Temp_s95_max)


if (compute_rmse_CV) {
    CV_forecast10 <- CV_pred(equation10)
    rmse10 <- rmse(data0$Load, CV_forecast10)
    rmse.CV[["rmse10"]] <- rmse10
}

gam10 <- gam(equation10, data=data0)
gam10.forecast <- predict(gam10,  newdata=data1)
gam10$gcv.ubre%>%sqrt
gam.check(gam10)

list.model[["gam10"]] = gam10


##### model 10_ti
equation10_ti <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                     s(Load.1, bs='cr') + s(Load.7, bs='cr') +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                     ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max) + 
                     ti(Temp_s95_min, Temp_s99_max) + ti(Temp_s99_min, Temp_s95_max)

if (compute_rmse_CV) {
    CV_forecast10_ti <- CV_pred(equation10_ti)
    rmse10_ti <- rmse(data0$Load, CV_forecast10_ti)
    rmse.CV[["rmse10_ti"]] <- rmse10_ti
}

gam10_ti <- gam(equation10_ti, data=data0)
gam10_ti.forecast <- predict(gam10_ti,  newdata=data1)
gam10_ti$gcv.ubre%>%sqrt
gam.check(gam10_ti)

list.model[["gam10_ti"]] = gam10_ti


##### model 11 -> by Weekdays for Load.1 and Load.7
equation11 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                     s(Load.1, bs='cr', by=as.factor(WeekDays)) + s(Load.7, bs='cr', by=as.factor(WeekDays)) +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     te(Temp_s95_max, Temp_s99_max) + te(Temp_s95_min, Temp_s99_min) +
                     te(Temp_s95_min, Temp_s99_max) + te(Temp_s99_min, Temp_s95_max)

if (compute_rmse_CV) {
    CV_forecast11 <- CV_pred(equation11)
    rmse11 <- rmse(data0$Load, CV_forecast11)
    rmse.CV[["rmse11"]] <- rmse11
}

gam11 <- gam(equation11, data=data0)
gam11.forecast <- predict(gam11,  newdata=data1)
gam11$gcv.ubre%>%sqrt
gam.check(gam11)

list.model[["gam11"]] = gam11


anova(gam9, gam9_ti, test='Chisq')  # -> not significant
                                    # -> significant (Pr(>Chi) = 0.001661) if non covid period
anova(gam9, gam10, test='Chisq')   #-> significant
anova(gam10, gam11, test='Chisq')  #-> significant 




##### model 11_ti
equation11_ti <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                     s(Load.1, bs='cr', by=as.factor(WeekDays)) + s(Load.7, bs='cr', by=as.factor(WeekDays)) +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                     ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max) + 
                     ti(Temp_s95_min, Temp_s99_max) + ti(Temp_s99_min, Temp_s95_max)

if (compute_rmse_CV) {
    CV_forecast11_ti <- CV_pred(equation11_ti)
    rmse11_ti <- rmse(data0$Load, CV_forecast11_ti)
    rmse.CV[["rmse11_ti"]] <- rmse11_ti
}

gam11_ti <- gam(equation11_ti, data=data0)
gam11_ti.forecast <- predict(gam11_ti,  newdata=data1)
gam11_ti$gcv.ubre%>%sqrt
gam.check(gam11_ti)

list.model[["gam11_ti"]] = gam11_ti


# 99.2 dev explained for 9, 9_ti, 10, 10_ti
# 99.3 dev explained for 11, 11_ti


##### model 12 -> by = workdays for load.1 load.7
library(forcats)

data0$WeekDays2 <- forcats::fct_recode(data0$WeekDays,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday', 'WorkDay'='Thursday')


equation12 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cc') +
                     s(Load.1, bs='cr', by=as.factor(WeekDays2)) + s(Load.7, bs='cr', by=as.factor(WeekDays2)) +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                     ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max) + 
                     ti(Temp_s95_min, Temp_s99_max) + ti(Temp_s99_min, Temp_s95_max)

if (compute_rmse_CV) {
    CV_forecast12 <- CV_pred(equation12)
    rmse12 <- rmse(data0$Load, CV_forecast12)
    rmse.CV[["rmse12"]] <- rmse12
}

gam12 <- gam(equation12, data=data0)
gam12.forecast <- predict(gam12,  newdata=data1)
gam12$gcv.ubre%>%sqrt
gam.check(gam12)

list.model[["gam12"]] = gam12



##### model 13 -> removing cc condition
equation13 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cr') +
                     s(Load.1, bs='cr', by=as.factor(WeekDays2)) + s(Load.7, bs='cr', by=as.factor(WeekDays2)) +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                     ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max) + 
                     ti(Temp_s95_min, Temp_s99_max) + ti(Temp_s99_min, Temp_s95_max)

if (compute_rmse_CV) {
    CV_forecast13 <- CV_pred(equation13)
    rmse13 <- rmse(data0$Load, CV_forecast13)
    rmse.CV[["rmse13"]] <- rmse13
}

gam13 <- gam(equation13, data=data0)
gam13.forecast <- predict(gam13,  newdata=data1)
gam13$gcv.ubre%>%sqrt
gam.check(gam13)

list.model[["gam13"]] = gam13


anova(gam11, gam11_ti, test='Chisq')

anova(gam11, gam12, test='Chisq')
anova(gam12, gam11, test='Chisq')

anova(gam11, gam13, test='Chisq')

anova(gam13, gam12, test='Chisq')


##### model 14 -> removing cc condition + by = weekdays (not workdays)
equation14 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
                     s(Date, k=3, bs='cr') + s(toy, k=30, bs='cr') +
                     s(Load.1, bs='cr', by=as.factor(WeekDays)) + s(Load.7, bs='cr', by=as.factor(WeekDays)) +
                     s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
                     ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
                     ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max) + 
                     ti(Temp_s95_min, Temp_s99_max) + ti(Temp_s99_min, Temp_s95_max)

if (compute_rmse_CV) {
    CV_forecast14 <- CV_pred(equation14)
    rmse14 <- rmse(data0$Load, CV_forecast14)
    rmse.CV[["rmse14"]] <- rmse14
}

gam14 <- gam(equation14, data=data0)
gam14.forecast <- predict(gam14,  newdata=data1)
gam14$gcv.ubre%>%sqrt
gam.check(gam14)

list.model[["gam14"]] = gam14



# ##### model 13 -> removing cc condition
# equation13 <- Load ~ WeekDays + BH + Summer_break + Christmas_break +
#                      s(Date, k=3, bs='cr') + s(toy, k=30, bs='cr') +
#                      s(Load.1, bs='cr', by=as.factor(WeekDays2)) + s(Load.7, bs='cr', by=as.factor(WeekDays2)) +
#                      s(Temp, k=10, bs='cr') + s(Temp_s99, k=10, bs='cr') +
#                      ti(Temp_s95_min) + ti(Temp_s95_max) + ti(Temp_s99_min) + ti(Temp_s99_max) +
#                      ti(Temp_s95_min, Temp_s99_min) + ti(Temp_s95_max, Temp_s99_max) + 
#                      ti(Temp_s95_min, Temp_s99_max) + ti(Temp_s99_min, Temp_s95_max)

# if (compute_rmse_CV) {
#     CV_forecast13 <- CV_pred(equation13)
#     rmse13 <- rmse(data0$Load, CV_forecast13)
#     rmse.CV[["rmse13"]] <- rmse13
# }

# gam13 <- gam(equation13, data=data0)
# gam13.forecast <- predict(gam13,  newdata=data1)
# gam13$gcv.ubre%>%sqrt
# gam.check(gam13)

# list.model[["gam13"]] = gam13


# saveRDS(rmse.CV, file = "Results/rmseCV_gams.RDS")


####################################################################################################
######################### observation of rmse
####################################################################################################
if (!compute_rmse_CV) {
    rmse.CV <- readRDS(file = 'Results/rmseCV_gams.RDS')
}

data1 <- subset(data1, selec = -Id)
data1 <- add_column(data1, Load=lead(data1$Load.1, default = mean(data1$Load.1)), .after = "Date")
data1$WeekDays2 <- forcats::fct_recode(data1$WeekDays,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday', 'WorkDay'='Thursday')


rmse.data = data.frame(matrix(nrow = length(list.model), ncol=2))
colnames(rmse.data) <- c('data0', 'data1')
rownames(rmse.data) <- names(list.model)

for (name in names(list.model)) {
    y.pred.data0 <- predict(list.model[[name]], newdata=data0)
    y.pred.data1 <- predict(list.model[[name]], newdata=data1)

    rmse.data[name, ] <- list(data0 = rmse(y.pred.data0, data0$Load), data1 = rmse(y.pred.data1, data1$Load))
}

par(mfrow=c(1,1))
plot(rmse.data$data0 / max(rmse.data$data0), type='l', col='black')
lines(rmse.data$data1 / max(rmse.data$data1), col='blue')
lines(unlist(rmse.CV) / max(unlist(rmse.CV)), col='red')


####################################################################################################
######################### residual correction with ARIMA
####################################################################################################
library(forecast)

equation <- equation13
                     
model = gam(equation, data=data0)

forcast_data0 <- CV_pred(equation, model=gam, n_block=10, data=data0)
model.forcast <- predict(model, newdata=data1)

residuals_data0 <- data0$Load - forcast_data0
residuals_data1 <- data1$Load - model.forcast

residuals_data0.ts <- ts(residuals_data0, frequency = 7)

arima.best <- auto.arima(residuals_data0.ts, max.p=3, max.q=4, 
                                             max.P=2, max.Q=2, 
                                             trace=T, ic="aic", method="CSS")


residuals.ts <- ts(c(residuals_data0, residuals_data1), frequency = 7)

refit <- Arima(residuals.ts, model=arima.best)
ARIMA.residuals.forecast <- tail(refit$fitted, nrow(data1))

model.arima.forecast <- model.forcast + ARIMA.residuals.forecast
rmse(data1$Load, model.arima.forecast)

# submit <- read_delim(file="Data/sample_submission.csv", delim=",")
# submit$Load <- model.arima.forecast
# write.table(submit, file="Submissions/gam_arima.csv", quote=F, sep=",", dec='.',row.names = F)


### searching for the best arima every ...
period = 121

model.arima_online.forecast <- model.forcast + ARIMA.residuals.forecast

for (i in 1:nrow(data1)) {
    if (i %% period == 0) {
        residuals_known.ts = ts(c(residuals_data0, residuals_data1[1:i-1]), frequency = 7)
        arima.best <- auto.arima(residuals_known.ts, max.p=3, max.q=4, 
                                                     max.P=2, max.Q=2, 
                                                     trace=T, ic="aic", method="CSS")
        refit <- Arima(residuals.ts, model=arima.best)
        ARIMA.residuals.forecast <- tail(refit$fitted, nrow(data1))
     
        model.arima_online.forecast[i:nrow(data1)] <- model.forcast[i:nrow(data1)] + ARIMA.residuals.forecast[i:nrow(data1)]
    }
}
rmse(data1$Load, model.arima.forecast) #1558
rmse(data1$Load, model.arima_online.forecast)

### arima ... (2 year ?) window
win_size = 730

residuals_calibration.ts <- residuals_data0%>%tail(n=win_size)%>%ts(frequency = 7)

arima.best <- auto.arima(residuals_calibration.ts, max.p=3, max.q=4, 
                                                   max.P=2, max.Q=2, 
                                                   trace=T, ic="aic", method="CSS")

residuals_forecast.ts <- ts(c(residuals_calibration.ts, residuals_data1), frequency = 7)

refit <- Arima(residuals_forecast.ts, model=arima.best)
ARIMA.residuals.forecast <- tail(refit$fitted, nrow(data1))

model.arima_window.forecast <- model.forcast + ARIMA.residuals.forecast

rmse(data1$Load, model.arima_window.forecast)


for (i in 1:nrow(data1)) {
    if (i %% 121 == 0) {
        residuals_calibration <- c(tail(residuals_data0, n=win_size-i+1), residuals_data1[1:i-1])
        residuals_calibration.ts <- ts(residuals_calibration, frequency = 7)

        arima.best <- auto.arima(residuals_calibration.ts, max.p=3, max.q=4, 
                                                           max.P=2, max.Q=2, 
                                                           trace=T, ic="aic", method="CSS")
        
        residuals_forecast.ts <- ts(c(tail(residuals_data0, n=win_size-i+1), residuals_data1), frequency = 7)
        # residuals_forecast.ts <- ts(residuals_data1, frequency = 7)

        refit <- Arima(residuals_forecast.ts, model=arima.best)
        
        ARIMA.residuals.forecast <- tail(refit$fitted, n=nrow(data1))
        model.arima_window.forecast[i:nrow(data1)] <- model.forcast[i:nrow(data1)] + ARIMA.residuals.forecast[i:nrow(data1)]
    }

}
rmse(data1$Load, model.arima_window.forecast)


submit <- read_delim(file="Data/sample_submission.csv", delim=",")
submit$Load <- model.arima_window.forecast
write.table(submit, file="Submissions/gam_arimaWindow.csv", quote=F, sep=",", dec='.',row.names = F)





