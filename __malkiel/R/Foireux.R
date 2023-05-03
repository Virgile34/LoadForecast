rm(list=objects())

setwd("C:/Users/malki/OneDrive/Bureau/M1AI CHALLENGE COVID")



library(tidyverse)
library(lubridate)

Data0 <- as.data.frame(read_delim("Data/train.csv", delim=","))
Data1 <- as.data.frame(read_delim("Data/test.csv", delim=","))

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

# factorize wished data
vars = c("BH", "DLS", "Summer_break", "Christmas_break")
for (var in vars) {
  Data0[ ,var] <- as.factor(Data0[, var])
  Data1[ ,var] <- as.factor(Data1[, var])
}
rm(var, vars)

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

#source('R/score.R')





######bloc CV
Nblock<-8
borne_block<-seq(1, nrow(Data0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))


fitmod <- function(eq, block)
{
  mod <- lm(eq, data=Data0[-block,])
  mod.cvpred <- predict(mod, newdata=Data0[block,])
  return(mod.cvpred)
}


#####regroupement de modalités
Data0$WeekDays2 <- forcats::fct_recode(Data0$WeekDays, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday')
Data1$WeekDays2 <- forcats::fct_recode(Data1$WeekDays, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday')

## feature engineering
Data0$Temp_trunc1 <- pmax(Data0$Temp-15,0)
Data1$Temp_trunc1 <- pmax(Data1$Temp-15,0)

Data0$Temp_trunc2 <- pmax(Data0$Temp-20,0)
Data1$Temp_trunc2 <- pmax(Data1$Temp-20,0)


##################################################################################cycle annuel: fourier
w<-2*pi/(365)
Nfourier<-15
for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*Data0$Time*i))
  assign(paste("sin", i, sep=""),sin(w*Data0$Time*i))
}
objects()

cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")

Data0<-eval(parse(text=paste("data.frame(Data0,",cos,",",sin,")",sep="")))
names(Data0)

for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*Data1$Time*i))
  assign(paste("sin", i, sep=""),sin(w*Data1$Time*i))
}

cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")

Data1<-eval(parse(text=paste("data.frame(Data1,",cos,",",sin,")",sep="")))
names(Data1)



cos<-paste(c('cos'),c(1:Nfourier),sep="")
sin<-paste(c('sin'),c(1:Nfourier),sep="")
fourier<-paste(c(cos,sin),collapse="+")

eq = as.formula(paste("Load ~ WeekDays2 + Temp +",sep=""))

form <- eq
form <- buildmer::add.terms(form, "Load.1")
#form <- buildmer::add.terms(form, "Load.7")
form <- buildmer::add.terms(form, "Summer_break")
form <- buildmer::add.terms(form, "Christmas_break")
form <- buildmer::add.terms(form, "BH")
form <- buildmer::add.terms(form, "DLS")
form <- buildmer::add.terms(form, "GovernmentResponseIndex")

form1<-eq
form1 <- buildmer::add.terms(form1, "Christmas_break")
form1 <- buildmer::add.terms(form1, "BH")
form1 <- buildmer::add.terms(form1, "Load.1")
###########################################################################################
#############soumission d'une prévision
###########################################################################################


mod5final <- lm(form1, data=Data0)
submit1 <- read_delim( file="Data/sample_submission.csv", delim=",")
submit1$Load <- predict(mod5final, newdata=Data1)
write.table(submit, file="Data/submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)


######################################################################################################################################################
##############################Random forest Malkiel
#####################################################################################################################################################
library(randomForest)
library(caret)
library(ipred)
library(h2o)
rmse <- function(y, ychap, digits=0){
  return( round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits) )
}
fitControl <- trainControl(method = "cv", number = 10)
model <- train(form, data = Data0, method = "cforest", trControl = fitControl)

predictions_cv <- predict(model, newdata=Data1)

submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Load <- predictions_cv


##################GAM
gam.fit <- gam(form, data = Data0)
gam.forecast <- predict(gam.fit, newdata = Data1)

#####################GBM
trControl <- trainControl(method = "cv", number = 10)
gbm.fit <- train(form, data = Data0, method = "gbm",trControl = trControl)
gbm.forecast <- predict(gbm.fit, newdata = Data1)
submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Load <- gbm.forecast
write.table(submit, file="Data/submission_gbm_improve_CV.csv", quote=F, sep=",", dec='.',row.names = F)


###################ARFORECAST
# Medium term model to remove trend and seasonality (using generalized additive model)
D<-data.frame(matrix(1:9, nrow = 3303, ncol = 2))
detrend.fit <- gam(form1, data = Data0)

D$X1 <- c(predict(detrend.fit), predict(detrend.fit,newdata = Data1))
D$X2 <-Data0$Load+Data1$Load.1 - D$X1

ar.forecast <- numeric(3028)
for (i in seq(3028)) {
  ar.fit <- ar(Data1$Load.1$Residuals[1:(275[i] - 1)])
  ar.forecast[i] <- as.numeric(predict(ar.fit)$pred) + electric_load$Medium[275[i]]
}


#pred3 <-  1/4*submit1$Load+3/4*gbm.forecast

plot(Data1[1:100, ]$Load.1, type='l', col='red')
lines(predictions_cv[1:100], col='black')
lines(submit1[1:100, ]$Load, col='green')
lines(pred3[1:100], col='blue')
lines(gbm.forecast[1:100], col='orange')
submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Load <- pred3

rmse(Data1$Load.1,gbm.forecast)
h2o.init()

train_df_h2o<-as.h2o(Data0)
test_df_h2o<-as.h2o(Data1)
attach(Data0)
ensemble <- h2o.stackedEnsemble(y="Load",validation_frame=test_df_h2o,
                                metalearner_algorithm="xgboost",
                                training_frame = train_df_h2o,metalearner_nfolds=5,
                                base_models = list(model, gbm.fit,fit, gam.fit,expert_mob))
data.frame(model,mod5final)
predict(Data1$Load.1, stack)
summary(stack)
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Data1$Load.1)

submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- pred2
write.table(submit, file="Data/submission_glm_and_Gradient.csv", quote=F, sep=",", dec='.',row.names = F)



library(mgcv)
library(party)
model_mob<-mob(Data0~s(as.numeric(Date),k=3, bs='cr')+ s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
                 s(Temp_s99,k=10, bs='cr') |WeekDays2 + toy, model=gam, control=mob)

# Note: You may need to adjust the arguments for mob_control

# Fit GAM model using mgcv::gam()
library(partykit)
attach(Data_train)
gam_model <- gam(Load ~ s(Temp, k = 9, bs = "bs") + s(Load.1, k = 6, bs = "bs") + s(Load.7, k = 6, bs = "bs") + s(Temp_s99, k = 9, bs = "bs"), data = Data0)

# Fit mixed-effects model using partykit::mob()
mob_model <- mob(formula(gam_model), WeekDays + toy+Summer_break+Christmas_break, model = glinearModel, control = mob_control(), data=Data0)

names(gam_model)
# Note: You may need to adjust the arguments for mob_control() based on your specific needs

###############################AUTOARIMA
library(forecast)
# convert the date column to a time series object
data_ts <- ts(Data0$Load, start = c(2010, 1), frequency = 365)

# fit the ARIMA model
fit <- auto.arima(data_ts)

# make predictions
predictions <- forecast(fit, h = 365)

# plot the predictions
plot(predictions)
