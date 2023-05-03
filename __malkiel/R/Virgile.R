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

source('R/score.R')





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

eq = as.formula(paste("Load ~ WeekDays2 + Temp + Temp_trunc1 + Temp_trunc2+",fourier,sep=""))

form <- eq
form <- buildmer::add.terms(form, "Load.1")
form <- buildmer::add.terms(form, "Load.7")
form <- buildmer::add.terms(form, "Summer_break")
form <- buildmer::add.terms(form, "Christmas_break")
form <- buildmer::add.terms(form, "BH")
form <- buildmer::add.terms(form, "DLS")
form <- buildmer::add.terms(form, "GovernmentResponseIndex")

form1<-eq
form1 <- buildmer::add.terms(form, "Christmas_break")
form1 <- buildmer::add.terms(form, "BH")

mod6 <- lm(form, data=Data0[sel_a,])
mod6.forecast <- predict(mod6, newdata=Data0[sel_b,])
summary(mod6)
#rmse(y=Data0$Load[sel_b], ychap=mod6.forecast)

mod6.cvpred<-lapply(block_list, fitmod, eq=form)%>%unlist
#rmse(y=Data0$Load, ychap=mod6.cvpred)








###########################################################################################
#############soumission d'une prévision
###########################################################################################
mod5final <- glm(form1, data=Data0)
glm.forecast <- predict(mod5final, newdata=Data1)
submit1 <- read_delim( file="Data/sample_submission.csv", delim=",")
submit1$Load <- glm.forecast
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

predictions <- predict(model, newdata=Data1)

submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Load <- predictions

write.table(submit1, file = "Data/submission_10fold.CV.csv", quote = F, sep = ",", dec = ".", row.names = F)
rmse(y=Data1$Load, ychap=predictions)


plot(predictions[1:100], type='l')
lines(Data1[2:101, ]$Load.1, col='red')
lines(submit1[1:100, ]$Load, col='green')
submit2 <- read_delim( file="Data/sample_submission.csv", delim=",")
submit2$Load <- predictions


pred2 <- (predictions)*1/5 + (submit1$Load)*4/5

plot(Data1[2:101, ]$Load.1, type='l', col='red')
lines(predictions[1:100], col='black')
lines(submit1[1:100, ]$Load, col='green')
lines(pred2[1:100], col='blue')
library(h2o)
h2o.init()
train_df_h2o<-as.h2o(Data0)
test_df_h2o<-as.h2o(Data1)
attach(Data0)
x <- setdiff(names(Data0), Load)
ensemble <- h2o.stackedEnsemble(x=x, y="Load",validation_frame=test_df_h2o,
                                metalearner_algorithm="drf",
                                training_frame = train_df_h2o,
                                base_models = list(gam10, gam9))
data.frame(model,mod5final)
predict(Data1$Load.1, stack)
summary(stack)
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = Data1$Load.1)

submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- pred2
write.table(submit, file="Data/submission_glm_and_RandomForest_improved2.csv", quote=F, sep=",", dec='.',row.names = F)

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()


###############################Gam
library(mgcv)
