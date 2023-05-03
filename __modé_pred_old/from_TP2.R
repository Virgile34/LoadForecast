setwd("~/University/M1/S2/Project")

rm(list=objects())

library(tidyverse)
library(lubridate)

Data0 <- as.data.frame(read_delim("Data_raw/train.csv", delim=","))
Data1 <- as.data.frame(read_delim("Data_raw/test.csv", delim=","))

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

mod6 <- lm(form, data=Data0[sel_a,])
mod6.forecast <- predict(mod6, newdata=Data0[sel_b,])
summary(mod6)
rmse(y=Data0$Load[sel_b], ychap=mod6.forecast)

mod6.cvpred<-lapply(block_list, fitmod, eq=form)%>%unlist
rmse(y=Data0$Load, ychap=mod6.cvpred)








###########################################################################################
#############soumission d'une prévision
###########################################################################################
mod5final <- lm(form, data=Data0)
lm.forecast <- predict(mod5final, newdata=Data1)
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- lm.forecast
write.table(submit, file="Data/submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)


######################################################################################################################################################
##############################Random forest Malkiel
#####################################################################################################################################################
library(randomForest)
library(caret)

fitControl <- trainControl(method = "cv", number = 10)
model <- train(Load~., data = Data0, method = "cforest", trControl = fitControl)

predictions <- predict(model, newdata=Data1)

submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Load <- predictions

write.table(submit, file = "Data/submission_10fold.CV.csv", quote = F, sep = ",", dec = ".", row.names = F)
rmse(y=Data1$Load, ychap=predictions)

plot(predictions[1:100], type='l')
lines(Data1[2:101, ]$Load.1, col='red')
lines(submit[1:100, ]$Load, col='green')

pred2 <- (predictions + submit$Load)/2

plot(Data1[2:101, ]$Load.1, type='l', col='red')
lines(predictions[1:100], col='black')
lines(submit[1:100, ]$Load, col='green')
lines(pred2[1:100], col='blue')


submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- pred2
write.table(submit, file="Data/submission_lm_and_RandomForest.csv", quote=F, sep=",", dec='.',row.names = F)
