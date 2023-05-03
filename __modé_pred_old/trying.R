setwd("~/University/M1/S2/Project")

rm(list=objects())

library(tidyverse)
library(lubridate)
library(forecast)

data0 <- as.data.frame(read_delim("Data_raw/train.csv", delim=","))
data1 <- as.data.frame(read_delim("Data_raw/test.csv", delim=","))

# do not use Date, Time is the new thing
data0$Time <- as.numeric(data0$Date)
data1$Time <- as.numeric(data1$Date)

# factorize wished data
vars = c("BH", "DLS", "Summer_break", "Christmas_break")
for (var in vars) {
  data0[ ,var] <- as.factor(data0[, var])
  data1[ ,var] <- as.factor(data1[, var])
}
rm(var, vars)



sel_a <- which(data0$Year <= 2019)
sel_b <- which(data0$Year >  2019)





