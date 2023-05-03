# Fit GAM model using mgcv::gam()
names(Data0)
gam_model <- gam(Load ~ s(Date, k = 3) + s(Temp, k = 9) + s(Load.1, k = 6) + s(Load.7, k = 6) + s(Temp_s99, k = 9), data = Data0)

# Fit mixed-effects model using partykit::mob()
mob_model <- mob(gam_model | WeekDays + toy, model = "mixed", control = mob_control())

# Note: You may need to adjust the arguments for mob_control() based on your specific needs
rfout <- mobforest.analysis(Load ~ Temp + Load.1 + Load.7 + Temp_s99, partition_vars = partition_vars, mobforest_controls = mobforest.control(ntree = 10, mtry = 2, 
                                                                                                                                              replace = TRUE, alpha = 0.05, bonferroni = TRUE, minsplit = 10), data = Data0,
                            processors = 4, model = linearModel, seed = 1111)
names(Data0)

#######################################BOOSTING

library(dplyr)
library(xgboost)

# convert character columns to numeric
Data0 <- Data0 %>%
  mutate_if(is.character, as.numeric)
Data1<-Data1 %>%
  mutate_if(is.character, as.numeric)
# create xgb.DMatrix object
dtrain <- xgb.DMatrix(data = as.matrix(Data0[, -1]), label = Data0$Load, nthread = 2)
dtest<- xgb.DMatrix(data = as.matrix(Data1[, -1]), label = Data1$Load, nthread = 2)
watchlist <- list(train = dtrain, eval = dtest)
