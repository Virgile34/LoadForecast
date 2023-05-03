##################GAM
gam.fit <- gam(form, data = Data0[1:2922,])
gam.forecast <- predict(gam.fit, newdata = Data0[sel_b,])

#####################GBM
gbm.fit <- train(form, data = Data0[1:2922,], method = "gbm")
library(gbm)
gbm.forecast2<- gbm()


gbm.forecast <- predict(gbm.fit, newdata = Data0[sel_b,])


mod5final <- glm(form, data=Data0[1:2922,])
glm.forecast <- predict(mod5final, newdata=Data0[sel_b,])
summary(gbm.fit)


#####################GLM


# Aggregation of experts
###########################




X <- cbind(gam.forecast, gbm.forecast,glm.forecast)
colnames(X) <- c('gam', 'gbm','glm')
Y <- Data0[sel_b,]$Load

matplot(cbind(Y, X), type = 'l', col = 1:6, ylab = 'Weekly load', xlab = 'Week')


# How good are the expert? Look at the oracles
o<-oracle(Y = Y, experts = X,loss.type = 'square', model = 'expert')
predict(o$prediction, newdata=Data1)
if(interactive()){
  plot(oracle.convex)
}

oracle.convex

# Is a single expert the best over time ? Are there breaks ?
oracle.shift <- oracle(Y = Y, experts = X, model='expert',loss.type = 'percentage', model = 'shifting')
if(interactive()){
  plot(oracle.shift)
}
oracle.shift

# Online aggregation of the experts with BOA
#############################################

# Initialize the aggregation rule
m0.BOA <- mixture(model = 'BOA', loss.type = 'square')

# Perform online prediction using BOA There are 3 equivalent possibilities 1)
# start with an empty model and update the model sequentially
m1.BOA <- m0.BOA
for (i in 1:length(Y)) {
  m1.BOA <- predict(m1.BOA, newexperts = X[i, ], newY = Y[i], quiet = TRUE)
}

# 2) perform online prediction directly from the empty model
m2.BOA <- predict(m0.BOA, newexpert = X, newY = Y, online = TRUE, quiet = TRUE)

# 3) perform the online aggregation directly
m3.BOA <- mixture(Y = Y, experts = X, model = 'BOA', loss.type = 'square', quiet = TRUE)

# These predictions are equivalent:
identical(m1.BOA, m2.BOA)  # TRUE
identical(m1.BOA, m3.BOA)  # TRUE

# Display the results
summary(m3.BOA)
if(interactive()){
  plot(m1.BOA)
}
