library('moderndive')
library('tidyverse')
library('skimr')
source('fixregday1.pck')

source("regday1.pck")
regday1.pck

weather <- NOAAGISSWD
#Assignment 3
#drought

DroughtLogit.year <- glm(Drought.Count~Year, family = binomial (link = logit),
                          data = NOAAGISSWD)
DroughtLogit.temp <- glm(Drought.Count~delta.temp, family = binomial (link = logit),
                          data = NOAAGISSWD)
DroughtLogit.yearAndtemp <- glm(Drought.Count~Year + delta.temp, family = binomial (link = logit),
                                 data = NOAAGISSWD)
DroughtLogit.yearWithtemp <- glm(Drought.Count~Year * delta.temp, family = binomial (link = logit),
                                  data = NOAAGISSWD)

#taking a look at variables in model 
summary(DroughtLogit.year)
#AIC == 51.153
#Null deviance: 53.413 on 43 degrees
#Residual deviance: 47.153 on 42 degrees
summary(DroughtLogit.temp)
#AIC: 51.335
#Null deviance: 53.413 on 43 degrees
#Residual deviance: 47.335 on 42 degrees
summary(DroughtLogit.yearAndtemp)
#AIC: 52.995
#Null deviance: 53.413 on 43 degrees
#Residual deviance: 46.995 on 41 degrees
summary(DroughtLogit.yearWithtemp)
#AIC: 52.995
#Null deviance: 53.413 o 43 degrees
#Residual deviance: 46.995 on 42 degrees 

pred1 <- predict(wildDroughtLogit.temp, NOAAGISSWD)
pred2 <- predict(wildDroughtLogit.year, NOAAGISSWD)
pred3 <- predict(wildDroughtLogit.yearAndtemp, NOAAGISSWD)
pred4 <- predict(wildDroughtLogit.yearWithtemp, NOAAGISSWD)

#plotting the predictions
plot(pred1, NOAAGISSWD$Drought.Count, main = "Predictions vs. Actuals (Temp)", xlab = "Predictions", ylab = "Actuals")

plot(pred2, NOAAGISSWD$Drought.Count, main = "Predictions vs. Actuals (Year)", xlab = "Predictions", ylab = "Actuals")

plot(pred3, NOAAGISSWD$Drought.Count, main = "Predictions vs. Actuals (Year+Temp)", xlab = "Predictions", ylab = "Actuals")

plot(pred4, NOAAGISSWD$Drought.Count, main = "Predictions vs. Actuals (Year*Temp)", xlab = "Predictions", ylab = "Actuals")
