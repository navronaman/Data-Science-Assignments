library('moderndive')
library('tidyverse')
library('skimr')
source('fixregday1.pck')

source("regday1.pck")
regday1.pck

weather <- NOAAGISSWD
#Assignment 3
#drought

wildDroughtLogit.year <- glm(Drought.Count~Year, family = binomial (link = logit),
                          data = NOAAGISSWD)
wildDroughtLogit.temp <- glm(Drought.Count~delta.temp, family = binomial (link = logit),
                          data = NOAAGISSWD)
wildDroughtLogit.yearAndtemp <- glm(Drought.Count~Year + delta.temp, family = binomial (link = logit),
                                 data = NOAAGISSWD)
wildDroughtLogit.yearWithtemp <- glm(Drought.Count~Year * delta.temp, family = binomial (link = logit),
                                  data = NOAAGISSWD)

#taking a look at variables in model 
summary(wildDroughtLogit.year)
#AIC == 51.153
#Null deviance: 53.413 on 43 degrees
#Residual deviance: 47.153 on 42 degrees
summary(wildDroughtLogit.temp)
#AIC: 51.335
#Null deviance: 53.413 on 43 degrees
#Residual deviance: 47.335 on 42 degrees
summary(wildDroughtLogit.yearAndtemp)
#AIC: 52.995
#Null deviance: 53.413 on 43 degrees
#Residual deviance: 46.995 on 41 degrees
summary(wildFireLogit.yearWithtemp)
#AIC: 52.995
#Null deviance: 53.413 o 43 degrees
#Residual deviance: 46.995 on 42 degrees 

pred1 <- predict(wildDroughtLogit.temp, NOAAGISSWD)
