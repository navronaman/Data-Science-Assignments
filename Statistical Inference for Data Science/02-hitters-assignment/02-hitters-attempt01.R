library("ISLR")
library("tidyverse")
library('moderndive')
library('skimr')
library('lars')

# AE, AW, NE, NW

#setting up data sets, removing NA values, and subsetting 
hitters <- Hitters
noNAHitter <- na.omit(hitters)

unique(hitters$NewLeague)
unique(hitters$Division)
hittersN <- noNAHitter[noNAHitter$League == 'N',]
hittersA <- noNAHitter[noNAHitter$League == 'A',]
hittersNE <- hittersN[hittersN$Division == 'E',]
hittersNW <- hittersN[hittersN$Division == 'W',]
hittersAE <- hittersA[hittersA$Division == 'E',]
hittersAW <- hittersA[hittersA$Division == 'W',]

boxplot(hittersNE$Salary, hittersNW$Salary, 
        hittersAE$Salary, hittersAW$Salary)

#Question 1
#The median for the NW, NE, and AW leagues are effectively the same.
#The AE league has a lower median, however. 
#All leagues have effectively the same interquartile range,
#with AW having the highest upper quartile and NE having the lowest. 

XNW <- hittersNW[, -c(20, 19, 14, 15)]
YNW <- hittersNW$Salary
larsNW <- lars(as.matrix(XNW), YNW)
larsNW 
min(larsNW$Cp)
larsNW$Cp == min(larsNW$Cp)
#16, index 17
larsNW$beta[17,]
larsNW$Cp[17]
#AtBat, Hits, HmRuns, Walks, and Years are positive
#0 = CAtBat, PutOuts, CRBI, CRuns
#<0 = CWalks, Assists, Runs 

XNE <- hittersNE[, -c(20, 19, 14, 15)]
YNE <- hittersNE$Salary
larsNE <- lars(as.matrix(XNE), YNE)
larsNE
min(larsNE$Cp)
larsNE$Cp == min(larsNE$Cp)
#8, index 9
larsNE$beta[9,]
larsNE$Cp[9]
#Walks mostly, PutOuts, CRBI and Assists minorly
#<0 AtBat, Errors
#0 = everything else 

#AW
XAW <- hittersAW[, -c(20, 19, 14, 15)]
YAW <- hittersAW$Salary
larsAW <- lars(as.matrix(XAW), YAW)
larsAW
min(larsAW$Cp)
larsAW$Cp == min(larsAW$Cp)
#22, so index 23
larsAW$beta[23,]
larsAW$Cp[23]
#Hits, Walks, and CRuns are standouts for predicting salary
#0 = HmRunes, Runs, CHmRuns, CHits
#<0 = RBI, AtBat, Years, Errors 

#AE
XAE <- hittersAE[, -c(20, 19, 14, 15)]
YAE <- hittersAE$Salary
larsAE <- lars(as.matrix(XAE), YAE)
larsAE
min(larsAE$Cp)
larsAE$Cp == min(larsAE$Cp)
#18, so index 19
larsAE$beta[19,]
larsAE$Cp[19]
#So CRBI matters a bit, and so do CRuns and Walks.
#0 = Error, RBI, Years, CHits 
#<0 = AtBat, -1.1 CWalks 

#Question 2
#Between the AE and AW regions, the salary of a player is 
#affected by different variables. 
#In the AW region, Hits, Walks, and CRuns are positively
#correlated, while Years, AtBats, RBI, and Errors are
#negatively correlated. 
#Comparitively, AE has CRBI, CRuns, and Walks positively
#correlated to salary, and AtBats and CWalks as negatively
#correlated to salary. 

#NW
plot(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW)
cor(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW)
cor(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW)^2
#0.775, r-squared = 0.600
lmNWBase <- cbind(XNW, YNW)
lmNW <- lm(YNW~., data = lmNWBase)
plot(predict(lmNW, XNW), YNW)

#NE
plot(as.matrix(XNE)%*%larsNE$beta[19,] + larsNE$mu, YNE)
cor(as.matrix(XNE)%*%larsNE$beta[19,] + larsNE$mu, YNE)
cor(as.matrix(XNE)%*%larsNE$beta[19,] + larsNE$mu, YNE)^2
#0.725, r-squared = 0.525
lmNEBase <- cbind(XNE, YNE)
lmNE <- lm(YNE~., data = lmNEBase)
plot(predict(lmNE, XNE), YNE)

#AW
plot(as.matrix(XAW)%*%larsAW$beta[19,] + larsAW$mu, YAW)
cor(as.matrix(XAW)%*%larsAW$beta[19,] + larsAW$mu, YAW)
cor(as.matrix(XAW)%*%larsAW$beta[19,] + larsAW$mu, YAW)^2
#0.853, r-squared = 0.727
lmAWBase <- cbind(XAW, YAW)
lmAW <- lm(YAW~., data = lmAWBase)
plot(predict(lmAW, XAW), YAW)

#AE 
plot(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)
cor(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)
cor(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)^2
#0.889, r-squared = 0.790
lmAEBase <- cbind(XAE, YAE)
lmAE <- lm(YAE~., data = lmAEBase)
plot(predict(lmAE, XAE), YAE)

#Predicting AW with other lm models
plot(predict(lmAE, XAW), YAW, main = 'AW with lmAE')
plot(predict(lmNW, XAW), YAW, main = 'AW with lmNW')
plot(predict(lmNE, XAW), YAW, main = 'AW with lmNE')

#Predicting AE with other lm models 
plot(predict(lmAW, XAE), YAE, main = 'AE with lmAW')
plot(predict(lmNW, XAE), YAE, main = 'AE with lmNW')
plot(predict(lmNE, XAE), YAE, main = 'AE with lmNE')

#Predicting AW with other lars models
plot(predict(larsAE, XAW), YAW, main = 'AW with larsAE')
plot(predict(larsNW, XAW), YAW, main = 'AW with larsNW')
plot(predict(larsNE, XAW), YAW, main = 'AW with larsNE')

#Predicting AE with other lars models 
plot(predict(larsAW, XAE), YAE, main = 'AE with larsAW')
plot(predict(larsNW, XAE), YAE, main = 'AE with larsNW')
plot(predict(larsNE, XAE), YAE, main = 'AE with larsNE')

#Question 3 