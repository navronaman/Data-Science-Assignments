library("ISLR")
library("tidyverse")
library('moderndive')
library('skimr')
library('lars')
colors = c('red', 'blue', 'yellow', 'purple', 'green')
colors_vector <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ffff99", "#b15928", "#8dd3c7", "#bebada")


set.seed(123)

#setting up data sets, removing NA values, and subsetting 
hitters <- Hitters
noNAHitter <- na.omit(hitters)
hittersN <- noNAHitter[noNAHitter$League == 'N',]
hittersA <- noNAHitter[noNAHitter$League == 'A',]
hittersNE <- hittersN[hittersN$Division == 'E',]
hittersNW <- hittersN[hittersN$Division == 'W',]
hittersAE <- hittersA[hittersA$Division == 'E',]
hittersAW <- hittersA[hittersA$Division == 'W',]

boxplot(hittersNE$Salary, hittersNW$Salary, 
        hittersAE$Salary, hittersAW$Salary,
        col=colors_vector, main="Boxplot of divisions salary")

mainBoxLegend = c('NE Salary', 'NW Salary', 'AE Salary', 'AW Salary')
legend('topright', legend = mainBoxLegend, fill = colors, border = 'black', cex = 1)


#Question 1
# Answered by my partner Anthony Li, as stated below:
#The median for the NW, NE, and AW leagues are effectively the same.
#The AE league has a lower median, however. 
#All leagues have effectively the same interquartile range,
#with AW having the highest upper quartile and NE having the lowest. 

#NW
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

#NE
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
#4, so index 5
larsAW$beta[5,]
larsAW$Cp[5]
#Hits, Walks, and CHits are standouts for predicting salary
#0 = Everything else 
#<0 = None 

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
plot(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW, main = 'NW with NW, lars', col="red")
cor(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW)
cor(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW)^2
#0.775, r-squared = 0.600
lmNWBase <- cbind(XNW, YNW)
lmNW <- lm(YNW~., data = lmNWBase)
plot(predict(lmNW, XNW), YNW, main="NW with NW, lm", col="blue")

#NE
plot(as.matrix(XNE)%*%larsNE$beta[19,] + larsNE$mu, YNE, main = "NE with NE, lars", col="red")
cor(as.matrix(XNE)%*%larsNE$beta[19,] + larsNE$mu, YNE)
cor(as.matrix(XNE)%*%larsNE$beta[19,] + larsNE$mu, YNE)^2
#0.725, r-squared = 0.525
lmNEBase <- cbind(XNE, YNE)
lmNE <- lm(YNE~., data = lmNEBase)
plot(predict(lmNE, XNE), YNE, main="NE with NE, lm", col="blue")

#AW
plot(as.matrix(XAW)%*%larsAW$beta[19,] + larsAW$mu, YAW, col="red")
cor(as.matrix(XAW)%*%larsAW$beta[19,] + larsAW$mu, YAW)
cor(as.matrix(XAW)%*%larsAW$beta[19,] + larsAW$mu, YAW)^2
#0.853, r-squared = 0.727
lmAWBase <- cbind(XAW, YAW)
lmAW <- lm(YAW~., data = lmAWBase)
plot(predict(lmAW, XAW), YAW, main = 'AW with AW, lm', col="blue")

#AE 
plot(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE, col="red")
cor(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)
cor(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)^2
#0.889, r-squared = 0.790
lmAEBase <- cbind(XAE, YAE)
lmAE <- lm(YAE~., data = lmAEBase)
plot(predict(lmAE, XAE), YAE, main = 'AE with AE, lm', col="blue")

#Predicting NE with other lm models
plot(predict(lmAE, XNE), YNE, main = 'NE with lmAE', col="blue")
lmNEwAE <- predict(lmAE, XNE)

plot(predict(lmAW, XNE), YNE, main = "NE with lmAW", col="blue")
lmNEwAW <- predict(lmAW, XNE)

plot(predict(lmNE, XNE), YNE, main = "NE with lmNE", col="blue")
lmNEwNE <- predict(lmNE)

plot(predict(lmNW, XNE), YNE, main = "NE with lmNW", col="blue")
lmNEwNW <- predict(lmNW, XNE)

#Predicting NW with other lm models

plot(predict(lmAE, XNW), YNW, main = 'NW with lmAE', col="blue")
lmNWwAE <- predict(lmAE, XNW)

plot(predict(lmAW, XNW), YNW, main = "NW with lmAW", col="blue")
lmNWwAW <- predict(lmAW, XNW)

plot(predict(lmNE, XNW), YNW, main = "NW with lmNE", col="blue")
lmNWwNE <- predict(lmNE, XNW)

plot(predict(lmNW, XNW), YNW, main = "NE with lmNW", col="blue")
lmNWwNW <- predict(lmNW)

#Predicting NE with other lars model
larsNEwAE <- predict(larsAE, XNE)
larsNEwAW <- predict(larsAW, XNE)
larsNEwNW <- predict(larsNW, XNE)

#Predicting NW with other lars model
larsNWwAE <- predict(larsAE, XNW)
larsNWwAW <- predict(larsAW, XNW)
larsNWwNE <- predict(larsNE, XNW)


#boxplotting

#lars

#NE
NE.AE <- as.matrix(XNE)%*%larsAE$beta[19,]
NE.AW <- as.matrix(XNE)%*%larsAW$beta[5,]
NE.NE <- as.matrix(XNE)%*%larsNE$beta[9,]
NE.NW <- as.matrix(XNE)%*%larsNW$beta[17,]

#normalize means 
NE.AE <- NE.AE - mean(NE.AE) + larsAE$mu
NE.AW <- NE.AW - mean(NE.AW) + larsAW$mu
NE.NE <- NE.NE - mean(NE.NE) + larsNE$mu
NE.NW <- NE.NW - mean(NE.NW) + larsNW$mu 
boxplot(hittersNE$Salary, c(as.matrix(NE.NE)), c(as.matrix(NE.AE)),
        c(as.matrix(NE.AW)), c(as.matrix(NE.NW)),
        main = 'NE compared to All, lars', col = colors_vector)
larsNELegend = c('Actual Salary', 'NE with NE','NE with AE', 'NE with AW',
                 'NE with NW')
legend('topright', legend = larsNELegend, fill = colors_vector, border = 'black', cex = 1)


#NW
NW.AE <- as.matrix(XNW)%*%larsAE$beta[19,]
NW.AW <- as.matrix(XNW)%*%larsAW$beta[5,]
NW.NE <- as.matrix(XNW)%*%larsNE$beta[9,]
NW.NW <- as.matrix(XNW)%*%larsNW$beta[17,]

#normalize means 
NW.AE <- NW.AE - mean(NW.AE) + larsAE$mu
NW.AW <- NW.AW - mean(NW.AW) + larsAW$mu
NW.NE <- NW.NE - mean(NW.NE) + larsNE$mu
NW.NW <- NW.NW - mean(NW.NW) + larsNW$mu 
boxplot(hittersNE$Salary, c(as.matrix(NW.NW)), c(as.matrix(NW.AE)),
        c(as.matrix(NW.AW)), c(as.matrix(NW.NE)),
        main = 'NW compared to All, lars', col = colors_vector)
larsNELegend = c('Actual Salary (NW)', 'NW with NW','NW with AE', 'NW with AW',
                 'NW with NE')
legend('topright', legend = larsNELegend, fill = colors_vector, border = 'black', cex = 1)


#lm 

#NE
boxplot(hittersNE$Salary, lmNEwNE, lmNEwAE, lmNEwAW, lmNEwNW, 
        main = 'NE compared with all, lm', col = colors_vector)
lmAWLegend = c('Actual Salary (NE)', 'NE with AE', 'NE with AW',
               'NE with NE', 'NE with NW')
legend('topright', legend = lmAWLegend, fill = colors_vector, border = 'black', cex = 1)


#NE
boxplot(hittersNE$Salary, lmNWwNW, lmNWwAE, lmNWwAW, lmNWwNE, 
        main = 'NE compared with all, lm', col = colors_vector)
lmAWLegend = c('Actual Salary (NW)', 'NW with NW','NW with AE', 'NW with AW','NE with NW')
legend('topright', legend = lmAWLegend, fill = colors_vector, border = 'black', cex = 1)



#scatters for NE and NW

# NE
# lars
plot(as.matrix(NE.AE), hittersNE$Salary, main = 'NE with AE, lars',
     xlab = 'lars', ylab = 'Actual', col="red")
plot(as.matrix(NE.AW), hittersNE$Salary, main = 'NE with AW, lars',
     xlab = 'lars', ylab = 'Actual', col="red")
plot(as.matrix(NE.NE), hittersNE$Salary, main = 'NE with NE, lars',
     xlab = 'lars', ylab = 'Actual', col="red")
plot(as.matrix(NE.NW), hittersNE$Salary, main = 'NE with NW, lars',
     xlab = 'lars', ylab = 'Actual', col="red")

# lm
plot(as.matrix(lmNEwAE), hittersNE$Salary, main = 'NE with AE, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")
plot(as.matrix(lmNEwAW), hittersNE$Salary, main = 'NE with AW, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")
plot(as.matrix(lmNEwNE), hittersNE$Salary, main = 'NE with NE, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")
plot(as.matrix(lmNEwNW), hittersNE$Salary, main = 'NE with NW, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")

# NW
# lars
plot(as.matrix(NW.AE), hittersNW$Salary, main = 'NW with AE, lars',
     xlab = 'lars', ylab = 'Actual', col="red")
plot(as.matrix(NW.AW), hittersNW$Salary, main = 'NW with AW, lars',
     xlab = 'lars', ylab = 'Actual', col="red")
plot(as.matrix(NW.NE), hittersNW$Salary, main = 'NW with NE, lars',
     xlab = 'lars', ylab = 'Actual', col="red")
plot(as.matrix(NW.NW), hittersNW$Salary, main = 'NW with NW, lars',
     xlab = 'lars', ylab = 'Actual', col="red")

# lm
plot(as.matrix(lmNWwAE), hittersNW$Salary, main = 'NW with AE, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")
plot(as.matrix(lmNWwAW), hittersNW$Salary, main = 'NW with AW, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")
plot(as.matrix(lmNWwNE), hittersNW$Salary, main = 'NW with NE, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")
plot(as.matrix(lmNWwNW), hittersNW$Salary, main = 'NW with NW, lm',
     xlab = 'lm', ylab = 'Actual', col="blue")

