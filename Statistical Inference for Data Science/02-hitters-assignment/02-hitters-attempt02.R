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
        hittersAE$Salary, hittersAW$Salary)

#Question 1
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
plot(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW, main = 'NW with NW, lars')
cor(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW)
cor(as.matrix(XNW)%*%larsNW$beta[19,] + larsNW$mu, YNW)^2
#0.775, r-squared = 0.600
lmNWBase <- cbind(XNW, YNW)
lmNW <- lm(YNW~., data = lmNWBase)
plot(predict(lmNW, XNW), YNW)

#NE
plot(as.matrix(XNE)%*%larsNE$beta[19,] + larsNE$mu, YNE, main = "NE with NE, lars")
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
plot(predict(lmAW, XAW), YAW, main = 'AW with AW, lm')

#AE 
plot(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)
cor(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)
cor(as.matrix(XAE)%*%larsAE$beta[19,] + larsAE$mu, YAE)^2
#0.889, r-squared = 0.790
lmAEBase <- cbind(XAE, YAE)
lmAE <- lm(YAE~., data = lmAEBase)
plot(predict(lmAE, XAE), YAE, main = 'AE with AE, lm')

#Anything below this line was my work, you can delete it. 

#Predicting AW with other lm models
plot(predict(lmAE, XAW), YAW, main = 'AW with lmAE')
lmAWwAE <- predict(lmAE, XAW)

plot(predict(lmNW, XAW), YAW, main = 'AW with lmNW')
lmAWwNW <- predict(lmNW, XAW)

plot(predict(lmNE, XAW), YAW, main = 'AW with lmNE')
lmAWwNE <- predict(lmNE, XAW)

#Predicting AE with other lm models 
plot(predict(lmAW, XAE), YAE, main = 'AE with lmAW')
lmAEwAW <- predict(lmAW, XAE)

plot(predict(lmNW, XAE), YAE, main = 'AE with lmNW')
lmAEwNW <- predict(lmNW, XAE)

plot(predict(lmNE, XAE), YAE, main = 'AE with lmNE')
lmAEwNE <- predict(lmNE, XAE)

#Predicting NE with other lm models
plot(predict(lmAE, XNE), YNE, main = 'NE with lmAE')
lmNEwAE <- predict(lmAE, XNE)

plot(predict(lmAW, XNE), YNE, main = "NE with lmAW")
lmNEwAW <- predict(lmAW, XNE)

plot(predict(lmNE, XNE), YNE, main = "NE with lmNE")

plot(predict(lmNW, XNE), YNE, main = "NE with lmNW")
lmNEwNW <- predict(lmNW, XNE)

#Predicting NW with other lm models

plot(predict(lmAE, XNW), YNW, main = 'NW with lmAE')
lmNWwAE <- predict(lmAE, XNW)

plot(predict(lmAW, XNW), YNW, main = "NW with lmAW")
lmNWwAW <- predict(lmAW, XNW)

plot(predict(lmNE, XNE), YNW, main = "NW with lmNE")
lmNWwNE <- predict(lmNe, XNW)

plot(predict(lmNW, XNW), YNW, main = "NE with lmNW")


#Predicting AW with other lars models
larsAWwAE <- predict(larsAE, XAW)
larsAWwNW <- predict(larsNW, XAW)
larsAWwNE <- predict(larsNE, XAW)

#Predicting AE with other lars models 
larsAEwAW <- predict(larsAW, XAE)
larsAEwNW <- predict(larsNW, XAE)
larsAEwNE <- predict(larsNW, XAE)

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

#AE
AE.AE <- as.matrix(XAE)%*%larsAE$beta[19,]
AE.AW <- as.matrix(XAE)%*%larsAW$beta[5,]
AE.NE <- as.matrix(XAE)%*%larsNE$beta[9,]
AE.NW <- as.matrix(XAE)%*%larsNW$beta[17,]
#normalize means
AE.AE <- AE.AE - mean(AE.AE) + larsAE$mu 
AE.AW <- AE.AW - mean(AE.AW) + larsAW$mu
AE.NE <- AE.NE - mean(AE.NE) + larsNE$mu
AE.NW <- AE.NW - mean(AE.NW) + larsNW$mu
boxplot(hittersAE$Salary, c(as.matrix(AE.AE)), c(as.matrix(AE.AW)),
        c(as.matrix(AE.NW)), c(as.matrix(AE.NE)),
        main = 'AE compared to All, lars', col = colors)
larsAELegend = c('Actual Salary', 'AE with AE', 'AE with AW',
                 'AE with NW', 'AE with NE')
legend('topright', legend = larsAELegend, fill = colors, border = 'black', cex = 0.36)

#AW
AW.AE <- as.matrix(XAW)%*%larsAE$beta[19,]
AW.AW <- as.matrix(XAW)%*%larsAW$beta[5,]
AW.NE <- as.matrix(XAW)%*%larsNE$beta[9,]
AW.NW <- as.matrix(XAW)%*%larsNW$beta[17,]
#normalize means 
AW.AE <- AW.AE - mean(AW.AE) + larsAE$mu
AW.AW <- AW.AW - mean(AW.AW) + larsAW$mu
AW.NW <- AW.NW - mean(AW.NW) + larsNW$mu
AW.NE <- AW.NE - mean(AW.NE) + larsNE$mu 
boxplot(hittersAE$Salary, c(as.matrix(AW.AE)), c(as.matrix(AW.AW)),
        c(as.matrix(AW.NW)), c(as.matrix(AW.NE)),
        main = 'AW compared to All, lars', col = colors)
larsAWLegend = c('Actual Salary', 'AW with AE', 'AW with AW',
                 'AW with NW', 'AW with NE')
legend('topright', legend = larsAWLegend, fill = colors, border = 'black', cex = 0.5)

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
larsNELegend = c('Actual Salary (NE)', 'NW with NW','NW with AE', 'NW with AW',
                 'NW with NE')
legend('topright', legend = larsNELegend, fill = colors_vector, border = 'black', cex = 1)


#lm 

## Li
#AE
lmAE.AE <- predict(lmAE)
boxplot(hittersAE$Salary, lmAE.AE, lmAEwAW, lmAEwNW, lmAEwNE, 
        main = 'AE compared with all, lm', col = colors)
lmAELegend = c('Actual Salary', 'AE with AE', 'AE with AW',
                 'AE with NW', 'AE with NE')
legend('topright', legend = lmAELegend, fill = colors, border = 'black', cex = 0.5)


#AW
lmAW.AW <- predict(lmAW)
boxplot(hittersAW$Salary, lmAW.AW, lmAWwAE, lmAWwNW, lmAWwNE, 
        main = 'AW compared with all, lm', col = colors)
lmAWLegend = c('Actual Salary', 'AW with AE', 'AW with AW',
                 'AW with NW', 'AW with NE')
legend('topright', legend = lmAWLegend, fill = colors, border = 'black', cex = 0.5)
## Li

#NE
lmNE.NE <- predict(lmNE)
boxplot(hittersNE$Salary, lmNE.NE, lmNEwAE, lmNEwNW, lmNEwNE, 
        main = 'NE compared with all, lm', col = colors)
lmAWLegend = c('Actual Salary (NE)', 'AW with AE', 'AW with AW',
               'AW with NW', 'AW with NE')
legend('topright', legend = lmAWLegend, fill = colors, border = 'black', cex = 0.5)



#scatters 

#AE
#lars
plot(as.matrix(AE.AE), hittersAE$Salary, main = 'AE with AE, lars,',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(AE.AW), hittersAE$Salary, main = 'AE with AW, lars,',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(AE.NE), hittersAE$Salary, main = 'AE with NE, lars,',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(AE.NW), hittersAE$Salary, main = 'AE with NW, lars,',
     xlab = 'lars', ylab = 'Actual')
#lm
plot(as.matrix(lmAE.AE), hittersAE$Salary, main = 'AE with AE, lm', 
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmAEwAW), hittersAE$Salary, main = 'AE with AW, lm', 
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmAEwNE), hittersAE$Salary, main = 'AE with NE, lm', 
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmAEwNW), hittersAE$Salary, main = 'AE with NW, lm', 
     xlab = 'lm', ylab = 'Actual')

#AW
#lars
plot(as.matrix(AW.AE), hittersAW$Salary, main = 'AW with AE, lars,',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(AW.AW), hittersAW$Salary, main = 'AW with AW, lars,',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(AW.NE), hittersAW$Salary, main = 'AW with NE, lars,',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(AW.NW), hittersAW$Salary, main = 'AW with NW, lars,',
     xlab = 'lars', ylab = 'Actual')
#lm 
plot(as.matrix(lmAW.AE), hittersAE$Salary, main = 'AW with AE, lm', 
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmAWwAW), hittersAE$Salary, main = 'AW with AW, lm', 
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmAWwNE), hittersAE$Salary, main = 'AW with NE, lm', 
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmAWwNW), hittersAE$Salary, main = 'AW with NW, lm', 
     xlab = 'lm', ylab = 'Actual')

# NE
# lars
plot(as.matrix(NE.AE), hittersNE$Salary, main = 'NE with AE, lars',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(NE.AW), hittersNE$Salary, main = 'NE with AW, lars',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(NE.NE), hittersNE$Salary, main = 'NE with NE, lars',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(NE.NW), hittersNE$Salary, main = 'NE with NW, lars',
     xlab = 'lars', ylab = 'Actual')

# lm
plot(as.matrix(lmNE.AE), hittersNE$Salary, main = 'NE with AE, lm',
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmNE.AW), hittersNE$Salary, main = 'NE with AW, lm',
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmNE.NE), hittersNE$Salary, main = 'NE with NE, lm',
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmNE.NW), hittersNE$Salary, main = 'NE with NW, lm',
     xlab = 'lm', ylab = 'Actual')

# NW
# lars
plot(as.matrix(NW.AE), hittersNW$Salary, main = 'NW with AE, lars',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(NW.AW), hittersNW$Salary, main = 'NW with AW, lars',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(NW.NE), hittersNW$Salary, main = 'NW with NE, lars',
     xlab = 'lars', ylab = 'Actual')
plot(as.matrix(NW.NW), hittersNW$Salary, main = 'NW with NW, lars',
     xlab = 'lars', ylab = 'Actual')

# lm
plot(as.matrix(lmNW.AE), hittersNW$Salary, main = 'NW with AE, lm',
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmNW.AW), hittersNW$Salary, main = 'NW with AW, lm',
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmNW.NE), hittersNW$Salary, main = 'NW with NE, lm',
     xlab = 'lm', ylab = 'Actual')
plot(as.matrix(lmNW.NW), hittersNW$Salary, main = 'NW with NW, lm',
     xlab = 'lm', ylab = 'Actual')
