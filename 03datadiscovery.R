setwd("A:/Jeevan/Rutgers 2023-2024/Classes/DATA101/Assignment 3")
getwd()
cars <- read.csv("Cars2023c-1.csv")

head(cars)
summary(cars)
unique(cars$Dealership)
unique(cars$Season)
unique(cars$Car)
unique(cars$Buyer)

mean_buyer_income = mean(cars$Buyer_Income)
mean_buyer_income ## 170193.6

m1 = mean(cars[cars$Season == "Spring" & cars$Dealership=="Austin", ]$Buyer_Income) ## 167185.4
m2 = mean(cars[cars$Dealership == "Philladephia" & cars$Car=="Ford", ]$Buyer_Income) ## 173409

q1 = mean_buyer_income - m1
q1
## 3008.193

q2 = m2 - mean_buyer_income
q2
## 3215.346