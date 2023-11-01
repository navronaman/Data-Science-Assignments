setwd("A:/Jeevan/Rutgers 2023-2024/Classes/DATA101/Assignment 3")
getwd()
cars <- read.csv("Cars2023c-1.csv")

head(cars)
summary(cars)
unique(cars$Dealership)
unique(cars$Season)
unique(cars$Car)
unique(cars$Buyer)

colors<- c('red','blue','cyan','yellow','green', 'orange', 'pink', 'purple', 'seagreen2', 'khaki', 'lightslategray')

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

round(table(cars[cars$Season == "Spring" & cars$Dealership=="Austin", ]$Car)/nrow(cars[cars$Season == "Spring" & cars$Dealership=="Austin", ]), 4)
barplot(table(cars[cars$Season == "Spring" & cars$Dealership=="Austin", ]$Car)/nrow(cars[cars$Season == "Spring" & cars$Dealership=="Austin", ]), xlab = "Car", ylab = "Trends", col=colors, main = "Trends of buying a car for Austin in Spring")


round(table(cars[cars$Dealership == "Philladephia" & cars$Car=="Ford", ]$Season)/nrow(cars[cars$Dealership == "Philladephia" & cars$Car=="Ford", ]), 4)
barplot(table(cars[cars$Dealership == "Philladephia" & cars$Car=="Ford", ]$Season)/nrow(cars[cars$Dealership == "Philladephia" & cars$Car=="Ford", ]), xlab = "Season", ylab = "Trends", col=colors, main = "Trends of buying a Ford in Philly accross the seasons")


## splitting the data
cars1 <- cars[1:25000, ]
cars2 <- cars[25000:50000, ]

## examining queries in cars1
mean(cars1$Buyer_Income) # 170189.6
q1_forcars1 = mean(cars1$Buyer_Income) - mean(cars1[cars1$Season == "Spring" & cars1$Dealership=="Austin", ]$Buyer_Income)
q1_forcars1 # 4249.692

q2_forcars1 = mean(cars1[cars1$Dealership == "Philladephia" & cars1$Car=="Ford", ]$Buyer_Income) - mean(cars1$Buyer_Income)
q2_forcars1 # 4869.5

round(table(cars1[cars1$Season == "Spring" & cars1$Dealership=="Austin", ]$Car)/nrow(cars1[cars1$Season == "Spring" & cars1$Dealership=="Austin", ]), 4)
barplot(table(cars1[cars1$Season == "Spring" & cars1$Dealership=="Austin", ]$Car)/nrow(cars1[cars1$Season == "Spring" & cars1$Dealership=="Austin", ]), xlab = "Car", ylab = "Trends", col=colors, main = "Trends of buying a car for Austin in Spring [1]")


round(table(cars1[cars1$Dealership == "Philladephia" & cars1$Car=="Ford", ]$Season)/nrow(cars1[cars1$Dealership == "Philladephia" & cars1$Car=="Ford", ]), 4)
barplot(table(cars1[cars1$Dealership == "Philladephia" & cars1$Car=="Ford", ]$Season)/nrow(cars1[cars1$Dealership == "Philladephia" & cars1$Car=="Ford", ]), xlab = "Season", ylab = "Trends", col=colors, main = "Trends of buying a Ford in Philly accross the seasons [1]")


## examining q2 in cars2
mean(cars2$Buyer_Income) # 170194.5
q1_forcars2 = mean(cars2$Buyer_Income) - mean(cars2[cars2$Season == "Spring" & cars2$Dealership=="Austin", ]$Buyer_Income)
q1_forcars2 # 1747.116

q2_forcars2 = mean(cars2[cars2$Dealership == "Philladephia" & cars2$Car=="Ford", ]$Buyer_Income) - mean(cars2$Buyer_Income)
q2_forcars2 # 1344.952

round(table(cars2[cars2$Season == "Spring" & cars2$Dealership=="Austin", ]$Car)/nrow(cars2[cars2$Season == "Spring" & cars1$Dealership=="Austin", ]), 4)
barplot(table(cars2[cars2$Season == "Spring" & cars2$Dealership=="Austin", ]$Car)/nrow(cars2[cars2$Season == "Spring" & cars2$Dealership=="Austin", ]), xlab = "Car", ylab = "Trends", col=colors, main = "Trends of buying a car for Austin in Spring [2]")

round(table(cars2[cars2$Dealership == "Philladephia" & cars2$Car=="Ford", ]$Season)/nrow(cars2[cars2$Dealership == "Philladephia" & cars2$Car=="Ford", ]), 4)
barplot(table(cars2[cars2$Dealership == "Philladephia" & cars2$Car=="Ford", ]$Season)/nrow(cars2[cars2$Dealership == "Philladephia" & cars2$Car=="Ford", ]), xlab = "Season", ylab = "Trends", col=colors, main = "Trends of buying a Ford in Philly accross the seasons [2]")


cat("We see there is a high discrepancy in the mean average buyer income for a car buyer in Austin during Spring for cars1 and cars2. ")
cat("We see there is a high discrepancy in the mean average buyer income for a car buyer in Philly buying Ford for cars1 and cars2. ")

## let's split the data even more

cars01 <- cars1[1:12500, ]
cars02 <- cars1[12501:25000, ]
cars03 <- cars2[1:12500, ]
cars04 <- cars2[12501:25000, ]

## examining queries in cars01
mean(cars01$Buyer_Income) 
q1_forcars01 = mean(cars01$Buyer_Income) - mean(cars01[cars01$Season == "Spring" & cars01$Dealership=="Austin", ]$Buyer_Income)
q1_forcars01 

q2_forcars01 = mean(cars01[cars01$Dealership == "Philladephia" & cars01$Car=="Ford", ]$Buyer_Income) - mean(cars01$Buyer_Income)
q2_forcars01 

## examining queries in cars02
mean(cars01$Buyer_Income) 
q1_forcars02 = mean(cars02$Buyer_Income) - mean(cars02[cars02$Season == "Spring" & cars02$Dealership == "Austin", ]$Buyer_Income)
q1_forcars02 

q2_forcars02 = mean(cars02[cars02$Dealership == "Philladephia" & cars02$Car == "Ford", ]$Buyer_Income) - mean(cars02$Buyer_Income)
q2_forcars02 

## examining queries in cars03
mean(cars03$Buyer_Income) 
q1_forcars03 = mean(cars03$Buyer_Income) - mean(cars03[cars03$Season == "Spring" & cars03$Dealership == "Austin", ]$Buyer_Income)
q1_forcars03 

q2_forcars03 = mean(cars03[cars03$Dealership == "Philladephia" & cars03$Car == "Ford", ]$Buyer_Income) - mean(cars03$Buyer_Income)
q2_forcars03 

## examining queries in cars04
mean(cars04$Buyer_Income) 
q1_forcars04 = mean(cars04$Buyer_Income) - mean(cars04[cars04$Season == "Spring" & cars04$Dealership == "Austin", ]$Buyer_Income)
q1_forcars04 

q2_forcars04 = mean(cars04[cars04$Dealership == "Philladephia" & cars04$Car == "Ford", ]$Buyer_Income) - mean(cars04$Buyer_Income)
q2_forcars04 

