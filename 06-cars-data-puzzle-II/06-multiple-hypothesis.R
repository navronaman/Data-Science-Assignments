# Imports
install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest@main")
library(HypothesisTesting)

colors <- c("violet", "darkblue", "lightblue", "darkgreen", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "beige","brown")

# Loading the dataset
cars <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Cars2022.csv")
head(cars)
summary(cars)
unique(cars$Car)
tapply(cars$Buyer_Age, cars$Car, mean)
barplot(tapply(cars$Buyer_Age, cars$Car, mean), main="Mean Buyer Age of Various Brands", xlab="Brand Name", ylab="Mean Buyer Age", col=colors)

cat("We can observe from the tapply that the brand 'Ram' has the highest mean buyer age.")
cat("We need obtain a new significance level for our multiple hypothesis test.")

cars_count <- length(unique(cars$Car))
cars_count #11
new_lvl <- (0.05)/(cars_count-1) #0.05/10
new_lvl #0.005 or 0.5%

permutation_test(cars, "Car", "Buyer_Age", 10000, "Chevrolet", "Ram")   #8e-04
permutation_test(cars, "Car", "Buyer_Age", 10000, "Nissan", "Ram")      #0.4514
permutation_test(cars, "Car", "Buyer_Age", 10000, "Kia", "Ram")         #0.3905
permutation_test(cars, "Car", "Buyer_Age", 10000, "Jeep", "Ram")        #0.0136
permutation_test(cars, "Car", "Buyer_Age", 10000, "Subaru", "Ram")      #0.1083
permutation_test(cars, "Car", "Buyer_Age", 10000, "Ford", "Ram")        #0.0036
permutation_test(cars, "Car", "Buyer_Age", 10000, "Toyota", "Ram")      #0.3561
permutation_test(cars, "Car", "Buyer_Age", 10000, "Honda", "Ram")       #0.0655
permutation_test(cars, "Car", "Buyer_Age", 10000, "Hyundai", "Ram")     #0.0121
permutation_test(cars, "Car", "Buyer_Age", 10000, "GMC", "Ram")         #0.1745
