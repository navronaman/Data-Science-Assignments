getwd()
phones <- read.csv("smartphone_cleaned_v5.csv")

# Exploring the Data
head(phones)
summary(phones)
nrow(phones)

colnames(phones)
unique(phones$brand_name)

tapply(phones$num_rear_cameras, phones$has_5g, mean)

tapply(phones$refresh_rate, phones$os, mean)

# Adding a colors vector for all future graph use
colors <- c("violet", "darkblue", "lightblue", "darkgreen", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "beige","brown")

# Removing vertu as it an outlier
phone_prices <- tapply(phones[phones$brand_name!="vertu" & (phones$os=="android" | phones$os=="ios"), ]$internal_memory, phones[phones$brand_name!="vertu" & (phones$os=="android" | phones$os=="ios"), ]$brand, mean)
# Barplot of mean internal memory of different brands
barplot(phone_prices, main="Mean Internal Memory of Various Brands", xlab="Brand Name", ylab="Mean Internal Memory", col=colors)

# Exploring Scatter Plots

plot(phones[phones$price>2000 & phones$price<40000 & phones$brand_name!="vertu", ]$price, phones[phones$price>2000 & phones$price<40000 & phones$brand_name!="vertu", ]$rating, main="Correlation between rating and price", xlab="Price", ylab="Rating", col="red")

plot(phones[phones$price>2000 & phones$price<40000 & phones$brand_name!="vertu", ]$price, phones[phones$price>2000 & phones$price<40000 & phones$brand_name!="vertu", ]$refresh_rate, main="Correlation between price and refresh rate", xlab="Price", ylab="Refesh Rate")

plot(phones[phones$price>2000 & phones$price<40000 , ]$price, phones[phones$price>2000 & phones$price<40000 , ]$num_rear_cameras, main="Correlation between price and number of rear cameras", xlab="Price", ylab="No of rear cameras", col="purple")

plot(phones[phones$price>2000 & phones$price<40000 & phones$battery_capacity<10000, ]$price, phones[phones$price>2000 & phones$battery_capacity<10000 & phones$price<40000, ]$battery_capacity, main="Correlation between price and battery capacity", xlab="Price", ylab="Battery capacity")

# Exploring box plots
boxplot(price~processor_brand,data=phones[phones$price>2000 & phones$price<40000 & phones$battery_capacity<10000, ], xlab="Processor Brand",ylab="Price", main="Boxplot of processor brand vs price",col=colors,border="black")

boxplot(price~brand_name,data=phones[phones$price>2000 & phones$price<40000, ], xlab="Brand",ylab="Price", main="Boxplot of brand vs price",col=colors,border="black")

#Boxplot of Price vs OS
boxplot(price~os,data=phones[phones$price>2000 & phones$price<40000 & (phones$os=="ios" | phones$os=="android" | phones$os=="other"), ], xlab="OS",ylab="Price", main="Boxplot of Operating System vs Price",col=c("violet", "cyan", "lightgreen"),border="black")

#Boxplot of Proccessor Speed vs OS
boxplot(processor_speed~os,data=phones[phones$price>2000 & phones$price<40000 & (phones$os=="ios" | phones$os=="android" | phones$os=="other"), ], xlab="OS",ylab="Processor Speed", main="Boxplot of OS vs Processor Speed",col=colors,border="black")

boxplot(battery_capacity~has_5g,data=phones[phones$price>2000 & phones$price<40000 & phones$battery_capacity<10000, ], xlab="Does the phone have 5G?",ylab="Battery Capacity", main="Boxplot of 5G vs Battery",col=colors,border="black")

boxplot(price~has_5g,data=phones[phones$price>2000 & phones$price<40000 & phones$battery_capacity<10000, ], xlab="Does the phone have 5G?",ylab="Price", main="Boxplot of 5G vs Price",col=c("cyan", "pink"),border="black")

boxplot(price~has_nfc,data=phones[phones$price>2000 & phones$price<40000 & phones$battery_capacity<10000, ], xlab="Does the phone have NFC?",ylab="Price", main="Boxplot of NFC vs Price",col=c("cyan", "pink"),border="black")

boxplot(price~has_ir_blaster,data=phones[phones$price>2000 & phones$price<40000 & phones$battery_capacity<10000, ], xlab="Does the phone have Infrared Blaster?",ylab="Price", main="Boxplot of Infrared Blaster vs Price",col=c("cyan", "pink"),border="black")

boxplot(rating~has_5g,data=phones[phones$price>2000 & phones$price<40000 & phones$battery_capacity<10000, ], xlab="Does the phone have 5G?",ylab="Ratings", main="Boxplot of 5G vs Ratings",col=c("cyan", "pink"),border="black")

#Explorting Mosaic Plots
mosaicplot(phones$has_5g~phones$has_nfc,xlab = 'Does the phone have 5G?',ylab = 'Does the phone have NFC?', main = "Mosiac of 5G vs NFC",col=colors,border="black")

mosaicplot(phones[phones$os=="android" | phones$os=="ios" | phones$os=="other", ]$os~phones[phones$os=="android" | phones$os=="ios" | phones$os=="other", ]$has_5g,xlab = 'OS',ylab = 'Does the phone have 5G?', main = "Mosiac of 5G vs OS",col=colors,border="black")

mosaicplot(phones$processor_brand~phones$has_5g,xlab = 'Processor Brand',ylab = 'Does the phone have 5G?', main = "Mosiac of Processor Brand vs 5G",col=c("cyan", "lightgreen", "yellow"),border="black")

mosaicplot(phones$processor_brand~phones$has_nfc,xlab = 'Processor Brand',ylab = 'Does the phone have NFC?', main = "Mosiac of Processor Brand vs NFC",col=c("pink", "orange"),border="black")

mosaicplot(phones$processor_brand~phones$has_ir_blaster,xlab = 'Processor Brand',ylab = 'Does the phone have an Infrared Blaster?', main = "Mosiac of Processor Brand vs Infrared Blaster",col=c("blue", "red"),border="black")

