install.packages("ModelMetrics")
library(ModelMetrics)

library(rpart)
library(rpart.plot)

income_tr <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/incomeTrain2023.csv')
income_te <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')

income_tr$CollegeLocation <- income_tr$College_location
income_tr$SquareLi <- income_tr$LinkedIN ^ 2
income_te$SquareLi <- income_te$LinkedIN ^ 2

summary(income_tr)
summary(income_te)

unique(income_tr$Major)

# Color Vectors
colors <- c("violet", "darkblue", "lightblue", "darkgreen", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "beige","brown")

colors_vector <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ffff99", "#b15928", "#8dd3c7", "#bebada")

## Barplots

# numerical variables, bar plots
plot(income_tr$GPA, income_tr$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
# some outliers with low GPAs and insane good salaries, but mostly no relationship

plot(income_tr$DOB, income_tr$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)
# also year of birth doesn't have much to do, similar salaries throughout with some outliers

plot(income_tr$Tuition, income_tr$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)
# there is a big gap b/w 6e+4 and 8e+4

plot(log(income_tr$LinkedIN), income_tr$Salary, main="Linkedin and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

plot(income_tr$LinkedIN^2, income_tr$Salary, main="(Linkedin)^2 and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)
# VERY INTERESTING, some relationship in salary and no of connections

## Boxplot

boxplot(Salary~CollegeLocation, data=income_tr, xlab="Location", ylab="Salary", main="Boxplot of Salary vs Location", col=c("khaki", "cyan"))
# except outliers, pretty standard

boxplot(Salary~Major, data=income_tr[income_tr$Major!="Other", ], xlab="Location", ylab="Salary", main="Boxplot of Salary vs Location", col=colors_vector)
# so many outliers in other
# if we remove others, these are the rankings
# 1. Vocational, 2. Professional, 3. Humanities, 4. Business, 5. STEM


# Let's see for each individual major

## Other

income_tr_o <- income_tr[income_tr$Major=="Other", ]

plot(income_tr_o$GPA, income_tr_o$Salary, main="GPA and Salary for Other Majors", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_o$DOB, income_tr_o$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_o$Tuition, income_tr_o$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(income_tr_o$LinkedIN, income_tr_o$Salary, main="No. of LinkedIn Connections and Salary for Other Majors", xlab="LinkedIn", ylab="Salary", col=colors_vector)

plot(income_tr_o$SquareLi, income_tr_o$Salary, main="No. of LinkedIn Squared and Salary for Other Majors", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# No. of LinkedIn connections is the biggest indicator of Salary for Other Majors

## Business
income_tr_bus <- income_tr[income_tr$Major=="Buisness", ]

plot(income_tr_bus$GPA, income_tr_bus$Salary, main="GPA and Salary for Business Majors", xlab="GPA", ylab="Salary", col=colors_vector)
plot(income_tr_bus$DOB, income_tr_bus$Salary, main="Year of Birth and Salary for Business Majors", xlab="GPA", ylab="Salary", col=colors_vector)
# Two distinct salaries
# Let's use RPart for Business


income_tr_bus_odd <- income_tr_bus[income_tr_bus$DOB %% 2 == 0, ]
income_tr_bus_even <- income_tr_bus[income_tr_bus$DOB %% 2 == 1, ]

plot(income_tr_bus_odd$GPA, income_tr_bus_odd$Salary, main="GPA and Salary for Business Majors born in even years", xlab="GPA", ylab="Salary", col=colors_vector)
plot(income_tr_bus_even$GPA, income_tr_bus_even$Salary, main="GPA and Salary for Business Majors born in odd years", xlab="GPA", ylab="Salary", col=colors_vector)


## STEM
income_tr_stem <- income_tr[income_tr$Major=="STEM", ]

plot(income_tr_stem$GPA, income_tr_stem$Salary, main="GPA and Salary for STEM Majors", xlab="GPA", ylab="Salary", col=colors_vector)

plot(1.2^(income_tr_stem$GPA), income_tr_stem$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
plot(sqrt(income_tr_stem$GPA), income_tr_stem$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

income_tr_stem_c <- income_tr_stem[income_tr_stem$CollegeLocation=="WestCoast", ]

income_tr_stem_odd <- income_tr_stem[income_tr_stem$DOB %% 2 == 0, ]

head(income_tr_stem_odd)
head(income_tr_stem_c)

# Plot with STEM majors at West Coast with GPA and Salary
plot(income_tr_stem_c$GPA, income_tr_stem_c$Salary, main="GPA and Salary for STEM Majors", xlab="GPA", ylab="Salary", col=colors_vector)

# Plot with STEM majors born on even years with GPA and Salary
plot(income_tr_stem_odd$GPA, income_tr_stem_odd$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

# Different plots
plot(income_tr_stem_play$GPA, income_tr_stem_play$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_stem$DOB, income_tr_stem$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_stem$Tuition, income_tr_stem$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_stem$LinkedIN), income_tr_stem$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# Best predictor for salaries of STEM Major is their GPA, so we will use Linear Regression


## Vocational
income_tr_v <- income_tr[income_tr$Major=="Vocational",]

plot(income_tr_v$GPA, income_tr_v$Salary, main="GPA and Salary for Vocational Majors", xlab="GPA", ylab="Salary", col=colors_vector)

income_tr_v_c <- income_tr_v[income_tr_v$CollegeLocation=="WestCoast", ]

plot(income_tr_v_c$GPA, income_tr_v_c$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_v$DOB, income_tr_v$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_v$Tuition, income_tr_v$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_v$LinkedIN), income_tr_v$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# Best predictor for salaries of Vocational Major is their GPA, so we will use Linear Regression

## Professional

income_tr_p <- income_tr[income_tr$Major=="Professional", ]

plot(income_tr_p$GPA, income_tr_p$Salary, main="GPA and Salary for Professional Majors", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_p$DOB, income_tr_p$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_p$Tuition, income_tr_p$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_p$LinkedIN), income_tr_p$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# Best predictor for salaries of Professional Major is their GPA, so we will use Linear Regression

## Humanities

income_tr_h <- income_tr[income_tr$Major=="Humanities", ]

plot(income_tr_h$GPA, income_tr_h$Salary, main="GPA and Salary for Humanities Majors", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_h$DOB, income_tr_h$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_h$Tuition, income_tr_h$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_h$LinkedIN), income_tr_h$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# Best predictor for salaries of Humanities Major is their GPA, so we will use Linear Regression


####
# Linear Regression Model for Major=="Other"
model1 <- lm(Salary ~ SquareLi, data = income_tr[income_tr$Major == "Other", ])

# Linear Regression Model for Major=="Buisness" and DOB%%2==0
model201 <- lm(Salary ~ GPA, data=income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 0, ])
model202 <- lm(Salary ~ GPA, data=income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 1, ])

# Linear Regression Models for all other Majors with GPA
model3 <- lm(Salary ~ GPA, data = income_tr[income_tr$Major == "Humanities", ])
model4 <- lm(Salary ~ GPA, data = income_tr[income_tr$Major == "STEM", ])
model5 <- lm(Salary ~ GPA, data = income_tr[income_tr$Major == "Vocational", ])
model6 <- lm(Salary ~ GPA, data = income_tr[income_tr$Major == "Professional", ])

# Decision Tree for all majors
# model3 <- rpart(Salary ~ GPA+CollegeLocation, data = income_tr[income_tr$Major == "Humanities", ], method = "anova")
# model4 <- rpart(Salary ~ GPA+CollegeLocation, data = income_tr[income_tr$Major == "STEM", ], method = "anova")
# model5 <- rpart(Salary ~ GPA+CollegeLocation, data = income_tr[income_tr$Major == "Vocational", ], method = "anova")
# model6 <- rpart(Salary ~ GPA+CollegeLocation, data = income_tr[income_tr$Major == "Professional", ], method = "anova")


# Predictions
pred1 <- predict(model1, newdata = income_tr[income_tr$Major == "Other", ])
pred201 <- predict(model201, newdata = income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 0, ])
pred202 <- predict(model202, newdata = income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 1, ])
pred3 <- predict(model3, newdata = income_tr[income_tr$Major == "Humanities", ])
pred4 <- predict(model4, newdata = income_tr[income_tr$Major == "STEM", ])
pred5 <- predict(model5, newdata = income_tr[income_tr$Major == "Vocational", ])
pred6 <- predict(model6, newdata = income_tr[income_tr$Major == "Professional", ])

# pred707 <- predict(model707, income_tr[income_tr$Major != "Other" & income_tr$Major!="Buisness", ])



# Combine Predictions
decision <- rep(0, nrow(income_tr))
decision[income_tr$Major == "Other"] <- pred1
decision[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 0] <- pred201
decision[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 1] <- pred202
decision[income_tr$Major == "Humanities"] <- pred3
decision[income_tr$Major == "STEM"] <- pred4
decision[income_tr$Major == "Vocational"] <- pred5
decision[income_tr$Major == "Professional"] <- pred6



# Calculate Mean Squared Error
mse <- mean((decision - income_tr$Salary)^2)
mse # 12,170
# 1702.478
# 141
# 89

mean_mse <- mse(income_tr$Salary, decision)
mean_mse
# 89.76286

# Let's write the submission file

pred1_final <- predict(model1, newdata = income_te[income_te$Major == "Other", ])
pred201_final <- predict(model201, newdata = income_te[income_te$Major == "Buisness" & income_tr$DOB%%2 == 0, ])
pred202_final <- predict(model202, newdata = income_te[income_te$Major == "Buisness" & income_tr$DOB%%2 == 1, ])
pred3_final <- predict(model3, newdata = income_te[income_te$Major == "Humanities", ])
pred4_final <- predict(model4, newdata = income_te[income_te$Major == "STEM", ])
pred5_final <- predict(model5, newdata = income_te[income_te$Major == "Vocational", ])
pred6_final <- predict(model6, newdata = income_te[income_te$Major == "Professional", ])

decision_final <- rep(0, nrow(income_te))
decision_final[income_te$Major == "Other"] <- pred1_final
decision_final[income_te$Major == "Buisness" & income_tr$DOB%%2 == 0] <- pred201_final
decision_final[income_te$Major == "Buisness" & income_tr$DOB%%2 == 1] <- pred202_final
decision_final[income_te$Major == "Humanities"] <- pred3_final
decision_final[income_te$Major == "STEM"] <- pred4_final
decision_final[income_te$Major == "Vocational"] <- pred5_final
decision_final[income_te$Major == "Professional"] <- pred6_final

ids<-c(1:nrow(income_te))
submission$ID<-ids

submission$RATING<-decision_final
submission
write.csv(submission, 'A:/Jeevan/Rutgers 2023-2024/Classes/DATA101/Assignment 10/submissionIncome2023.csv', row.names=FALSE)




# PLAN
# lm with squared linkein + salary for OTHER
# lm with odd business with GPA
# lm with even business with GPA
# lm with vocational with GPA
# lm with proffessional with GPA
# lm with humanities with GPA
# lm with STEM with GPA
