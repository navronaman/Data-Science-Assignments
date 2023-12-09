library(rpart)
library(rpart.plot)

income_tr <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/incomeTrain2023.csv')
income_te <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')

income_tr$CollegeLocation <- income_tr$College_location
income_tr$SquareLi <- income_tr$LinkedIN ^ 2

# Color Vectors
colors <- c("violet", "darkblue", "lightblue", "darkgreen", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "beige","brown")

colors_vector <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ffff99", "#b15928", "#8dd3c7", "#bebada")

# Barplots
# numerical variables, bar plots
plot(income_tr$GPA, income_tr$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
# some outliers with low GPAs and insane good salaries, but mostly no relationship

plot(income_tr$DOB, income_tr$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)
# also year of birth doesn't have much to do, similar salaries throughout with some outliers

plot(income_tr$Tuition, income_tr$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)
# there is a big gap b/w 6e+4 and 8e+4

plot(log(income_tr$LinkedIN), income_tr$Salary, main="Linkedin and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

plot(income_tr$LinkedIN^2, income_tr$Salary, main="Linkedin and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)
# VERY INTERESTING, some relationship in salary and no of connections

# Let's see for each indivisual major

# Business
income_tr_bus <- income_tr[income_tr$Major=="Buisness", ]

plot(income_tr_bus$GPA, income_tr_bus$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
# Two distinct salaries
# Let's use RPart for Business

income_tr_bus_odd <- income_tr_bus[income_tr_bus$DOB %% 2 == 0, ]
income_tr_bus_even <- income_tr_bus[income_tr_bus$DOB %% 2 == 1, ]

plot(income_tr_bus_odd$GPA, income_tr_bus_odd$Salary, main="GPA and Salary for Business Majors born in even years", xlab="GPA", ylab="Salary", col=colors_vector)
plot(income_tr_bus_even$GPA, income_tr_bus_even$Salary, main="GPA and Salary for Business Majors born in odd years", xlab="GPA", ylab="Salary", col=colors_vector)


# STEM
income_tr_stem <- income_tr[income_tr$Major=="STEM", ]

plot(income_tr_stem$GPA, income_tr_stem$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
plot(1.2^(income_tr_stem$GPA), income_tr_stem$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)


plot(income_tr_stem$DOB, income_tr_stem$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_stem$Tuition, income_tr_stem$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_stem$LinkedIN), income_tr_stem$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# Vocational
income_tr_v <- income_tr[income_tr$Major=="Vocational",]

plot(income_tr_v$GPA, income_tr_v$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_v$DOB, income_tr_v$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_v$Tuition, income_tr_v$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_v$LinkedIN), income_tr_v$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# Professional

income_tr_p <- income_tr[income_tr$Major=="Professional", ]

plot(income_tr_p$GPA, income_tr_p$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_p$DOB, income_tr_p$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_p$Tuition, income_tr_p$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_p$LinkedIN), income_tr_p$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)

# Humanities

income_tr_h <- income_tr[income_tr$Major=="Humanities", ]

plot(income_tr_h$GPA, income_tr_h$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_tr_h$DOB, income_tr_h$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_tr_h$Tuition, income_tr_h$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(log(income_tr_h$LinkedIN), income_tr_h$Salary, main="LinkedIn and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)


####
# Linear Regression Model for Major=="Other"
model1 <- lm(Salary ~ SquareLi, data = income_tr[income_tr$Major == "Other", ])

# Decision Tree Model for Major!="Other"
model01 <- rpart(Salary ~ Major, data = income_tr[income_tr$Major != "Other" & income_tr$Major!="Buisness", ], method = "anova")

# Linear Regression Model for Major=="Buisness" and DOB%%2==0
model001 <- lm(Salary ~ GPA, data=income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 0, ])
model002 <- lm(Salary ~ GPA, data=income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 1, ])

# Predictions
pred1 <- predict(model1, newdata = income_tr[income_tr$Major == "Other", ])
pred01 <- predict(model01, newdata = income_tr[income_tr$Major != "Other" & income_tr$Major != "Buisness", ])
pred001 <- predict(model001, newdata = income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 0, ])
pred002 <- predict(model002, newdata = income_tr[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 1, ])

# Combine Predictions
decision <- rep(0, nrow(income_tr))
decision[income_tr$Major == "Other"] <- pred1
decision[income_tr$Major != "Other" & income_tr$Major!="Buisness"] <- pred01
decision[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 0] <- pred001
decision[income_tr$Major == "Buisness" & income_tr$DOB%%2 == 1] <- pred002

# Calculate Mean Squared Error
mse <- mean((decision - income_tr$Salary)^2)
mse # 12,170
# 1702



# PLAN
# lm with squared linkein + salary for OTHER
# lm with odd business with GPA
# lm with even business with GPA
# lm with vocational with GPA
# lm with proffessional with GPA
# lm with humanities with GPA
# lm with STEM with GPA
