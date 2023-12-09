library(rpart)
library(rpart.plot)

income_tr <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/incomeTrain2023.csv')
income_te <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')

summary(income_tr)
summary(income_te)

# 4 numerical variables - no of linkedin connections, year of birth, tuition, gpa
# 2 categorical variable - college locations, college major

unique(income_tr$Major)
unique(income_te$Major)

unique(income_tr$CollegeLocation)
unique(income_te$College_location)

income_tr$CollegeLocation <- income_tr$College_location

unique(income_tr$DOB)
unique(income_te$DOB)
summary(income_tr$DOB)
summary(income_te$DOB)

summary(income_tr$LinkedIN)
summary(income_te$LinkedIN)

summary(income_tr$GPA)
summary(income_te$GPA)

summary(income_tr$Tuition)
summary(income_te$Tuition)

colors <- c("violet", "darkblue", "lightblue", "darkgreen", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "beige","brown")

colors_vector <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ffff99", "#b15928", "#8dd3c7", "#bebada")


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

income_tr_new <- income_tr[income_tr$Major=="Other", ]
# major = business is woah
# stem is whatt
# OTHER IS LINEAR

plot(income_tr_new$LinkedIN, income_tr_new$Salary, main="Linkedin and Salary [updated sets]", xlab="LinkedIn", ylab="Salary", col=colors_vector)


# categorical variables, box plots and bar plots
# box plots

boxplot(Salary~CollegeLocation, data=income_tr, xlab="Location", ylab="Salary", main="Boxplot of Salary vs Location", col=c("khaki", "cyan"))
# except outliers, pretty standard

boxplot(Salary~Major, data=income_tr[income_tr$Major!="Other", ], xlab="Location", ylab="Salary", main="Boxplot of Salary vs Location", col=colors_vector)
# so many outliers in other
# if we remove others, these are the rankings
# 1. Vocational, 2. Professional, 3. Humanities, 4. Business, 5. STEM

income_truncated <- income_tr[income_tr$Salary<16000, ]


summary(income_truncated$Salary)
# stem majors earn less


plot(income_truncated$GPA, income_truncated$Salary, main="GPA and Salary", xlab="GPA", ylab="Salary", col=colors_vector)

plot(income_truncated$DOB, income_truncated$Salary, main="Year and Salary", xlab="Year", ylab="Salary", col=colors_vector)

plot(income_truncated$Tuition, income_truncated$Salary, main="Tuition and Salary", xlab="Tuition", ylab="Salary", col=colors_vector)

plot(income_truncated$LinkedIN, income_truncated$Salary, main="Linkedin and Salary", xlab="LinkedIn", ylab="Salary", col=colors_vector)


# let's play with these 3 variables, GPA, LinkedIn, Tuition
income_tr$GPAOverTuition <- income_tr$GPA / income_tr$Tuition
income_tr$GPATimesTuition <- income_tr$GPA * income_tr$Tuition
income_tr$SqrGPAOverTuition <- sqrt(income_tr$GPA / income_tr$Tuition)
income_tr$SquareGPATimesTuition <- sqrt(income_tr$GPA * income_tr$Tuition)
income_tr$LogGPAOverTuition <- log(income_tr$GPA/income_tr$Tuition)
income_tr$LogGPATimesTuition <- log(income_tr$GPA*income_tr$Tuition)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

income_tr$normalizedGPA <- normalize(income_tr$GPA)
income_tr$normalizedTuition <- normalize(income_tr$Tuition)
income_tr$normalizedLinkedIn <- normalize(income_tr$LinkedIN)

income_tr$normalizedRatio <- income_tr$normalizedGPA/income_tr$normalizedTuition


plot(income_tr$GPAOverTuition, income_tr$Salary, main="(GPA/Tuition) and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
plot(income_tr$GPATimesTuition, income_tr$Salary, main="(GPA x Tuition) and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
plot(income_tr$SqrGPAOverTuition, income_tr$Salary, main="(sqrt[GPA / Tuition]) and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
plot(income_tr$SquareGPATimesTuition, income_tr$Salary, main="(sqrt[GPA * Tuition]) and Salary", xlab="GPA", ylab="Salary", col=colors_vector)
plot(income_tr$LogGPAOverTuition, income_tr$Salary, main="log(GPA / Tutition) and Salary", xlab="log(GPA/Tutition)", ylab = "Salary", col=colors_vector)
plot(income_tr$LogGPATimesTuition, income_tr$Salary, main="log(GPA / Tutition) and Salary", xlab="log(GPA/Tutition)", ylab = "Salary", col=colors_vector)



income_tr$GPAOverLinkedIn <- income_tr$normalizedGPA/income_tr$normalizedLinkedIn

plot(income_tr$GPAOverLinkedIn, income_tr$Salary, main="GPA/Tuition and Salary", xlab="log(GPA/Tutition)", ylab = "Salary", col=colors_vector)


income_tr$SqaureLi <- income_tr$LinkedIN ^ 2


# LM
head(income_tr)



model1 <- lm(Salary ~ Tuition+GPA+SqaureLi, data=income_tr[income_tr$Major=="Other", ])
model01 <- rpart(Salary ~ Tuition+GPA+LinkedIN+Major+CollegeLocation, data = income_tr[income_tr$Major!="Other", ], method = "class")
model01


# model2 <- lm(Salary~ Tuition+GPA+LinkedIN, data=income_tr[income_tr$Major=="Humanities", ])
# model2 <- rpart(Salary ~ Tuition+GPA+LinkedIN, data = income_tr[income_tr$Major=="Humanities", ], method = "class")

# model3 <- lm(Salary~ Tuition+GPA+LinkedIN, data=income_tr[income_tr$Major=="STEM", ])
# model3 <- rpart(Salary ~ Tuition+GPA+LinkedIN, data = income_tr[income_tr$Major=="STEM", ], method = "class")

# model4 <- lm(Salary~ Tuition+GPA+LinkedIN, data=income_tr[income_tr$Major=="Vocational", ])
# model4 <- rpart(Salary ~ Tuition+GPA+LinkedIN, data = income_tr[income_tr$Major=="Vocational", ], method = "class")

# model5 <- lm(Salary~ Tuition+GPA+LinkedIN, data=income_tr[income_tr$Major=="Professional", ])
# model5 <- rpart(Salary ~ Tuition+GPA+LinkedIN, data = income_tr[income_tr$Major=="Professional", ], method = "class")

# model6 <- lm(Salary~ Tuition+GPA+LinkedIN, data=income_tr[income_tr$Major=="Buisness", ])
# model6 <- rpart(Salary ~ Tuition+GPA+LinkedIN, data = income_tr[income_tr$Major=="Buisness", ], method = "class")

pred1 <- predict(model1, newdata=income_tr[income_tr$Major=="Other", ])
pred01 <- predict(model01, newdata=income_tr[income_tr$Major!="Other", ])
pred01

# pred2 <- predict(model2, newdata=income_tr[income_tr$Major=="Humanities", ])
# pred3 <- predict(model3, newdata=income_tr[income_tr$Major=="STEM", ])
# pred4 <- predict(model4, newdata=income_tr[income_tr$Major=="Vocational", ])
# pred5 <- predict(model5, newdata=income_tr[income_tr$Major=="Professional", ])
# pred6 <- predict(model6, newdata=income_tr[income_tr$Major=="Buisness", ])


# decision <- rep(0, nrow(income_tr))
# decision[income_tr$Major=="Other"] <- pred1
# decision[income_tr$Major=="Humanities"] <- pred2
# decision[income_tr$Major=="STEM"] <- pred3
# decision[income_tr$Major=="Vocational"] <- pred4
# decision[income_tr$Major=="Professional"] <- pred5
# decision[income_tr$Major=="Buisness"] <- pred6

decision <- rep(0, nrow(income_tr[income_tr$Major=="Other", ]))


decision[income_tr$Major=="Other"] <- pred1
# decision[income_tr$Major!="Other"] <- pred01


mean((decision-income_tr$Salary)^2)
# it is 6430

