install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

movies_tr<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Tr.csv')

head(movies_tr)
summary(movies_tr)
nrow(movies_tr)

movies_te<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Test_Student.csv')

head(movies_te)
summary(movies_te)
nrow(movies_te)

submission<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionMovies2023.csv')

head(submission)
summary(submission)
nrow(submission)

## Let's explore the data now

summary(movies_tr)
unique(movies_tr$Genre)
unique(movies_tr$Content)

## 4 C 3

plot(movies_tr$Income, movies_tr$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")

plot(movies_tr[movies_tr$RATING=="Great", ]$Income, movies_tr[movies_tr$RATING=="Great", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")

plot(movies_tr[movies_tr$RATING=="Average", ]$Income, movies_tr[movies_tr$RATING=="Average", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")


# trial 1
plot(movies_tr[movies_tr$Genre=="Drama", ]$Income, movies_tr[movies_tr$Genre=="Drama", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
# trial 2
plot(movies_tr[movies_tr$Genre=="Documentary", ]$Income, movies_tr[movies_tr$Genre=="Documentary", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
# trial 3
plot(movies_tr[movies_tr$Genre=="Comedy", ]$Income, movies_tr[movies_tr$Genre=="Comedy", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
# trial 4
plot(movies_tr[movies_tr$Genre=="Action", ]$Income, movies_tr[movies_tr$Genre=="Action", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")


# trial 5
plot(movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Action", ]$Income, movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Action", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Drama", ]$Income, movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Drama", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Documentary", ]$Income, movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Documentary", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Comedy", ]$Income, movies_tr[movies_tr$Content=="R" & movies_tr$Genre=="Comedy", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")

plot(movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Action", ]$Income, movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Action", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Drama", ]$Income, movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Drama", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Documentary", ]$Income, movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Documentary", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Comedy", ]$Income, movies_tr[movies_tr$Content=="PG" & movies_tr$Genre=="Comedy", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")

plot(movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Action", ]$Income, movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Action", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Drama", ]$Income, movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Drama", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Documentary", ]$Income, movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Documentary", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")
plot(movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Comedy", ]$Income, movies_tr[movies_tr$Content=="PG13" & movies_tr$Genre=="Comedy", ]$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience")

## this doesn't really work because uh, yeah bad idea
colors <- c("violet", "darkblue", "lightblue", "darkgreen", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "beige","brown")

colors_vector <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ffff99", "#b15928", "#8dd3c7", "#bebada")

# base case
boxplot(Audience~RATING, data=movies_tr, xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# average movies have HIGHER median than great movies for audience
# higher audience -> average, lower audience -> great
boxplot(Income~RATING, data=movies_tr, xlab="Rating", ylab="Income", main="Boxplot of Income vs Rating", col=c("khaki", "cyan"))
# great movies have HIGHER median than average movies for income
# higher income -> great, lower income -> average

# Cases for audience
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Action", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# similar to base case
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Comedy", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# #
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Documentary", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# #
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Drama", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# #

# cases for income
boxplot(Income~RATING, data=movies_tr[movies_tr$Genre=="Action", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# similar to base case
boxplot(Income~RATING, data=movies_tr[movies_tr$Genre=="Comedy", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# #
boxplot(Income~RATING, data=movies_tr[movies_tr$Genre=="Documentary", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# #
boxplot(Income~RATING, data=movies_tr[movies_tr$Genre=="Drama", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
# #
## lil dif for drama

boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Action" & movies_tr$Content=="R", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Action" & movies_tr$Content=="PG", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Action" & movies_tr$Content=="PG13", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))

boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Comedy" & movies_tr$Content=="R", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Comedy" & movies_tr$Content=="PG", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Comedy" & movies_tr$Content=="PG13", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))

boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Documentary" & movies_tr$Content=="R", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Documentary" & movies_tr$Content=="PG", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Documentary" & movies_tr$Content=="PG13", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))

boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Drama" & movies_tr$Content=="R", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
## VERY INTERESTING, almost similar medians
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Drama" & movies_tr$Content=="PG", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))
boxplot(Audience~RATING, data=movies_tr[movies_tr$Genre=="Drama" & movies_tr$Content=="PG13", ], xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))


## same for base for most


## MOSAIC PLOT TIME

mosaicplot(movies_tr$RATING~movies_tr$Content, xlab="RATING", ylab="Content Rating", main="Mosaic of RATING and Content Rating", col=colors)
mosaicplot(movies_tr$Content~movies_tr$RATING, xlab="Content Rating", ylab="Rating", main="Mosaic of RATING and Content Rating", col=colors)
# there are more average movies  in the dataset than there are great movies
# PG and PG13 movies have a higher tendency to be average than great movies

mosaicplot(movies_tr$RATING~movies_tr$Genre, xlab="RATING", ylab="Genre", main="Mosaic of RATING and Content Rating", col=colors)
mosaicplot(movies_tr$Genre~movies_tr$RATING, xlab="Genre", ylab="Rating", main="Mosaic of RATING and Content Rating", col=colors)

# Action, Comedy and Documentary tend to be Average, Drama splits even

mosaicplot(movies_tr[movies_tr$Content=="PG", ]$Genre~movies_tr[movies_tr$Content=="PG", ]$RATING, xlab="Genre", ylab="Rating", main="Mosaic of RATING and Content Rating", col=colors)
## all PG movies tend to be more average


mosaicplot(movies_tr[movies_tr$Content=="R", ]$Genre~movies_tr[movies_tr$Content=="R", ]$RATING, xlab="Genre", ylab="Rating", main="Mosaic of RATING and Content Rating", col=colors)
# DRAMA R rated movies tend to be GREAT AF
# Action R and Comedy R and Documentary R are average-er

interesting_data = movies_tr[movies_tr$Genre=="Drama" & movies_tr$Content=="R", ]
boxplot(Audience~RATING, data=interesting_data, xlab="Rating", ylab="Audience", main="Boxplot of Audience vs Rating for Int Data", col=c("khaki", "cyan"))
boxplot(Income~RATING, data=movies_tr, xlab="Rating", ylab="Income", main="Boxplot of Audience vs Rating", col=c("khaki", "cyan"))


mosaicplot(movies_tr[movies_tr$Content=="PG13", ]$Genre~movies_tr[movies_tr$Content=="PG13", ]$RATING, xlab="Genre", ylab="Rating", main="Mosaic of RATING and Content Rating", col=colors)
# PG13 tends to be avergae
# comedy pg13 is great-ish, same for docu PG13

nrow(movies_tr[movies_tr$RATING=="Average", ])/nrow(movies_tr)
nrow(movies_tr[movies_tr$RATING=="Great", ])/nrow(movies_tr)

# imports

install.packages("rpart")
install.packages("rpart.plot")
devtools::install_github("devanshagr/CrossValidation")
library(rpart)

library(rpart.plot)

colnames(movies_tr)

decision<-rep("Great", nrow(movies_tr))
decision[movies_tr$Genre=="Drama" & movies_tr$Content=="R"]<-"Great"
decision[movies_tr$Genre!="Drama" & movies_tr$Content!="R"]<-"Average"
accuracy<-round(mean(movies_tr$RATING==decision), 2)
accuracy

# rpart (formula, method, data, control, ...)
# formula - here we mention the prediction column

tree <- rpart(RATING ~ Genre+Content+Audience+Income, data = movies_tr, method = "class")
tree
rpart.plot(tree)
pred <- predict(tree, movies_tr, type="class")
head(pred)
mean(movies_tr$RATING==pred) # 92.40


summary(movies_tr$Income)
summary(movies_te$Income)

summary(movies_tr$Audience)
summary(movies_te$Audience)

# multiply the income and audience columns by a factor and then run rpart
# basically, multiply audience bt 5/4 and income by 3/4

# CONTROL PARAMETERS

# min split - no of rows below which a split doesn't occur
# it should only split if the number of rows is 200 or whatever
# higher min split means a smaller tree

tree2 <- rpart(RATING ~ Genre+Content+Audience+Income, data = movies_tr, method = "class", control=rpart.control(minsplit = 50))
rpart.plot(tree2)
pred2 <- predict(tree2, movies_tr, type = "class")
head(pred2)
mean(movies_tr$RATING==pred2) # 92

# 95 on training, 80 on testing
# 45+15+10 = 70


# min bucket
tree3 <- rpart(RATING ~ Genre+Content+Audience+Income, data = movies_tr, method = "class", control=rpart.control(minbucket = 100))
rpart.plot(tree3)
pred3 <- predict(tree3, movies_tr, type = "class")
head(pred3)
mean(movies_tr$RATING==pred3) # 87


# cp
tree4 <- rpart(RATING ~ Genre+Content+Audience+Income, data = movies_tr, method = "class", control=rpart.control(cp = 0.00005))
tree4
rpart.plot(tree4)
pred4 <- predict(tree4, movies_tr, type = "class")
head(pred4) 
mean(movies_tr$RATING==pred4) # 94.3

# will avoid rpart due to overfitting of data


# let's try something new
plot(movies_tr$Income, movies_tr$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience", col=colors_vector)

plot(movies_tr[movies_tr$RATING=="Great", ]$Income, movies_tr[movies_tr$RATING=="Great", ]$Audience, main="Correlation between audience and income for great movies", xlab="Income", ylab="Audience", col=colors_vector)

plot(movies_tr[movies_tr$RATING=="Average", ]$Income, movies_tr[movies_tr$RATING=="Average", ]$Audience, main="Correlation between audience and income for average movies", xlab="Income", ylab="Audience", col=colors_vector)

# average movies tend to have a higher audience, with an income that's increasing
# great movies have a high income, with an audience that is increasing
# there are many outliers, but let's create another variable

movies_tr$AudienceMinusIncome = movies_tr$Audience - movies_tr$Income
movies_tr$IncomeMinusAudience = movies_tr$Income - movies_tr$Audience

movies_te$AudienceMinusIncome = movies_te$Audience - movies_te$Income
movies_te$IncomeMinusAudience = movies_te$Income - movies_te$Audience

plot(movies_tr$Income, movies_tr$IncomeMinusAudience, main="Correlation between income and (income-audience)", xlab="Income", ylab="Audience", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Great", ]$Income, movies_tr[movies_tr$RATING=="Great", ]$IncomeMinusAudience, main="Correlation between income and (income and audience) for great movies", xlab="Income", ylab="(IncomeMinuseAudience)", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Average", ]$Income, movies_tr[movies_tr$RATING=="Average", ]$IncomeMinusAudience, main="Correlation between income and (income and audience) for average movies", xlab="Income", ylab="(IncomeMinusAudience)", col=colors_vector)

plot(movies_tr$IncomeMinusAudience, movies_tr$Income, main="Correlation between income and (income-audience)", ylab="Income", xlab="Audience", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Great", ]$IncomeMinusAudience, movies_tr[movies_tr$RATING=="Great", ]$Income, main="Correlation between income and (income and audience) for great movies", ylab="Income", xlab="(IncomeMinuseAudience)", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Average", ]$IncomeMinusAudience, movies_tr[movies_tr$RATING=="Average", ]$Income, main="Correlation between income and (income and audience) for average movies", ylab="Income", xlab="(IncomeMinusAudience)", col=colors_vector)

plot(movies_tr$AudienceMinusIncome, movies_tr$Audience, main="Correlation between income and (audience-income)", ylab="Audience", xlab="AudienceMinusIncome", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Great", ]$AudienceMinusIncome, movies_tr[movies_tr$RATING=="Great", ]$Audience, main="Correlation between audience and (audience-income) for great movies", ylab="Audience", xlab="(AudienceMinusIncome)", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Average", ]$AudienceMinusIncome, movies_tr[movies_tr$RATING=="Average", ]$Audience, main="Correlation between audience and (audience-income) for average movies", ylab="Audience", xlab="(AudienceMinusIncome)", col=colors_vector)



tree5 <- rpart(RATING ~ Genre+Content+Audience+Income+AudienceMinusIncome, data = movies_tr, method = "class")
tree5
rpart.plot(tree5)
pred <- predict(tree5, movies_tr, type="class")
head(pred)
mean(movies_tr$RATING==pred)