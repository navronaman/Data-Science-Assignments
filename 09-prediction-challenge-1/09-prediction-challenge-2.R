# Imports
install.packages("rpart")
install.packages("rpart.plot")
devtools::install_github("devanshagr/CrossValidation")
library(rpart)
library(rpart.plot)


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

# let's try to use a factor for both models

nrow(submission)
summary(movies_tr$Audience)
summary(movies_te$Audience)

summary(movies_tr$Income)
summary(movies_te$Income)

# let's multiply audience by a factor of 4
# let's multiply income by a factor of 5

movies_tr$Audience <- movies_tr$Audience * 4
movies_tr$Income <- movies_tr$Income * 5

summary(movies_tr$Audience)
summary(movies_te$Audience)

summary(movies_tr$Income)
summary(movies_te$Income)

base_tree <- rpart(tree <- rpart(RATING ~ Genre+Content+Audience+Income, data = movies_tr, method = "class", ))

tree <- rpart(RATING ~ Genre+Content+Audience+Income, data = movies_tr, method = "class", control=rpart.control(cp = 0.00005))
tree
rpart.plot(tree)

# let's try manipulating some more variales
plot(movies_tr$Income, movies_tr$Audience, main="Correlation between audience and income", xlab="Income", ylab="Audience", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Great", ]$Income, movies_tr[movies_tr$RATING=="Great", ]$Audience, main="Correlation between audience and income for great movies", xlab="Income", ylab="Audience", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Average", ]$Income, movies_tr[movies_tr$RATING=="Average", ]$Audience, main="Correlation between audience and income for average movies", xlab="Income", ylab="Audience", col=colors_vector)

movies_tr$IncMinAud <- movies_tr$Income - movies_tr$Audience
movies_te$IncMinAud <- movies_te$Income - movies_te$Audience

movies_tr$IncOverAud <- movies_tr$Income/movies_tr$Audience
movies_te$IncOverAud <- movies_te$Income/movies_te$Audience

nrow(movies_tr)
nrow(movies_tr[movies_tr$Audience > movies_tr$IncMinAud, ])
nrow(movies_tr[movies_tr$Audience <= movies_tr$IncMinAud, ])

(nrow(movies_tr[movies_tr$Audience > movies_tr$IncMinAud & movies$RATING=="Great", ]))/(nrow(movies_tr[movies_tr$Audience > movies_tr$IncMinAud, ]))
(nrow(movies_tr[movies_tr$Audience > movies_tr$IncMinAud & movies$RATING=="Average", ]))/(nrow(movies_tr[movies_tr$Audience > movies_tr$IncMinAud, ]))

(nrow(movies_tr[movies_tr$Audience <= movies_tr$IncMinAud & movies$RATING=="Great", ]))/(nrow(movies_tr[movies_tr$Audience <= movies_tr$IncMinAud, ]))
(nrow(movies_tr[movies_tr$Audience <= movies_tr$IncMinAud & movies$RATING=="Average", ]))/(nrow(movies_tr[movies_tr$Audience <= movies_tr$IncMinAud, ]))

# 87% of movies that have audience greater than (income minus audience) are great
# 17% of movies that have audience less than (income minus audience) are average

plot(movies_tr$IncMinAud, movies_tr$Audience, main="Correlation between audience and (income-audience)", ylab="Audience", xlab="Audience", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Great", ]$IncMinAud, movies_tr[movies_tr$RATING=="Great", ]$Audience, main="Correlation between audience and (income-audience)", ylab="Audience", xlab="Audience", col=colors_vector)
plot(movies_tr[movies_tr$RATING=="Average", ]$IncMinAud, movies_tr[movies_tr$RATING=="Average", ]$Audience, main="Correlation between audience and (income-audience)", ylab="Audience", xlab="Audience", col=colors_vector)


mosaicplot()



# how to read a tree
# root avg (0.66, 0.33) - average no is 66% and average yes is 33%
# first condition is audience greater than 
# yes conditions go to left
# no condition go to right
# if audience is less than 12789, 


rpart.plot(tree)
pred <- predict(tree, movies_tr, type="class")
pred[movies_tr$Genre=="Drama" & movies_tr$Content=="R"] <- "Great"
pred[movies_tr$Audience > movies_tr$IncMinAud] <- "Average"
pred[movies_tr$Audience  < mean(movies_tr$Audience)*(1/3) & movies_tr$Income>mean(movies_tr$Income)*(1/3)] <- "Great"
head(pred)
mean(movies_tr$RATING==pred)


tree_new_3 <- rpart(RATING ~ Genre+Content+IncOverAud, data = movies_tr, method = "class")
tree_new_3
rpart.plot(tree_new_3)
pred_new_3 <- predict(tree_new_3, movies_tr, type="class")
head(pred)
mean(movies_tr$RATING==pred_new_3)




decision <- rep("Average", nrow(movies_tr))
decision[movies$Content=="R" & movies$Genre=="Drama"] <- "Great"
decision