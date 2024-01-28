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

movies_tr$IncOverAud <- movies_tr$Income/movies_tr$Audience
movies_te$IncOverAud <- movies_te$Income/movies_te$Audience


# tree
tree_new_5 <- rpart(RATING ~ Genre+Content+Income+Audience+IncOverAud, data = movies_tr, method = "class")
tree_new_5
rpart.plot(tree_new_5)
pred_new_5 <- predict(tree_new_5, movies_tr, type="class")
head(pred)
mean(movies_tr$RATING==pred_new_5)


submission<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionMovies2023.csv')

submission$RATING<-pred
submission
write.csv(submission, '/Users/pateltulsic/Desktop/submission.csv', row.names=FALSE)
