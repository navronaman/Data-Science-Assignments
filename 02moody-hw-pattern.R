setwd("A:/Jeevan/Rutgers 2023-2024/Classes/DATA101/Assignment 2")
getwd()

moody <- read.csv("moody2023F2.csv")

head(moody)

summary(moody)

unique(moody$Grade)
unique(moody$Row)
unique(moody$Section)
unique(moody$Attendance)
unique(moody$Score)

boxplot(Score~Grade, data=moody, xlab="Grade", ylab="Score", main="Boxplot of Grade vs Score", col=c("khaki", "cyan"))

# Range for pass and range for fail
tapply(moody$Score, moody$Grade, range)

cat("We see that if a student has below a score of 50, he is bound to fail.")
cat("We see that if a student has above a score of 59, he is bound to pass.")
cat("Let's take an closer look at the data of students who have scored between 50 and 59, and that is data we are unsure about.")

mood <- moody[moody$Score > 50 & moody$Score <= 59,]
head(mood)
summary(mood)

tapply(mood$Score, mood$Section, mean)
tapply(mood$Score, mood$Row, mean)
tapply(mood$Score, mood$Attendance, mean)

table(mood$Attendance, mood$Section)
table(mood$Attendance, mood$Row)

colors <- c("violet", "blue", "cyan", "green","yellow", "red", "purple", "orange" )

mosaicplot(mood$Grade~mood$Section, xlab="Grade", ylab="Section", main="Mosaic of Grade and Section for a specific dataset", col=colors)

mosaicplot(mood$Grade~mood$Row, xlab="Grade", ylab="Row", main="Mosaic of Grade and Row for a specific dataset", col=colors)

mosaicplot(mood$Grade~mood$Attendance, xlab="Grade", ylab="Attendance", main="Mosaic of Grade and Attendance for a specific dataset", col=colors)
           
# Let's compare the section with the row, and see if the student passed

table(mood[mood$Section=="A1", ]$Grade, mood[mood$Section=="A1", ]$Row)

table(mood[mood$Section=="A2", ]$Grade, mood[mood$Section=="A2", ]$Row)

table(mood[mood$Section=="A3", ]$Grade, mood[mood$Section=="A3", ]$Row)

table(mood[mood$Section=="B1", ]$Grade, mood[mood$Section=="B1", ]$Row)

table(mood[mood$Section=="B2", ]$Grade, mood[mood$Section=="B2", ]$Row)

table(mood[mood$Section=="B3", ]$Grade, mood[mood$Section=="B3", ]$Row)

# Let's compare Section with attendance for dataset mood

table(mood[mood$Section=="A1", ]$Grade, mood[mood$Section=="A1", ]$Attendance)

table(mood[mood$Section=="A2", ]$Grade, mood[mood$Section=="A2", ]$Attendance)

table(mood[mood$Section=="A3", ]$Grade, mood[mood$Section=="A3", ]$Attendance)

table(mood[mood$Section=="B1", ]$Grade, mood[mood$Section=="B1", ]$Attendance)

table(mood[mood$Section=="B2", ]$Grade, mood[mood$Section=="B2", ]$Attendance)

table(mood[mood$Section=="B3", ]$Grade, mood[mood$Section=="B3", ]$Attendance)

# Let's compare row with attendance for mood dataset

table(mood[mood$Row==1, ]$Grade, mood[mood$Row==1, ]$Attendance)

table(mood[mood$Row==2, ]$Grade, mood[mood$Row==2, ]$Attendance)

table(mood[mood$Row==3, ]$Grade, mood[mood$Row==3, ]$Attendance)

table(mood[mood$Row==4, ]$Grade, mood[mood$Row==4, ]$Attendance)

table(mood[mood$Row==5, ]$Grade, mood[mood$Row==5, ]$Attendance)

table(mood[mood$Row==6, ]$Grade, mood[mood$Row==6, ]$Attendance)

table(mood[mood$Row==7, ]$Grade, mood[mood$Row==7, ]$Attendance)

table(mood[mood$Row==8, ]$Grade, mood[mood$Row==8, ]$Attendance)


## if you sit in section A1 and A2 you are more likely to fail
## if you sit in rows 6, 7 and 8, you are definitely going to fail
## high attendance helps you