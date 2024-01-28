install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)


party2023 <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")

head(party2023)
summary(party2023)
unique(party2023$Music)
unique(party2023$DJ)
unique(party2023$Day)
unique(party2023$Ticket)

# PART A

colors <- c("violet", "blue", "cyan", "green","yellow", "red", "purple", "orange" )

# Hypothesis 1
tapply(party2023$Attendance, party2023$DJ, mean)
barplot(tapply(party2023$Attendance, party2023$DJ, mean), main = "Mean attendance of various DJs", xlab = "DJ", ylab = "Mean Attedance",  col = colors)

# Ania has the highest attendance, Carol has lowest
# Null Hypothesis: There is no difference in mean attendance of Ani and Carol
# Alternative Hypothesis: Mean attendance of Ania is higher than Carol

permutation_test(party2023, 'DJ', 'Attendance', 10000, 'Carol', 'Ania')
z_test_from_data(party2023, 'DJ', 'Attendance', 'Carol', 'Ania')

# Hypothesis 2
df = party2023[party2023$Music == "Techno" & party2023$DJ == "Rohit" &party2023$Ticket>7, ]
tapply(df$Attendance, df$Day, mean)
barplot(tapply(df$Attendance, df$Day, mean), main = "Mean attendance of DJ Rohit playing Techno on various days", xlab = "Day", ylab = "Mean Attedance",  col = colors)

# Null Hypothesis:  There is no difference in mean attendance of DJ Rohit playing Techno with ticket price higher than $7 on Friday and Saturday
# Alternative Hypothesis: Mean attendance for DJ Rohit playing Techno with a $7 is higher on Friday than on Saturday

permutation_test(df, 'Day', 'Attendance', 10000, 'Saturday', 'Friday') #0.01
z_test_from_data(df, 'Day', 'Attendance', 'Saturday', 'Friday') # 0.01

# Hypothesis 3
df3 = party2023[party2023$Music=="Jazz" & party2023$Day=="Saturday", ]
tapply(df3$Rating, df3$DJ, mean)
barplot(tapply(df3$Rating, df3$DJ, mean), main = "Mean ratings for Jazz on a Saturday for different DJs", xlab = "DJ", ylab = "Mean Ratings", col = colors)

# Null Hypothesis: There is no difference in mean ratings of DJ Rohit and DJ Ania playing Jazz on a Saturday
# Alternative Hypothesis: Mean ratings of Jazz played on Saturday are higher for DJ Ania than they are for DJ Rohit

permutation_test(df3, 'DJ', 'Rating', 10000, 'Rohit', 'Ania')
z_test_from_data(df3, 'DJ', 'Rating', 'Rohit', 'Ania')

# Part B

# Hypothesis A
dfa = party2023[party2023$DJ=="Ania" & party2023$Ticket>7 & party2023$Day=="Thursday", ]
tapply(dfa$Attendance, dfa$Music, mean)

# Null Hypothesis: There is no difference in mean attendance of DJ Ania performing on Thursday with a ticket prices above $7 in Rock and Techno
# Alternative Hypothesis: For mean attendance of DJ Ania on Thursday with a ticket price above $7, Techno has a higher mean attendance than Rock

permutation_test(dfa, 'Music', 'Attendance', 10000, 'Rock', 'Techno') #p-value is 0.001

# Since p-value from permutation test is 0.001, we can reject the null hypothesis
# This p-value is very close to zero


# Hypothesis B
tapply(party2023$Attendance, party2023$DJ, mean)
barplot(tapply(party2023$Attendance, party2023$DJ, mean), main = "Mean attendance of various DJs", xlab = "DJ", ylab = "Mean Attedance",  col = colors)

# Null Hypothesis: There is no difference in mean attendance in DJ Alex and DJ Ania
# Alternative Hypothesis:  DJ Ania has a higher mean attendance than DJ Alex

permutation_test(party2023, 'DJ', 'Attendance', 10000, 'Alex', 'Ania') #p-value is 0.005-0.07
z_test_from_data(party2023, 'DJ', 'Attendance', 'Alex', 'Ania') #p-value is 0.027

# Since p-value from z-test is 0.027, we can reject the null hypothesis
# This is closer to the critical value

# Hypothesis C
tapply(party2023$Rating, party2023$Music, mean)
barplot(tapply(party2023$Rating, party2023$Music, mean), xlab = "Music Type", ylab = "Mean Ratings", main = "Mean ratings of various music types", col = colors)

# Null Hypothesis: There is no difference in mean ratings of HipHop and Salsa
# Alternative Hypothesis: Salsa has a higher mean ratings than that of HipHop

permutation_test(party2023, 'Music', 'Rating', 10000, 'HipHop', 'Salsa') #p-value is 0.09
z_test_from_data(party2023, 'Music', 'Rating', 'HipHop', 'Salsa') #p-value is 0.15

# Since p-value from z-test is 0.15, we fail to reject the null hypothesis