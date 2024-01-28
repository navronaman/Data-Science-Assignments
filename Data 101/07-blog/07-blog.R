install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

setwd("A:/Jeevan/Rutgers 2023-2024/Classes/DATA101/Assignment 7")
getwd()

books <- read.csv("books.csv")
head(books)
summary(books)

## let's do some pre-processing 

books$average_rating <- as.numeric(books$average_rating)
books$isbn <- as.numeric(books$isbn)
books$isbn13 <- as.numeric(books$isbn13)
books$num_pages <- as.numeric(books$num_pages)
books$publication_year <- as.numeric(substr(books$publication_date, nchar(books$publication_date) - 3, nchar(books$publication_date)))
books <- books[!is.na(books$publication_year), ]
books <- books[books$publication_year>1000, ]
books$publication_decade <- cut(books$publication_year,
                                breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
                                labels = c("1900s", "1910s", "1920s", "1930s", "1940s", "1950s", "1960s", "1970s" ,"1980s", "1990s", "2000s", "2010s"),
                                include.lowest = TRUE)
books$ratings_range <- cut(books$average_rating, 
                           breaks = c(0, 1, 2, 3, 4, 5),
                           labels = c(1, 2, 3, 4, 5),
                           include.lowest = TRUE)
books$page_range <- cut(books$num_pages,
                        breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 5000),
                        labels = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 5000),
                        include.lowest = TRUE)
books <- books[!is.na(books$page_range), ]

nrow(books)
ncol(books)

# now that our dataset has some extra categorical and numerical variables for us to work with
# let's get started on the analysis
head(books)

summary(books)

unique(books$publication_decade)

unique(books$ratings_range)

unique(books$publisher)

# we have a lot of publishers, let's create a subset with the top 10 publishers
publisher_freq <- table(books$publisher)

top_publishers <- head(sort(publisher_freq, decreasing = TRUE), 10)
cat("Top 10 Publishers:")
top_publishers

books_top <- books[books$publisher=="Penguin Books" | 
                     books$publisher=="Mariner Books" | 
                     books$publisher=="Ballantine Books" | 
                     books$publisher=="HarperCollins" | 
                     books$publisher=="Pocket Books" |
                     books$publisher=="Scholastic Inc." |
                     books$publisher=="Bantam" |
                     books$publisher=="VIZ Media LLC" |
                     books$publisher=="Penguin Classics" |
                     books$publisher=="Vintage" |
                     books$publisher=="Scholastic Inc.", ]

summary(books_top)

colors <- c("violet", "darkblue", "lightblue", "beige", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "darkgreen","brown")

colors_vector <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ffff99", "#b15928", "#8dd3c7", "#bebada")


tapply(books_top$average_rating, books_top$publisher, mean)
barplot(tapply(books_top$average_rating, books_top$publisher, mean), 
        xlab = "Top Publishers",
        ylab = "Mean Ratings", 
        main = "Mean Ratings of Top Publishers",
        col = colors_vector)

tapply(books$average_rating, books$publication_decade, mean)
tapply(books_top$average_rating, books_top$publisher, mean)

max(tapply(books$average_rating, books$publication_decade, mean))
min(tapply(books$average_rating, books$publication_decade, mean))

barplot(tapply(books$average_rating, books$publication_decade, mean), 
        xlab = "Publication Decade",
        ylab = "Mean Rating out of 5",
        main = "Mean Ratings of Books accross decades",
        col =  colors_vector)


# Null Hypothesis - There is no difference between the mean ratings of 1930s and 1920s
# Alternate Hypothesis - Books of 1920s have a higher mean rating than the books of 1930s

permutation_test(books, "publication_decade", "average_rating", 10000, "1930s", "1920s")

z_test_from_data(books, "publication_decade", "average_rating", "1930s", "1920s") #0.02

z_test_from_data(books, "publication_decade", "average_rating", "2000s", "1950s") #0.03

# Null Hypothesis - There is no mean difference between the Phalanu and Dhimkhanu Publisher
# Alternate - Phalanu Publisher has higher mean rating that Dhimkanu Publisher

books_top2 <- books[books$publisher=="Scholastic Inc." | books$publisher=="Penguin Books", ]
tapply(books_top2$average_rating, books_top2$publisher, mean)
barplot(tapply(books_top2$average_rating, books_top2$publisher, mean),
        xlab = "Publisher",
        ylab = "Mean Rating out of 5",
        main = "Mean Ratings of Penguin and Scholastic",
        col =  colors_vector)



permutation_test(books_top, "publisher", "average_rating", 10000, "Penguin Books", "Scholastic Inc.")

# From permutation test we can see that Scholastic Inc has had a higher mean ratings for its book than Penguin Books

schol_peng <- books[books$publisher == "Scholastic Inc." |
                      books$publisher == "Penguin Books", ]
top_from_peng_schol <- head(schol_peng[order(-schol_peng$average_rating), ], 10)
tapply(top_from_peng_schol$average_rating, top_from_peng_schol$title, mean)
barplot(tapply(top_from_peng_schol$average_rating, top_from_peng_schol$title, mean),
        main = "Top Books from Scholastic and Penguin",
        xlab = "Title of the Book",
        ylab = "Average Rating",
        col = colors_vector,
        las = 2,  # Rotate x-axis labels
        cex.names = 0.8)  # Adjust the size of the labels

par(cex.lab = 0.7,
    cex.axis = 0.7)

boxplot(num_pages~publisher,
        data=books_top[books_top$num_pages<1000, ],
        xlab = "Publisher",
        ylab = "Num of Pages",
        main = "Boxplot of Publishers and Num of Pages",
        col = colors_vector,
        border="black"
)

boxplot(num_pages~authors,
        data=books_odd[books_odd$num_pages<1000, ],
        xlab = "Author",
        ylab = "Num of Pages",
        main = "Boxplot of Authors and Num of Pages",
        col = colors,
        border="black"
)


# books_s <- books[grepl("Agatha Christie|Roald Dahl", books$authors, ignore.case = TRUE), ]
# tapply(books_s$average_rating, books_s$authors, mean)

# z_test_from_data(books_s, "authors", "average_rating", "Agatha Christie", "Roald Dahl")


# Chi Square Test between Ratings and Ratings Count
contingency_table <- table(books$authors, books$ratings_range)
contingency_table
chisq.test(contingency_table)


books_odd = books[books$authors == "Roald Dahl" | 
                    books$authors == "J.K. Rowling" | 
                    books$authors == "Agatha Christie" | 
                    books$authors == "Enid Blyton", ]

head(books_odd)

# let's create a table for us to do bayesian odds in

table(books_odd$ratings_range, books_odd$authors)

# Observation - JK Rowling is the author
# Belief - JK Rowling's books have a rating of 4 or higher

# Number of rows that have a rating of 4 or higher
nf <- nrow(books_odd[books_odd$ratings_range == 5, ])
nf

n <- nrow(books_odd)
n

Prior <- nf/n
Prior

PriorOdds <- Prior/(1-Prior)
PriorOdds

TruePositive <- (nrow(books_odd[books_odd$authors=="J.K. Rowling" & books_odd$ratings_range==5, ]))/(nrow(books_odd[books_odd$authors=="J.K. Rowling", ]))
TruePositive <- round(TruePositive, 2)
TruePositive

FalsePositive <- (nrow(books_odd[books_odd$authors=="J.K. Rowling" & books_odd$ratings_range!= 5, ]))/(nrow(books_odd[books_odd$authors!="J. K. Rowling", ]))
FalsePositive <- round(FalsePositive, 2)
FalsePositive

LikelihoodRatio <- TruePositive/FalsePositive
LikelihoodRatio

PosteriorOdds <- LikelihoodRatio * PriorOdds
PosteriorOdds

Posterior <- PosteriorOdds/(1+Posterior)
Posterior

# Observation - The book has a rating of 4 or higher
# Belief - The book is written by Agatha Christie

# Number of rows that have a rating of 4 or higher
nf <- nrow(books_odd[books_odd$authors == "Agatha Christie", ])
nf

n <- nrow(books_odd)
n

Prior <- nf/n
Prior

PriorOdds <- Prior/(1-Prior)
PriorOdds

TruePositive <- (nrow(books_odd[books_odd$authors=="Agatha Christie" & books_odd$ratings_range==5, ]))/(nrow(books_odd[books_odd$range==5, ]))
TruePositive <- round(TruePositive, 2)
TruePositive

FalsePositive <- (nrow(books_odd[books_odd$authors!="Agatha Christie" & books_odd$ratings_range== 5, ]))/(nrow(books_odd[books_odd$range!=5, ]))
FalsePositive <- round(FalsePositive, 2)
FalsePositive

LikelihoodRatio <- TruePositive/FalsePositive
LikelihoodRatio

PosteriorOdds <- LikelihoodRatio * PriorOdds
PosteriorOdds

Posterior <- PosteriorOdds/(1+Posterior)
Posterior

# not working need to select something better

summary(books_odd)
summary(books_odd$ratings_count)
summary(books_odd$num_pages)
table(books_odd$authors, books_odd$ratings_count)

# okay so
table(books_odd$authors, books_odd$publication_decade)

# observation - agatha christie is the author
# belief - book is from 2000s

nf <- nrow(books_odd[books_odd$publication_decade == "2000s", ])
nf #44

n <- nrow(books_odd)
n #63

Prior <- nf/n
Prior #0.7

PriorOdds <- Prior/(1-Prior)
PriorOdds #2.3

TruePositive <- (nrow(books_odd[books_odd$authors=="Agatha Christie" & books_odd$publication_decade=="2000s", ]))/(nrow(books_odd[books_odd$publication_decade=="2000s", ]))
TruePositive <- round(TruePositive, 2)
TruePositive #0.55

FalsePositive <- (nrow(books_odd[books_odd$authors=="Agatha Christie" & books_odd$publication_decade!="2000s", ]))/(nrow(books_odd[books_odd$publication_decade=="2000s", ]))
FalsePositive <- round(FalsePositive, 2)
FalsePositive #0.2

LikelihoodRatio <- TruePositive/FalsePositive
LikelihoodRatio #2.75

PosteriorOdds <- LikelihoodRatio * PriorOdds
PosteriorOdds #6.37

Posterior <- PosteriorOdds/(1+Posterior)
Posterior #5.22

books_odd[books_odd$publication_decade=="2000s" & books_odd$authors=="Agatha Christie", ]
