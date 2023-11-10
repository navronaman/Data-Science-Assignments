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
                   books$publisher=="Vintage", ]

summary(books_top)



# HaperCollins Publishers = 48
# HarperOne - 35
# HarperTorch - 48



colors <- c("violet", "darkblue", "lightblue", "darkgreen", "lightgreen","yellow", "darkred", "cyan","orange", "pink", "grey", "beige","brown")

tapply(books$average_rating, books$publication_decade, mean)
max(tapply(books$average_rating, books$publication_decade, mean))
min(tapply(books$average_rating, books$publication_decade, mean))

barplot(tapply(books$average_rating, books$publication_decade, mean), 
        xlab = "Publication Decade",
        ylab = "Mean Rating out of 5",
        main = "Mean Ratings of Books accross decades",
        col =  colors)


# Null Hypothesis - There is no difference between the mean ratings of 1930s and 1920s
# Alternate Hypothesis - Books of 1920s have a higher mean rating than the books of 1930s

permutation_test(books, "publication_decade", "average_rating", 10000, "1930s", "1920s")

z_test_from_data(books, "publication_decade", "average_rating", "1930s", "1920s") #0.02

z_test_from_data(books, "publication_decade", "average_rating", "2000s", "1950s") #0.03



books[books$authors=="Agatha Christie", ]
books[books$authors=="Roald Dahl", ]


books_s <- books[grepl("Agatha Christie|Roald Dahl", books$authors, ignore.case = TRUE), ]
tapply(books_s$average_rating, books_s$authors, mean)

z_test_from_data(books_s, "authors", "average_rating", "Agatha Christie", "Roald Dahl")

