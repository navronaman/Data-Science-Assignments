Data Pages - Quantifying the Library

Naman Shah

Assignment 7

Data101

[kaggle.com/datasets/jealousleopard/goodreadsbooks](https://www.kaggle.com/datasets/jealousleopard/goodreadsbooks)


## <a name="_d0fxud1l4hqz"></a>Overview
My name is Naman Shah, and I’m a freshman studying Computer Science and Data Science. Lately, I've been on my computers all the time, but before the onset of technology in my life, I loved reading. Taking me to my hometown’s library would be the happiest day of my life. From the fantasies of Rick Riordan in Percy Jackson, Katniss Everdeen in Hunger Games, Detective Hercule Poirot in Murder on the Orient Express I absolutely loved books. My favorite authors of all time were J K Rowling, Agatha Christie, Enid Blyton and Roald Dahl. Their books took me to a different world, the Hogwarts Express, the Chocolate Factory, all while I was sitting on a couch in the library. I wanted to learn more about the books, their publication houses, their ratings, their re-releases with production changes, and how books have changed over the years.

## <a name="_4nthhktupy0b"></a>Dataset
I chose the Goodreads-book dataset from Kaggle to work on the data. It has entries from 11122 books, and it has 12 columns describing each book. From the title, the authors, the publication house, to the ratings, publication date, etc. I added 4 variables of my own using the cut() function to talk about things like rating range (out of 5), publication decade, etc. Let’s have a look at some of the relationships, trends that I observed in the dataset.

## <a name="_14c3zhj9t4m8"></a>Which Decade had the best of the best?

The 1920s had some of the best books according to user ratings on GoodReads, surprisingly the 1930s saw a sharp drop in user ratings. To confirm my results, I ran a z-test on the data. 

Null Hypothesis - There is no difference between the mean ratings of 1930s and 1920s

Alternate Hypothesis - Books of 1920s have a higher mean rating than the books of 1930s

P-value - 0.02

As this p-value was less than 0.05, I could reject the null hypothesis. I still believe that the release of Murder of Roger Acrokyd in June 1926 by Agatha Christie is the reason for this huge gap. A solid recommendation to anyone, you will not see the end coming. 

## <a name="_cdw52yp0vkl3"></a>Top Publishers and their mean ratings

Surprisingly, VIZ Media LLC which primarily publishes Anime books, manga and other comic book content had one of the highest mean ratings for a big publisher. Scholastic Inc. which is popular for being one of the only publishers who accepted J K Rowling’s first Harry Potter book, also has a high mean rating.

If you’re me, the only two publishers you’ve heard of are Penguin and Scholastic. I really wanted to see how they did ratings wise, so I compared their mean ratings for Scholastic Inc. and Penguin Books. Scholastic had a solid 4.06 while Penguin Classics had a 3.94. This was close. So I used a permutation test. 

Null Hypothesis - There is no mean difference between the Scholastic and Penguin’s mean ratings

Alternate - Scholastic has higher mean rating that Penguin

I got the p-value of 0.0282, once again I could reject my null hypothesis.

Some of books with highest mean ratings from Penguin and Scholastic include Harry Potter and the Prisoner of Azkaban, Life with Jeeves, The Complete Maus, and Before the Mayflower: A Complete History of Black America.

## <a name="_m07o30dc0kce"></a>Bayesian Odds - Who published when?

I subsetted the dataset to include my top 4 authors, JKR, Agatha Christie, Enid Blyton and Roald Dahl. Then from their publication date, I spread their books into the decades that they were published. 

**Observation - Agatha Christie is the author**

**Belief - Book is from the 2000s**

- Prior Odds = 2.3
- True Positive = 0.55
- False Positive = 0.2
- Likelihood Ratio = 2.75
- Posterior Odds = 6.37

The analysis suggests that after observing Agatha Christie as the author, the odds of the book being from the 2000s increase from the prior odds of 2.3 to the posterior odds of 6.37.

This implies that, given Agatha Christie as the author, the probability of the book being from the 2000s is higher than the overall prior probability.

Now this is surprising for many reasons, mainly because Agatha Christie passed away in 1976. After some more investigations we found that a number of her books had been republished in the past 20 years under her trust fund.

## <a name="_7xbeps65zjhz"></a>Other Interesting Data Analysis