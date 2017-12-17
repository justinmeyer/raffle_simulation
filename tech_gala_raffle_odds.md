Description of the analysis
===========================

At the recent Tech Gala in Madison, Wisconsin there was a raffle for a number of items. Sarah and I bought a total of 10 tickets and entered to win nine of the items (one ticket apparently went missing). We won five of the nine items that we tried to win.

This seemed like a very unlikely outcome so I got the data from the raffle organizer. Here are the items that we entered, the number of tickets we entered, the number of total tickets that were entered, and whether or not we won:

-   Pinecone 1/8 (won)
-   Card 1/2 (didn't win)
-   Candle 1/9 (didn't win)
-   Cutting boards \[5 of this item were available\] 1/24 (didn't win)
-   Studio class 1/21 (didn't win)
-   Gift card 1/7 (won)
-   Hammer \[2 of this item were available\] 1/11 (won one)
-   Stand \[4 of this item were available\] 1/37 (won one)
-   Cards 1/1 (won)

Simulate the raffle
===================

Set the number of times to simulate the raffle:

``` r
options(scipen = 999) # Turn off scientific notation
repeats <- 100000
```

Simulate the raffle 100000 times:

``` r
# Create source data
pinecone <- c(1, rep(0, 7))
card <- c(1, 0)
candle <- c(1, rep(0, 8))
board <- c(1, rep(0, 23))
class <- c(1, rep(0, 20))
gift_card <- c(1, rep(0, 6))
hammer <- c(1, rep(0, 10))
stand <- c(1, rep(0, 36))
cards <- c(1)

# Create a vector to store the number of items won
items_won_vector <- vector("numeric")

# Simulation loop
for(i in 1:repeats){
        
        # Determine if each item was won
        pinecone_won <- sample(pinecone, 1)
        card_won <- sample(card, 1)
        candle_won <- sample(candle, 1)
        board_won <- sum(sample(board, 5, replace = FALSE))
        class_won <- sample(class, 1)
        gift_card_won <- sample(gift_card, 1)
        hammer_won <- sum(sample(hammer, 2, replace = FALSE))
        stand_won <- sum(sample(stand, 4, replace = FALSE))
        cards_won <- sample(cards, 1)

        # Calculate total number of items won
        items_won <- sum(pinecone_won, card_won, 
                         candle_won, board_won, 
                         class_won, gift_card_won, 
                         hammer_won, stand_won, 
                         cards_won)
        
        # Report progress of simulation
        # print(paste0("test: ", i))
        # print(items_won)
        
        # Add number of items won to vector
        items_won_vector[i] <- items_won
        
        # Clean up
        rm(items_won,
                pinecone_won, card_won, 
                candle_won, board_won, 
                class_won, gift_card_won, 
                hammer_won, stand_won, 
                cards_won)
        
        }
```

### Summary statistics

``` r
# Summary stats
summary(items_won_vector)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   2.000   2.421   3.000   7.000

### Count of items won across the 100000 simulations

``` r
# Count of number of items won
table(items_won_vector)
```

    ## items_won_vector
    ##     1     2     3     4     5     6     7 
    ## 18569 38353 28823 11376  2474   372    33

Histogram of the count of items won across the 100000 simulations
=================================================================

``` r
hist(items_won_vector)
```

![](tech_gala_raffle_odds_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Percent of items won across the 100000 simulations

**We should expect the observed outcome (winning five of the nine raffles we entered) to happen only about 2.5% of the time:**

``` r
# Percent of number of items won
temp <- table(items_won_vector)
prop.table(temp)
```

    ## items_won_vector
    ##       1       2       3       4       5       6       7 
    ## 0.18569 0.38353 0.28823 0.11376 0.02474 0.00372 0.00033

Deterministic method
====================

While I created the above simulation my friend Carl approached the problem from a different direction. He calculated the odds of the outcome and came up with a very similar result:

``` r
winProbs <- c(1/8,
              1/2,
              1/9,
              sum(1/24,1/23,1/22,1/21,1/20),
              1/21,
              1/7,
              sum(1/11, 1/10),
              sum(1/37, 1/36, 1/35, 1/34),
              1)

#All combos of 5 from 9
allPossible = combn(9,5)

#Find joint probability of each outcome of 5 ... don't forget probabilities of losing the 4 that are not won.
probs <- sapply(1:dim(allPossible)[2], function(x) prod(winProbs[allPossible[,x]],
                                                        1-winProbs[-allPossible[,x]]))
```

### Probability of winning any five items:

``` r
sum(probs)
```

    ## [1] 0.02762106
