# libraries
library(ggplot2)
library(tidyverse)

# reproducible
set.seed(1022)

# function to generate a set of scores
scores_rep <- function(n = 8, n_speed) {
  speed <- lead <- boulder <- rep(NA, n)
  
  if(n_speed > 0) {
    n_others <- n - n_speed
    
    # there is a speed specialist in this index. 
    # they should be top in speed and last in the others
    speed[1:n_speed] <- sample(1:n_speed, n_speed)
    lead[1:n_speed] <- sample((n_others + 1):n, n_speed)
    boulder[1:n_speed] <- sample((n_others + 1):n, n_speed)
    
    # non speed specialists are the opposite
    speed[(n_speed + 1):n] <- sample((n_speed + 1):n, n_others)
    lead[(n_speed + 1):n] <- sample(1:n_others, n_others)
    boulder[(n_speed + 1):n] <- sample(1:n_others, n_others)
  } else {
    speed <- sample(1:n, n)
    boulder <- sample(1:n, n)
    lead <- sample(1:n, n)
  }
  speed*boulder*lead # multiplicative rounds
}

# distribution of the scores for a particular place
dsn_place <- function(place, n = 8, B = 1000, cumulative = FALSE, n_speed = 0) {
  # place is which place
  # n is the number of people in the round (finals = 8)
  # B is number of samples
  # cumulative is the indicator of all places <= place
  # s_speed is the numebr of speed specialists in the round
  if(!cumulative) {
    scores_star <- rep(NA, B) 
  } else {
    scores_star <- matrix(NA, nrow = B, ncol = place)
  }
  for(i in 1:B) {
    scores <- scores_rep(n, n_speed)
    if(!cumulative) {
      scores_star[i] <- sort(scores)[place]
    } else {
      scores_star[i,] <- sort(scores)[1:place]
    }
  }
  scores_star
}

# finals scores
B <- 10000
max_place <- 8

scores_samples <- data.frame(dsn_place(max_place, max_place, B, cumulative = TRUE)) %>%
  gather(place, scores, everything()) %>%
  separate(place, into = c("junk", "place"), sep = 1) %>%
  mutate(place = as.numeric(place)) %>%
  select(-junk)

## all scores
ggplot(scores_samples) +
  geom_histogram(aes(scores, fill = place <= 3), binwidth = 10, position = "dodge") ## podium

## with 1 speed specialist in the final
scores_samples_speed <- data.frame(dsn_place(max_place, max_place, B, cumulative = TRUE, n_speed = 1)) %>%
  gather(place, scores, everything()) %>%
  separate(place, into = c("junk", "place"), sep = 1) %>%
  mutate(place = as.numeric(place)) %>%
  select(-junk)

## all scores
ggplot(scores_samples_speed) +
  geom_histogram(aes(scores, fill = place <= 3), binwidth = 10, position = "dodge") ## podium

## compare
scores_samples %>% mutate(speed = 0) %>%
  bind_rows(scores_samples_speed %>% mutate(speed = 1)) %>%
  ggplot() +
  geom_density(aes(scores, color = as.factor(speed))) + ## podium 
  facet_grid(place <= 3 ~.) +
  geom_vline(aes(xintercept = 64)) 


## probability that a speed climber medals
scores_samples %>%
  filter(scores >= 64 & place <= 3)  %>%
  summarise(num = n()) %>%
  as.numeric()/B

## Probability that a speed climber makes it to finals?
## What the is distribution of the number of speed climbers in the final?

## What is a qualifying score in the quali?
## what is a medal score in the final?
quantile(scores_samples_speed %>% filter(place <= 3) %>% .$scores, c(.025, .975))

## what is a gold medal score
quantile(scores_samples_speed %>% filter(place <= 1) %>% .$scores, c(.025, .975))












  


