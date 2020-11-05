# Problem 4b
library(tidyverse)

# tournament format: a team keeps playing until they accrue 
# 7 wins or 3 losses (whichever comes first - no draws allowed). 
# Assume a fixed win rate p ∈ [0, 1] across all rounds
# (they are paired at random).

# Base tournament set up with repeats and data store
# Initialisation
NRepeat <- 10000                    # number of replicates
totalWins <- rep(NA, NRepeat)       # win store
totalLosses <- rep(NA, NRepeat)     # loss store
totalMatches <- rep(NA, NRepeat)    # matches store
probs <- rep(NA, NRepeat)           # probability store

# run tournament
set.seed(231215)

for (i in seq(NRepeat)) {
  nWins <- 0                   # set win counter
  nLosses <- 0                 # set loss counter
  
  
  p <- runif(1, min = 0, max = 1)    # calculate fixed probability
  
  
  while(nLosses < 3 & nWins < 7) {      # set stopping condition
    
      result <- rbinom(n = 1, size = 1, prob = p)   # simulate game result
      # 0 - loss
      # 1 - win
      
      if (result == 0) {
        nLosses <- nLosses + 1
      }
      
      if (result == 1) {
        nWins <- nWins + 1
      }
      
      nMatches <- nWins + nLosses            # find number of matches
  }
  
  totalLosses[i] <- nLosses              # record wins, losses, matches and probabilities
  totalWins[i] <- nWins                  
  totalMatches[i] <- nMatches
  probs[i] <- p
  

}

#Plot how the total number of matches played (i.e. wins + losses) 
#varies as a function of p.

# create data frame for ggplot
matchProb <- data.frame(p = probs, matches =totalMatches)

ggplot(matchProb, aes(x = p, y = matches)) +     # plot total matches against probability
  geom_point() +
  xlab("Probability") +
  ylab("Total Number of Matches") +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(min(matchProb$matches), max(matchProb$matches))) 



# Comment on the observed win rate relative to the assumed win rate p 
# (i.e. if a team obtains 2 wins - 3 losses, the maximum likelihood 
# point estimate for their win rate is 40%). Specifically, focus on the
# effect driven by the format of this tournament.

winRate <- data.frame(p = probs, wins = totalWins)

ggplot(winRate, aes(x = p, y = wins)) +     # plot total matches against probability
  geom_point() +
  xlab("Probability") +
  ylab("Number of Wins") +
  geom_smooth(se = FALSE) +
  geom_abline(aes(intercept = 0, slope = 7)) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(min(winRate$wins), max(winRate$wins))) 


winRate <- data.frame(p = probs, wins = totalWins, losses = totalLosses, rate = totalWins/(totalMatches))

ggplot(winRate, aes(x = p, y = rate)) +     # plot total matches against probability
  geom_point() +
  xlab("Probability") +
  ylab("Win rate") +
  geom_smooth(se = FALSE) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.1))


