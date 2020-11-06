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
     
      if (result == 0) {  # 0 - loss
        nLosses <- nLosses + 1
      }
      
      if (result == 1) { # 1 - win
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

# Initialisation
avgMatches <- rep(NA, 100) # average matches store
avgWinRate <- rep(NA, 100) # average matches store
pseq <- seq(0,1,0.01) # probability sequence
# run tournament
set.seed(231220)
for (p in pseq){
  NRepeat <- 10000                    # number of replicates
  totalWins <- rep(NA, NRepeat)       # win store
  totalLosses <- rep(NA, NRepeat)     # loss store
  totalMatches <- rep(NA, NRepeat)    # matches store
  probs <- rep(NA, NRepeat)           # probability store
  
  for (i in seq(NRepeat)) {
    nWins <- 0                   # set win counter
    nLosses <- 0                 # set loss counter
    
    while(nLosses < 3 & nWins < 7) {      # set stopping condition
      result <- rbinom(n = 1, size = 1, prob = p)   # simulate game result
      
      if (result == 0) {  # 0 - loss
        nLosses <- nLosses + 1
      }
      
      if (result == 1) { # 1 - win
        nWins <- nWins + 1
      }
      nMatches <- nWins + nLosses            # find number of matches
    }
    
    totalMatches[i] <- nMatches
    totalLosses[i] <- nLosses              # record wins, losses, matches and matches
    totalWins[i] <- nWins                  
  
    
  }
  avgMatches[which(pseq == p)] <- mean(totalMatches)
  avgWinRate[which(pseq == p)] <- mean(totalWins/totalMatches)
}  
  
# create data frame for ggplot
matchProb <- data.frame(p = pseq, matches = avgMatches, rate = avgWinRate)

ggplot(matchProb, aes(x = p, y = avgMatches)) +     # plot total matches against probability
  geom_point() +
  xlab("Probability") +
  ylab("Total Number of Matches") +
  ggtitle("Number of matches vs Probability") +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(min(matchProb$matches), max(matchProb$matches))) 



# Comment on the observed win rate relative to the assumed win rate p 
# (i.e. if a team obtains 2 wins - 3 losses, the maximum likelihood 
# point estimate for their win rate is 40%). Specifically, focus on the
# effect driven by the format of this tournament.

# plots to back up commentary 
ggplot(matchProb, aes(x = p, y = rate)) +     # plot win rate against probability
  geom_point() +
  xlab("Probability") +
  ylab("Observed Win rate") +
  ggtitle("Probability vs Observed Win Rate") +
  geom_smooth(se = FALSE) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.1))

ggplot(matchProb, aes(x = p, y = abs(rate-p))) +     # plot win rate against probability
  geom_point() +
  xlab("Probability") +
  ylab("Observed Difference") +
  ggtitle("Observed differences between probability and observed win rate") +
  scale_x_continuous(breaks = seq(0,1,0.1)) 





