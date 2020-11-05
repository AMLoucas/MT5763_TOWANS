# Problem 4b

# tournament format: a team keeps playing until they accrue 
# 7 wins or 3 losses (whichever comes first - no draws allowed). 
# Assume a fixed win rate p ∈ [0, 1] across all rounds
# (they are paired at random).

# Initialisation
NRepeat <- 10000 
totalWins <- rep(NA, NRepeat)
totalLosses <- rep(NA, NRepeat)
totalMatches <- rep(NA, NRepeat)
probs <- rep(NA, NRepeat)

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
  
  totalLosses[i] <- nLosses
  totalWins[i] <- nWins
  totalMatches[i] <- nMatches
  probs[i] <- p
  

}


#Plot how the total number of matches played (i.e. wins + losses) 
#varies as a function of p.








# Comment on the observed win rate relative to the assumed win rate p 
# (i.e. if a team obtains 2 wins - 3 losses, the maximum likelihood 
# point estimate for their win rate is 40%). Specifically, focus on the
# effect driven by the format of this tournament.


