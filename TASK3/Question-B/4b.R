# Problem 4b

# tournament format: a team keeps playing until they accrue 
# 7 wins or 3 losses (whichever comes first - no draws allowed). 
# Assume a fixed win rate p ∈ [0, 1] across all rounds
# (they are paired at random).

set.seed(231215)
nWins <- 0
nLosses <- 0
nMatches <- 0

p <- runif(1, min = 0, max = 1)


while(nLosses < 3 & nWins < 7) {
  
    result <- rbinom(n = 1, size = 1, prob = p)
    nMatches <- nMatches + 1
    # 0 - loss
    # 1 - win
    
    if (result == 0) {
      nLosses <- nLosses + 1
    }
    
    if (result == 1) {
      nWins <- nWins + 1
    }
    
}




#Plot how the total number of matches played (i.e. wins + losses) 
#varies as a function of p.

# Comment on the observed win rate relative to the assumed win rate p 
# (i.e. if a team obtains 2 wins - 3 losses, the maximum likelihood 
# point estimate for their win rate is 40%). Specifically, focus on the
# effect driven by the format of this tournament.


