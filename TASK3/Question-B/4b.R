# Problem 4b
library(tidyverse)
library(doParallel)
library(parallel)

# tournament format: a team keeps playing until they accrue 
# 7 wins or 3 losses (whichever comes first - no draws allowed). 
# Assume a fixed win rate p ∈ [0, 1] across all rounds
# (they are paired at random).

# Base tournament set up with repeats and data store
# Initialisation

# function takes fixed p between 0 and 1 and 1 replicate as default 
tournament <- function(p = runif(1, min = 0, max = 1), NRepeat = 1) {
                    
  totalWins <- rep(NA, NRepeat)       # win store
  totalLosses <- rep(NA, NRepeat)     # loss store
  totalMatches <- rep(NA, NRepeat)    # matches store
  
  # run tournament
  set.seed(231215)             # reproducibility
  
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
    
    totalLosses[i] <- nLosses              # record wins, losses, matches and probabilities
    totalWins[i] <- nWins                  
    totalMatches[i] <- nMatches
  
  }
  return(list(losses = totalLosses,         
              matches = totalMatches,
              wins = totalWins, 
              probability = p
             ))
}



#Plot how the total number of matches played (i.e. wins + losses) 
#varies as a function of p.

pseq <- seq(0,1,0.01) # probability sequence

# Parallelise
nCores <- 8 # no. of cores
cl <- makeCluster(spec = nCores, type = "PSOCK")
registerDoParallel(cl)

start <- Sys.time()
averages <- foreach(p = pseq, .combine='rbind', .multicombine=TRUE) %dopar% {
  sim <- tournament(p, 10000)
  matches <-(mean(sim$matches))
  rate <- (mean(sim$wins/sim$matches))
  data.frame(p, matches, rate)
}
end <- Sys.time()
stopCluster(cl)


# non parallelise 
# run tournament

averages <- data.frame(p = pseq, matches = rep(NA,101), rate = rep(NA,101))

start <- Sys.time()
for (p in pseq){
  sim <- tournament(p, 10000)
  averages[which(averages$p == p),]$matches <- mean(sim$matches)
  averages[which(averages$p == p),]$rate <- mean(sim$wins/sim$matches)
}  
end <- Sys.time()
end-start


ggplot(averages, aes(x = p, y = matches)) +     # plot total matches against probability
  geom_point() +
  xlab("Assumed win rate") +
  ylab("Total Number of Matches") +
  ggtitle("Number of matches vs Assumed win Rate") +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(min(matchProb$matches), max(matchProb$matches) + 1)) 



# Comment on the observed win rate relative to the assumed win rate p 
# (i.e. if a team obtains 2 wins - 3 losses, the maximum likelihood 
# point estimate for their win rate is 40%). Specifically, focus on the
# effect driven by the format of this tournament.

# plots to back up commentary 
ggplot(averages, aes(x = p, y = rate)) +     # plot win rate against probability
  geom_point() +
  xlab("Assumed win rate") +
  ylab("Observed win rate") +
  ggtitle("Observed Win Rate v Assumed win rate") +
  geom_smooth(se = FALSE) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.1))

ggplot(averages, aes(x = p, y = (rate-p))) +     # plot win rate against probability
  geom_point() +
  xlab("Assumed win rate") +
  ylab("Observed Difference") +
  ggtitle("Observed differences between assumed win rate and observed win rate") +
  scale_x_continuous(breaks = seq(0,1,0.1)) 


# run speed testing

times <- rep(NA,10)
for(i in 1:10){
  # Parallelise
  nCores <- 8 # no. of cores
  cl <- makeCluster(spec = nCores, type = "PSOCK")
  registerDoParallel(cl)
  
  start <- Sys.time()
  averages <- foreach(p = pseq, .combine='rbind', .multicombine=TRUE) %dopar% {
    sim <- tournament(p, 10000)
    matches <-(mean(sim$matches))
    rate <- (mean(sim$wins/sim$matches))
    data.frame(p, matches, rate)
  }
  end <- Sys.time()
  times[i] <- end-start
  stopCluster(cl)
}
avgP <- mean(times)

times <- rep(NA,10)
for(i in 1:10){
  start <- Sys.time()
  for (p in pseq){
    sim <- tournament(p, 10000)
    averages[which(averages$p == p),]$matches <- mean(sim$matches)
    averages[which(averages$p == p),]$rate <- mean(sim$wins/sim$matches)
  }  
  end <- Sys.time()
  times[i] <- end-start
}
avgNP <- mean(times)