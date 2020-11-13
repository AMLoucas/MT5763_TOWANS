# Group project
# Monte Carlo simulation

library(tidyverse)
library(doParallel)
library(parallel)
# Question 1 - Compute the probability

# Simulate the observed data
set.seed(45214)           # for reproducibility
N <- 1000                 # Specify the number of datasets to be simulated
x <- rnorm(N,4,sqrt(10))  # create the "observed" data for x
y <- runif(N,2,8)         # create the "observed" data for y

# calculate the probability
p <- sum(x > y)/N
print(p)

#------------------------------

#Question 2 - Bootstrapping

# create data
regData <- data.frame(x,y)

# Define a function called BootStrap for bootstrapping
# with variables inputData and nBoot
BootStrap <- function(inputData, nBoot){
  
  set.seed(56337)   # for reproducibility
  bootResults <- array(dim=c(nBoot, 1))  # results
  
  # Loop across all samples
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # find the number for X>Y
    tot <- sum(bootData[,1] > bootData[,2])
    
    # Store results
    bootResults[i] <- tot/nrow(inputData)
    
  }
  
  bootResults
  
} # end of function


# derive the sample variance
sample_distri <- BootStrap(regData,nBoot = 1000)

# Plot the histogram
ggplot(data.frame(sample_distri)) +
  geom_histogram(aes(sample_distri), colour = "black", fill = "orange", alpha = 0.1) +
  geom_vline(xintercept = p, col="purple", lwd=1) +
  ggtitle("Histogram of Pr(X>Y)") +
  xlab("Pr(X > Y)") +
  ylab("Frequency") 


#-----------------------------

# Question 3 - how sample variance varies with No. of simulations



# Loop through varying numbers of MC simulations
# from 5 to 200
nSim <- seq(5, 200)

# Non - parallelised version
# Create empty data frame to append estimations to
sample_var_final <- data.frame(number_of_simulations = nSim, variance = rep(NA, length(nSim)))

start <- Sys.time()
for (k in nSim){
  
  # Simulate observed data
  set.seed(45214)           # for reproducibility
  x <- rnorm(k,4,sqrt(10))  # create the "observed" data for x
  y <- runif(k,2,8)         # create the "observed" data for y
  regData2 <- data.frame(x,y)

  # Bootstrapping to derive each sampling distribution
  # for number of MC simulations with value k
  sample_distri <- BootStrap(regData2,nBoot = 1000)
  
  # Store variance of sample distribution for this iteration
 
  sample_var_final[which(sample_var_final$number_of_simulations == k),]$variance <- var(sample_distri)
}
end <- Sys.time()
end-start

# Parallelise
nCores <- detectCores() # no. of cores
cl <- makeCluster(spec = nCores, type = "PSOCK")
registerDoParallel(cl)

start <- Sys.time()
sample_var_final <- foreach(k = nSim, .combine='rbind', .multicombine=TRUE) %dopar% {
  set.seed(45214)           # for reproducibility
  x <- rnorm(k,4,sqrt(10))  # create the "observed" data for x
  y <- runif(k,2,8)         # create the "observed" data for y
  regData2 <- data.frame(x,y)
  sample_distri <- BootStrap(regData2,nBoot = 1000)
  number_of_simulations <- k
  variance <- var(sample_distri)
  data.frame(number_of_simulations, variance)
}
end <- Sys.time()
end-start
stopCluster(cl)

# Plot the graph of variance against number of simulations
ggplot(sample_var_final) +
  geom_point(aes(x = 1/number_of_simulations, y = variance), colour = "orange") +
  geom_smooth(aes(x = 1/number_of_simulations,y = variance), method = "lm") +
  xlab("Inverse number of Monte Carlo simulations") +
  ylab("Variance of sample distribution") +
  ggtitle("Sample variance against number of simulations")

ggplot(sample_var_final) +
  geom_point(aes(x = number_of_simulations, y = variance), colour = "orange") +
  geom_smooth(aes(x = number_of_simulations,y = variance), se = FALSE) +
  xlab("Number of Monte Carlo simulations") +
  ylab("Variance of sample distribution") +
  ggtitle("Sample variance against number of simulations")





# Test run times
times <- rep(NA,10)

for(i in 1:10){
  
start <- Sys.time()
  for (k in nSim){
    
    # Simulate observed data
    set.seed(45214)           # for reproducibility
    x <- rnorm(k,4,sqrt(10))  # create the "observed" data for x
    y <- runif(k,2,8)         # create the "observed" data for y
    regData2 <- data.frame(x,y)
    
    # Bootstrapping to derive each sampling distribution
    # for number of MC simulations with value k
    sample_distri <- BootStrap(regData2,nBoot = 1000)
    
    # Store variance of sample distribution for this iteration
    
    sample_var_final[which(sample_var_final$number_of_simulations == k),]$variance <- var(sample_distri)
  }
  end <- Sys.time()
  times[i] <- end-start
}
avgNP <- mean(times)


# Parallelise
times <- rep(NA,10)
for (i in 1:10){
  nCores <- detectCores() # no. of cores
  cl <- makeCluster(spec = nCores, type = "PSOCK")
  registerDoParallel(cl)
  
  start <- Sys.time()
  sample_var_final <- foreach(k = nSim, .combine='rbind', .multicombine=TRUE) %dopar% {
    set.seed(45214)           # for reproducibility
    x <- rnorm(k,4,sqrt(10))  # create the "observed" data for x
    y <- runif(k,2,8)         # create the "observed" data for y
    regData2 <- data.frame(x,y)
    sample_distri <- BootStrap(regData2,nBoot = 1000)
    number_of_simulations <- k
    variance <- var(sample_distri)
    data.frame(number_of_simulations, variance)
  }
  end <- Sys.time()
  times[i] <- end-start
  stopCluster(cl)
}
avgP <- mean(times)