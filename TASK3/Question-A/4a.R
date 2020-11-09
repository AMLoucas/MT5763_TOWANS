# Group project
# Monte Carlo simulation

library(tidyverse)

# Question 1 - Compute the probability

# Simulate the observed data
set.seed(45214)    # for reproducibility
N <- 100           # Specify the number of datasets to be simulated
x <- rnorm(N,4,sqrt(10)) # create the "observed" data for x
y <- runif(N,2,8)  # create the "observed" data for y

# calculate the probability
p <- sum(x > y)/N
print(p)


#Question 2 - Bootstrapping

# Put the observed data in data frame
obsData <- data.frame(x,y)

# Initialisation
set.seed(56337)    # for reproducibility
NRepeat <- 1000    # no. of bootstrapped samples
bootRes <- matrix(data = NA, nrow = NRepeat, ncol = 1) # results

# Loop across all samples
for (i in seq(NRepeat)) {
  
  # Resample with replacement
  bootData <- obsData[sample(x = N, size = N, replace = T), ]
  
  # Find the number for x>y
  tot <- sum(bootData[,1] > bootData[,2])
  
  # Store results
  bootRes[i] <- tot/N
}


# Plot the histogram
ggplot(data.frame(bootRes)) +
  geom_histogram(aes(bootRes), colour = "black", fill = "orange", alpha = 0.1) +
  geom_vline(xintercept = p, col="purple", lwd=1) +
  ggtitle("Histogram of Pr(X>Y)") +
  xlab("Pr(X > Y)") +
  ylab("Frequency")


# Question 3 - how sample variance varies with No. of simulations


# Create empty data frame to append estimations to
sample_var_final <- data.frame(number_of_simulations = 0, variance = 0)

# Loop through varying numbers of simulations
for (k in seq(500,5000,10)){
  
  # Simulate observed data
  set.seed(45214)    # for reproducibility
  x <- rnorm(k,4,sqrt(10)) # create the "observed" data for x
  y <- runif(k,2,8)  # create the "observed" data for y
  obsData <- data.frame(x,y)
  
  # Bootstrapping
  set.seed(56337)    # for reproducibility
  NRepeat <- 1000    # no. of bootstrapped samples
  bootRes <- matrix(data = NA, nrow = NRepeat, ncol = 1) # results
  
  # Loop across all samples
  for (i in seq(NRepeat)) {
    
    # Resample with replacement
    bootData <- obsData[sample(x = k, size = k, replace = T), ]
    
    # Find the number for x>y
    tot <- sum(bootData[,1] > bootData[,2])
    
    # Store results
    bootRes[i] <- tot/k
  }
  
  # Store variance of sample distribution for this iteration
  sample_var <- data.frame(number_of_simulations = k, variance = var(bootRes))
  
  # append this iteration to the final table
  sample_var_final <- rbind(sample_var_final,sample_var)
}

# adjust the final table
sample_var_final <- sample_var_final[-1,]


# Plot the graph of variance against number of simulations
ggplot(sample_var_final) +
  geom_point(aes(x = 1/number_of_simulations, y = variance), colour = "orange") +
  geom_smooth(aes(x = 1/number_of_simulations,y = variance), method = lm) +
  xlab("Inverse of number of Monte Carlo simulations") +
  ylab("Variance of sample distribution") +
  ggtitle("Sample variance against number of simulations")

# build the linear model for sample variance and inverse of number of simulations
mdl <- lm(variance ~ 1/number_of_simulations, data = sample_var_final)

# find 95% confidence intervals
confint(mdl, level = 0.95)

# Model checking
# Checking plots
par(mfrow = c(2,2))
plot(mdl)
