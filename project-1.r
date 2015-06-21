library("knitr")
library("plyr")
library("stringr")
library("ggplot2")
require("ggplot2")
opts_chunk$set(echo = TRUE)
opts_chunk$set(cache = TRUE)
opts_chunk$set(warnings = FALSE)

# Step 1: Generate an exponential distribution for 40 exponentials with a rate of 'lambda' = 0.2 (we assumed lambda == 0.2).

# Ensure that the random numbers are always the same by setting the seed
set.seed(3890) 
numberOfSimulations <- 1000
numberOfExponentials <- 40
lambda <- 0.2


# Using the above values, generate 1000 sets of simulations and store them in a dataframe.
simulationSetDF <- data.frame(mean=(numberOfSimulations))
for(simulationIndex in 1:numberOfSimulations){
  thisSimulationSet <- rexp(numberOfExponentials, lambda)
  simulationSetDF[simulationIndex, 1]<-mean(thisSimulationSet)
}
# Sample contents
head(simulationSetDF)

# Sample Mean
sampleMean <- mean(simulationSetDF$mean)
sampleMean

# Theoretical Mean
theoreticalMean <- 1/lambda
theoreticalMean

# plot histogram
hist(simulationSetDF$mean, probability = TRUE, main = "Distribution of Simulated and Theoretical Means", xlab = "Mean of 40 Exponentials")

# plot the density curve
lines(density(simulationSetDF$mean), col="red", lwd=5)

# plot theoretical mean
abline(v=theoreticalMean, col="yellow", lwd=5)

# capture first 100 means between the range of simulationSet 
sequenceOfMeans <- seq(min(simulationSetDF$mean), max(simulationSetDF$mean), length=100)

# capture the density
densities <- dnorm(sequenceOfMeans, mean=theoreticalMean, sd=theoreticalMean/sqrt(numberOfExponentials))

# plot theoretical density curve
lines(sequenceOfMeans,densities, col="blue", lwd=5)

# add legend
legend('topright',c('Simulated Density Curve','Theoretical Density Curve','Theoretical Mean'),cex=0.8,col=c('red','blue','yellow'),lty=1,lwd=5)


#### Analysis 2: How variable the sample is (via variance) and compare it to the theoretical variance of the distribution.

# Calculate Theoretical Variance
theoreticalVariance <- ((1/lambda)^2) / numberOfExponentials
theoreticalVariance

# Calculate Sample Variance
sampleVariance <- var(simulationSetDF$mean)
sampleVariance

# We observe that the sample variance and theoretical variances are very close.

# Analysis 3: Show that the distribution is approximately normal.
# Referring to the plotted histgram we observe that both sample and theoretical distributions are approximately normal

# Appendix
R-Source code for this project: https://github.com/onlinebaba/statistical-inference/blob/master/project-1.r
