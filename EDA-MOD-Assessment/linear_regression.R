#### Setup
## Some required libraries
if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}
## Source the required files
# Todo insert here

#### PART 1: Regular Linear Regression

## For a real formula y = 22x - 35
theta <- matrix(c(-35, 22))

## Generate some data
sampleSize <- 1000
set.seed(5)
# For the x values we will be generating for
x <- runif(sampleSize, min=-20, max=20)
M <- matrix(c(1), ncol=length(x))
M <- rbind(M, x)

# Generate points from line with normal noise
y <- c(t(theta) %*% M + rnorm(length(x), mean=10, sd=100))

## Plot the data
# Store in a dataframe for plotting
dataDf <- data.frame(x=x, y=y)

fitP <- ggplot(dataDf, aes(x, y)) +
  geom_point(colour="dodgerblue",alpha=0.75) +
  geom_abline(aes(colour="green", intercept=theta[1], slope=theta[2]), alpha=0.8, size=1) +
  scale_color_identity(labels=c("True Line"), guide="legend") +
  ggtitle("Synthetic Data (Linear Data)") +
  labs(y="The Y value", x = "The X feature")
fitP

## Fit using R's built in linear regression function (Maximum Likelihood Estsimator)
linReg <- lm(y ~ x)

fitP <- fitP +
  geom_abline(aes(colour="black", intercept=linReg$coefficients[1], slope=linReg$coefficients[2]), alpha=0.8, size=1) +
  scale_color_identity(labels=c("Regression Line", "True Line"), guide="legend")
fitP

# Wow pretty close!

## Fit using gradient descent

initTheta <- matrix(c(1, 2))
gradResults <- gradDescent(x, y, initTheta, sumOfSquaresCost, sumOfSquaresGrad, 3E-9, 5000)
fitP <- fitP +
  geom_abline(aes(colour="red", intercept=gradResults$theta[1], slope=gradResults$theta[2]), alpha=0.8, size=1) +
  scale_color_identity(labels=c("Regression Line", "Real Line", "Gradient Descent"), guide="legend")
fitP

# Again pretty close!

# Check the learning curves

gradDf <- data.frame(iter=1:length(gradResults$cost), cost=gradResults$cost)
costP <- ggplot(gradDf, aes(iter, cost)) +
  geom_point(colour="red",alpha=0.75, shape=42) +
  ggtitle("Gradient Descent: Cost vs Iterations") +
  labs(y="Sum of Squares", x = "Iterations")
costP

# That's a smooth descent!

## Sum of Squares Error and Mean Squared Error
# Real Line
theta
sumOfSquaresCost(x, y, theta)
sumOfSquaresCost(x, y, theta) / length(x)
# Maximum Likelihood Estimator
mleTheta <- matrix(c(linReg$coefficients))
mleTheta
sumOfSquaresCost(x, y, mleTheta)
sumOfSquaresCost(x, y, mleTheta) / length(x)
# Gradient Descent
gradResults$theta
sumOfSquaresCost(x, y, gradResults$theta)
sumOfSquaresCost(x, y, gradResults$theta) / length(x)

## Other metrics

# We can look at the summary of the linear regression
summary(linReg)

# Well looks like that x is pretty significant

