#### Setup
## Some required libraries
if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}
## Source the required files
# Todo insert here

#### PART 1: Regression on non linear data
## y = 2x^3 - x^2 + 3x - 33
theta <- matrix(c(-33, 3, 1, 2))
genYFunction <- function(t) {
  genY <- function(x) {
    return((t[4] * (x ^ 3)) + (t[3] * (x ^ 2)) + (t[2] *  x) - t[1])
  }
  return(genY)
}

## Generate some data
sampleSize <- 1000
set.seed(5)
# For the x values we will be generating for
x <- runif(sampleSize, min=-20, max=20)
realFunction <- genYFunction(theta)
y <- sapply(x, realFunction) + rnorm(length(x), mean=32, sd=3000)

## Plot the data
# Store in a dataframe for plotting
dataDf <- data.frame(x=x, y=y)

fitP <- ggplot(dataDf, aes(x, y)) +
  geom_point(colour="dodgerblue",alpha=0.75) +
  stat_function(fun = realFunction, aes(colour = "black")) +
  scale_color_identity(labels=c("True Line"), guide="legend") +
  ggtitle("Synthetic Data (Polynomial)") +
  labs(y="The Y value", x = "The X feature")
fitP

## Fit using R's built in linear regression function
linReg <- lm(y ~ x)

fitP <- fitP +
  geom_abline(aes(colour="green", intercept=linReg$coefficients[1], slope=linReg$coefficients[2]), alpha=0.8, size=1) +
  scale_color_identity(labels=c("True Line", "Lin Regression Line"), guide="legend")
fitP

## Fit using R's built something something
nonlinReg <- lm(y ~ x + I(x^2) + I(x^3))

polyFitFunction <- genYFunction(nonlinReg$coefficients)
fitP <- fitP +
  stat_function(fun = polyFitFunction, aes(colour = "red")) +
  scale_color_identity(labels=c("True Line", "Lin Regression Line", "Poly Regression Line"), guide="legend")
fitP

## Sum of Squares Error and Mean Squared Error
# Real Line
theta
polySumOfSquaresCost(x, y, realFunction)
polySumOfSquaresCost(x, y, realFunction) / length(x)
# Maximum Likelihood Estimator
mleTheta <- matrix(c(linReg$coefficients))
mleTheta
sumOfSquaresCost(x, y, mleTheta)
sumOfSquaresCost(x, y, mleTheta) / length(x)
# Polynomial Regression
nonlinReg$coefficients
polySumOfSquaresCost(x, y, polyFitFunction)
polySumOfSquaresCost(x, y, polyFitFunction) / length(x)

# We see from the graph and the values of these errors, linear regression performed poorly on the non linear data

## Other metrics

summary(nonlinReg)

# Well looks like that x is pretty significant

