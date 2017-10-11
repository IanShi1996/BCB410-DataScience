#### Setup
## Some required libraries
if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Let's generate some data
# for decision boundary y = 22x - 2
# where anything above the line is labeled as 1 while below is 0
theta <- c(-2, 22)

genY <- function(x) {
  return(theta[1] + (theta[2] * x))
}

## Generate some data
sampleSize <- 400
set.seed(5)
# For the x values we will be generating for
x <- runif(sampleSize, min=-20, max=20)
y <- sapply(x, genY) + rnorm(length(x), mean=0, sd=10)

# From the x and y generated, find out their label
goodLab <- vector(length=length(x))
shapeLab <- vector(length=length(x))
for (i in 1:length(x)) {
  tmp <- theta[1] + (theta[2] * x[i])
  if (y[i] > tmp) {
    goodLab[i] <- 1
    shapeLab[i] <- "class 1"
  } else {
    goodLab[i] <- 0
    shapeLab[i] <- "class 2"
  }
}

# Generate a bad label for a bad decision boundary
# Classify everything to the right of the 0 axis to be 1
# while left to be 0
badLab <- vector(length=length(x))
for (i in 1:length(x)) {
  if (x[i] > 0) {
    badLab[i] <- 1
  } else {
    badLab[i] <- 0
  }
}

goodPlot <- qplot(x, y, colour = goodLab, shape = shapeLab)
goodPlot

badPlot <- qplot(x, y, colour = badLab, shape = shapeLab)
badPlot

