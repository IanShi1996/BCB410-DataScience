sumOfSquaresCost <- function(x, y, theta) {
  X <- matrix(c(1), ncol=length(x))
  X <- rbind(X, x)
  return(sum((y - (t(theta) %*% X)) ^ 2))
}

sumOfSquaresGrad <- function(x, y, theta) {
  X <- matrix(c(1), ncol=length(x))
  X <- rbind(X, x)
  tmp <- y - (t(theta) %*% X)
  rowWise <- sweep(X,MARGIN=2, tmp,`*`)
  return(-2 * matrix(rowSums(rowWise)))
}

sigmoidFunction <- function(x) {
  return(1/(1 + exp(-x)))
}

logisticCost <- function(x, y, theta) {
  X <- matrix(c(1), ncol=length(x))
  X <- rbind(X, x)
  h <- sapply(t(theta) %*% X, sigmoidFunction)
  totalCost <- 0
  for (i in 1:length(h)) {
    elementCost <- -(y[i] * log(h[i])) - ((1 - y) * log(1 - h[i]))
    totalCost <- totalCost + elementCost
  }
  return(totalCost)
}

logisticCostGrad <- function(x, y, theta) {
  X <- matrix(c(1), ncol=length(x))
  X <- rbind(X, x)
  h <- sapply(t(theta) %*% X, sigmoidFunction)
  tmp <- h - y
  rowWise <- sweep(X,MARGIN=2, tmp,`*`)
  return(matrix(rowSums(rowWise)))
}

polySumOfSquaresCost <- function(x, y, genFunction) {
  return(sum((y - sapply(x, genFunction)) ^ 2))
}
