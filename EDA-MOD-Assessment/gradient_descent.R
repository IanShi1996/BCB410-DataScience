gradDescent <- function(x, y, theta, costFun, costGrad, alpha, maxIter, verbose=FALSE) {
  cost <- c(length=maxIter + 1)
  cost[1] <- costFun(x, y, theta)
  iter <- 0
  while (iter < maxIter) {
    theta <- theta - (alpha * costGrad(x, y, theta))
    iterCost <- costFun(x, y, theta)
    cost[iter + 2] <- iterCost
    if (verbose) {
      print(sprintf("Iter: %f/%f, Cost: %f", iter + 1, maxIter, iterCost))
    }
    iter <- iter + 1
  }
  return(list("theta"=theta, "cost"=cost))
}
