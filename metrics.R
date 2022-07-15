rmse <- function(predY, trueY) {
  val <- sqrt(mean((predY - trueY)^2))
  return(val)
}

R2 <- function(predY, trueY) {
  ssr <- sum((trueY - predY)^2)
  sst <- sum((trueY - mean(trueY))^2)
  r2 <- 1 - ssr/sst
  return(r2)
}

