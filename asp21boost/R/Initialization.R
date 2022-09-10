#initialization
init_beta <- function(m) {
  y <- m$y
  x <- m$x
  beta_0 <- mean(y)
  beta_j <- rep(0, ncol(x) - 1)
  m$coefficients$location <- matrix(c(beta_0 , beta_j),ncol=1)
  m$residuals <- y - x%*%m$coefficients$location
  m
}

init_gamma <- function(m) {
  y <- m$y
  z <- m$z
  gamma_0 <- mean(logsigma(m))
  gamma_j <- rep(0, ncol(z) - 1)
  m$coefficients$scale <- c(gamma_0 , gamma_j)

  m
}
