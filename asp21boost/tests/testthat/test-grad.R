library(numDeriv)
library(usethis)
library(devtools)
library(asp21boost)
library(testthat)
library(lmls)

### unit test for gradient functions

tst <- simulate_data(100,2,seed = 234)
x1 <- tst$X[,1]
x2 <- tst$X[,2]
y <- tst$y

m <- lmls(y ~ x1 + x2, ~ x1 + x2, light = FALSE)

f_b <- function(eta) {
  -sum(dnorm(m$y, eta , exp(fitted(m,"scale")), log = TRUE))
}

f_g <- function(zeta) {
  -sum(dnorm(m$y, fitted(m,"location"), exp(zeta), log = TRUE))
}

test_that("gradient of beta is correct", {
  num_score <- numDeriv::grad(f_b, fitted(m,"location"))
  out <- gradient_beta(m)
  expect_exactly(as.vector(out$u), -num_score)
})

test_that("gradient of gamma is correct", {
  num_score <- numDeriv::grad(f_g, fitted(m,"scale"))
  out <- gradient_gamma(m)
  expect_exactly(as.vector(out$v), -num_score)
})
