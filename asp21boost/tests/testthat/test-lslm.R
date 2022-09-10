library(usethis)
library(devtools)
library(asp21boost)
library(testthat)
library(gamboostLSS)


showcaselist <- simulate_data(100,2,234)
showcasedata <- data.frame(cbind(showcaselist$y, showcaselist$X))
testgl <- glmboostLSS(X1~., data = showcasedata,control = boost_control(mstop = 1000, nu = 0.1))

###integration tests
testmod <- lslm_boost(X1 ~., ~showcaselist$X, data = showcasedata, mstop = 7000, nu_b = 0.1, nu_g = 0.1)

test_that("lmls() location estimates are close to true values", {
  expect_roughly(as.vector(testmod$coefficients$location), showcaselist$beta_coef)
})

test_that("lmls() scale estimates are close to true values", {
expect_roughly(as.vector(testmod$coefficients$scale), showcaselist$gamma_coef)
})



###Unit tests

testmod <- lslm_boost(X1~., ~showcaselist$X,
                      data = showcasedata, mstop = 1000, nu_g=0.1)

##mit gamboostlss vergleichen
test_that("lslm_boost() estimates are close to true values", {
  out_mu <- as.vector(c(coef(testgl)$mu[1]+attributes(coef(testgl)$mu)$offset,coef(testgl)$mu[2],coef(testgl)$mu[3]))
  out_sigma <- as.vector(c(coef(testgl)$sigma[1]+attributes(coef(testgl)$sigma)$offset,coef(testgl)$sigma[2],coef(testgl)$sigma[3]))
  expect_roughly(as.vector(coef(testmod, "location")), out_mu) 
  expect_roughly(as.vector(coef(testmod, "scale")), out_sigma)
})


####test correct list of names
test_that("setup() creates list with correct names", {
  n <- c("y", "x", "z", "nobs", "light",
         "call", "coefficients", "fitted.values", "residuals",
         "iterations", "u","v","mstop","nu_b","nu_g")
  
  expect_true(all(n %in% names(testmod)))
})


#### test setup() creates list with correct values

test_that("setup() creates list with correct values", {
  expect_equal(testmod$y, showcaselist$y)
  
  expect_exactly(matrix(testmod$x, ncol=3), cbind(1, showcaselist$X))
  expect_exactly(matrix(testmod$z, ncol=3), cbind(1, showcaselist$X))
  
  expect_equal(testmod$nobs, 100)
  
})

####test sets class attribute
test_that("setup() sets class attribute", {
  expect_s3_class(testmod, "lslm_boost")
})

test_that("setup() argument 'data' works with data frame", {
  dat <- data.frame(y = showcaselist$y, x2 = showcaselist$X[,1], x1 = showcaselist$X[,2])
  m <- setup(y ~ x2 + x1, ~ x2 + x1, data = dat, light=T,call=NULL,mstop=4,nu_b=0.1,nu_g=0.01)
  
  expect_error(m, NA)
})

test_that("setup() argument 'data' works with list", {
  dat <- list(y = showcaselist$y, x2 = showcaselist$X[,1], x1 = showcaselist$X[,2])
  m <- setup(y ~ x2 + x1, ~ x2 + x1, data = dat, light=T,mstop=4,call=NULL,nu_b=0.1,nu_g=0.01)
  
  expect_error(m, NA)
})

m <- estimate(testmod)

test_that("estimate() stores number of iterations", {
  expect_true(is.numeric(m$iterations))
})
