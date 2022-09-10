#' @importFrom stats model.matrix update

setup <- function(location, scale, data, light, call, mstop, nu_b, nu_g) {
  scale <- update(scale, paste(location[[2]], "~."))
  y <- eval(location[[2]], data, environment(location))
  x <- model.matrix(location, data)
  z <- model.matrix(scale, data)

  nobs <- length(y)


  m <- structure(
    list(
      y               = y,
      x               = x,
      z               = z,
      nobs            = nobs,
      light           = light,
      call            = call,
      coefficients    = list(location = NULL, scale = NULL),
      fitted.values   = list(location = NULL, scale = NULL),
      residuals       = NULL,
      iterations      = NULL,
      u               = NULL,
      v               = NULL,
      mstop           = mstop,
      nu_b            = nu_b,
      nu_g            = nu_g
    ),
    class = "lslm_boost"
  )


  m
}

#' @importFrom stats logLik

estimate <- function(m, maxit = m$mstop, nu_b = m$nu_b, nu_g = m$nu_g) {


  if(!is.numeric(c(maxit,nu_b,nu_g))){
    stop("parameters maxit, nu_b, nu_g must be numeric")
  }

  if(maxit%%1 != 0){
    stop("number of iterations must be a whole number")
  }

  if(nu_b<=0 || nu_b>1){
    stop("step length nu_b must be between zero and one")
  }

  if(nu_g<=0 || nu_g>1){
    stop("step length nu_g must be between zero and one")
  }


  m <- init_beta(m)
  m <- init_gamma(m)
  m$fitted.values$location <- m$x %*% m$coefficients$location
  m$fitted.values$scale <- m$z %*% m$coefficients$scale


  m <- gradient_beta(m)
  m <- gradient_gamma(m)

  it <- 0

  while (it < maxit) {
    it <- it + 1

    m <- update_beta(m, bestfit_beta(m), nu_b)
    m <- update_gamma(m,bestfit_gamma(m),nu_g)

    m <- gradient_beta(m)
    m <- gradient_gamma(m)
  }


  m$iterations <- it
  print("Finished.")

  m
}

finish <- function(m) {
  if (m$light) {
    m$x <- m$z <- NULL
  }

  m$residuals <- m$y - fitted(m, "location")

  m
}

#' Componentwise gradient boosting in location-scale regression
#'
#' @description
#'
#' The location-scale regression model assumes a normally distributed response
#' variable with one linear predictor for the mean (= the location) and one for
#' the standard deviation (= the scale). The standard deviation is mapped to
#' the linear predictor through a log link.
#'
#' This function sets up the model object and estimates it with componentwise gradient boosting.
#'
#'
#' @param location A two-sided formula with the response variable on the LHS
#'                 and the predictor for the mean on the RHS.
#' @param scale A one-sided formula with the predictor for the standard
#'              deviation on the RHS.
#' @param data A data frame (or list or environment) in which to evaluate
#'             the `location` and `scale` formulas.
#' @param light If `TRUE`, the design matrices are removed from the estimated
#'              model to save some memory. Has to be `FALSE` if the model is
#'              to be used in cross validation.
#' @param mstop Integer denoting the number of iterations the algorithm. Defaults to 1000.
#' @param nu_b Learning rate for the location coefficients. Defaults to 0.1.
#' @param nu_g Learning rate for the scale coefficients. Defaults to 0.01.
#'
#' @export

lslm_boost <- function(location,
                       scale = ~1,
                       data = environment(location),
                       light = FALSE,
                       mstop = 1000,
                       nu_b = 0.1,
                       nu_g = 0.01) {
  m <- setup(location, scale, data, light, match.call(), mstop, nu_b, nu_g)
  m <- estimate(m)
  m <- finish(m)
  m
}
