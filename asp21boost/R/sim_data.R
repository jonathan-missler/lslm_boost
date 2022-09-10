#' Simulating data for Gaussian Location-Scale Regression models
#'
#' @description
#' Simulating a normally distributed random variable with a linear model as mean and a
#' log-linear model as standard deviation.
#'
#' @param n_obs number of observations to be generated.
#' @param n_var number of covariates for the linear models.
#' @param seed seed for random number generator.
#' @param frac_beta fraction of location coefficients that are to be set to 0.
#' @param frac_gamma fraction of scale coefficients that are to be set to 0.
#'
#' @return A list containing the objects:
#'  \itemize{
#'   \item y - gaussian random variable with mean X %*% beta_coef and standard deviation
#'         exp(X %*% gamma_coef)
#'   \item X - matrix of covariates used to generate y
#'   \item beta_coef - coefficients for location parameter
#'   \item gamma_coef - coefficients for scale parameter
#'  }
#'
#' @importFrom stats rnorm runif
#' @export
simulate_data <- function(n_obs, n_var, seed = NULL, frac_beta = 0.25, frac_gamma = 0.25){
  if(!is.null(seed)){
    set.seed(seed)
  }
  if(!is.numeric(c(n_obs,n_var,frac_beta,frac_gamma))){
    stop("parameters n_obs, n_var, frac_beta, frac_gamma must be numeric")
  }

  if(n_var<=0){
    stop("number of variables must be larger than zero")
  }

  if(n_var%%1 != 0){
    stop("number of variables must be a whole number")
  }

  if(n_obs<=1){
    stop("number of observations must be larger than one")
  }

  if(n_obs%%1 != 0){
    stop("number of observations must be a whole number")
  }

  if(frac_beta<0 || frac_beta>1){
    stop("value for frac_beta must be between zero and one")
  }

  if(frac_gamma<0 || frac_gamma>1){
    stop("value for frac_gamma must be between zero and one")
  }

  X <- matrix(rnorm(n_obs*n_var), nrow = n_obs, ncol = n_var)

  standardize <- function(df){
    return(apply(df, 2, function(x){ (x-mean(x))/sd(x)}))
  }

  X <- standardize(X)
  X <- cbind(rep(1,n_obs), X)


  beta_coef <- c(rep(0, floor(frac_beta*n_var)), runif(ceiling((1-frac_beta) * n_var), -10, 10),1)
  beta_coef <- sample(beta_coef)

  gamma_coef <- c(rep(0, floor(frac_gamma*n_var)), runif(ceiling((1-frac_gamma) * n_var), -0.001,0.001),0.001)
  gamma_coef <- sample(gamma_coef)

  y <- rnorm(n_obs, mean = X %*% beta_coef, sd = exp(X %*% gamma_coef))

  out_list <- list(
    y = y,
    X = X[,2:ncol(X)],
    beta_coef = beta_coef,
    gamma_coef = gamma_coef
  )

  return(out_list)
}


