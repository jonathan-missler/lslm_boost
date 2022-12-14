# derivatives -----------------------------------------------------------------

#' @importFrom stats fitted resid

score_beta <- function(m) {
  drop((resid(m) / fitted(m, "scale")^2) %*% m$x)
}

#' @importFrom stats resid

score_gamma <- function(m) {
  drop((resid(m, "pearson")^2 - 1) %*% m$z)
}

#' @importFrom stats fitted

info_beta <- function(m) {
  crossprod(m$x / fitted(m, "scale")^2, m$x)
}

info_gamma <- function(m) {
  2 * crossprod(m$z)
}

# mcmc helpers ----------------------------------------------------------------

set_beta <- function(m, beta) {
  m$coefficients$location <- beta
  m$fitted.values$location <- drop(m$x %*% beta)
  m$residuals <- m$y - m$fitted.values$location
  m
}

set_gamma <- function(m, gamma) {
  m$coefficients$scale <- gamma
  m$fitted.values$scale <- exp(drop(m$z %*% gamma))
  m
}

set_coef_funs <- list(
  location = set_beta,
  scale = set_gamma
)

set_coef <- function(m, coef, predictor) {
  set_coef_funs[[predictor]](m, coef)
}

score_funs <- list(
  location = score_beta,
  scale = score_gamma
)

score <- function(m, predictor) {
  score_funs[[predictor]](m)
}

chol_info_funs <- list(
  location = function(m) chol(info_beta(m)),
  scale = function(m) m$chol_info_gamma
)

chol_info <- function(m, predictor) {
  chol_info_funs[[predictor]](m)
}

# multivariate normal distribution --------------------------------------------

#' @importFrom stats rnorm

rmvnorm <- function(n, mu = 0, chol_sig_inv) {
  dim <- nrow(chol_sig_inv)

  std_norm <- matrix(rnorm(dim * n), dim, n)
  scaled <- backsolve(chol_sig_inv, std_norm)
  shifted <- scaled + mu

  shifted
}

#' @importFrom stats dnorm

dmvnorm <- function(x, mu = 0, chol_sig_inv, log = FALSE) {
  std_norm <- drop(chol_sig_inv %*% (x - mu))
  correction <- sum(log(diag(chol_sig_inv)))

  log_prob <- sum(dnorm(std_norm, log = TRUE)) + correction

  if (log) {
    log_prob
  } else {
    exp(log_prob)
  }
}

# summary helpers -------------------------------------------------------------

#' @importFrom generics tidy

coefmat <- function(m, predictor) {
  m <- tidy(m, predictor)
  out <- as.matrix(m[c("estimate")])
  colnames(out) <- c("Estimate")
  rownames(out) <- m$term
  out
}

















