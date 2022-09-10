#'@importFrom stats fitted

gradient_beta <- function(m) {

  m$u <- (1/exp(fitted(m,"scale"))^2) * (m$y - fitted(m,"location"))

  m
}



