#Determine the best fitting variable

bestfit_beta <- function(m) {
  x <- m$x
  u <- m$u
  beta_est <- beta_est(m)
  j_poss <- rep(0, length(beta_est))
  for(j in 1:ncol(x)) {
    a <- (u - x[,j] * beta_est[j])^2
    j_poss[j] <- sum(a)
  }
  j_star <- which(j_poss == min(j_poss))[1]
  return(j_star)
}

