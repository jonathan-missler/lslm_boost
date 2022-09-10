#Determine the best fitting variable

bestfit_gamma <- function(m) {
  v <- m$v
  z <- m$z
  gamma_est <- gamma_est(m)
  j_poss <- rep(0, length(gamma_est))
  for(j in 1:ncol(z)) {
    a <- (v - z[,j] * gamma_est[j])^2
    j_poss[j] <- sum(a)
  }
  j_star <- which(j_poss == min(j_poss))[1]
  return(j_star)
}
