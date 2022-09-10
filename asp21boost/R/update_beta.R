update_beta <- function(m, j_star, nu){
  beta_est <- beta_est(m)
  beta_vec <- m$coefficients$location
  beta_vec[j_star] <- beta_vec[j_star] + nu*beta_est[j_star]
  m$coefficients$location <- beta_vec
  m$fitted.values$location <- m$fitted.values$location + nu * m$x[ ,j_star] * beta_est[j_star]


  m
}
