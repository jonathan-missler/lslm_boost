update_gamma <- function(m, j_star, nu){
  gamma_est <- gamma_est(m)
  gamma_vec <- m$coefficients$scale
  gamma_vec[j_star] <- gamma_vec[j_star] + nu*gamma_est[j_star]
  m$coefficients$scale <- gamma_vec
  m$fitted.values$scale <- m$fitted.values$scale + nu * m$z[ ,j_star] * gamma_est[j_star]


  m
}
