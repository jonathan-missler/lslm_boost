
ols <- function(X,u, intercept = FALSE){
  if(intercept){
    X <- cbind(rep(1, length(u)), X)
    out <- solve(t(X)%*%X)%*%t(X)%*%u
    out <- out[2:ncol(X)]
  } else {
    out <- solve(t(X)%*%X)%*%t(X)%*%u
  }
  return(out)
}



beta_est <- function(m){
  beta_0 <- ols(m$x[,1], m$u)
  betas <- apply(m$x[,2:ncol(m$x)],2, FUN = ols, u = m$u, intercept=TRUE)

  return(c(beta_0,betas))
}

gamma_est <- function(m, intercept=FALSE){
  out <- apply(X = m$z,2, FUN = ols, u = m$v, intercept=intercept)

  return(out)
}




