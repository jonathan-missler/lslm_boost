#'@importFrom stats fitted

gradient_gamma <- function(m) {
  
  m$v <- ((m$y - fitted(m,"location"))^2)/exp(fitted(m,"scale"))^2 - 1
  
  m
}
