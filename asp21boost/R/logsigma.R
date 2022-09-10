
#' @importFrom stats resid
logsigma <- function(m){
  out <- 0.6351814 + log(abs(resid(m)))
  return(out)
}

