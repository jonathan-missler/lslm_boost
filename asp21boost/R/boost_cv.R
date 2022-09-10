#' Log likelihood based cross validation for iterations in location-scale
#' boosting models
#'
#' @description
#'
#' k-fold cross validation for componentwise gradient boosting in location-scale
#' regression models. The selection criterion is the log likelihood function of
#' the underlying model.
#'
#' The function outputs the resulting average log likelihood over all folds for
#' each parameter as well as the value of the parameter with which the maximum
#' likelihood was achieved.
#'
#' @param m An object of class `lslm_boost` containing the relevant data and
#'          correct settings for the learning rates.
#' @param k Number of folds in k-fold cross validation.
#' @param mstops Vector of parameters containing positive integer values.
#' @param seed Seed for random number generation.
#'
#' @return A list containing numeric values:
#'  \itemize{
#'   \item mean_logliks - The values of log likelihood given the parameter mstop,
#'         averaged over all folds.
#'   \item max_loglik - The value of the biggest average log likelihood.
#'   \item max_mstop - Number of iterations that maximizes the log likelihood.
#' }
#'
#' @importFrom caret createFolds
#' @export

boost_CV <- function(m,
                     k=5,
                     mstops,
                     seed = NULL) {
  if(class(m) != "lslm_boost"){
    stop("object not of type 'lslm_boost'.")
  }

  if(floor(k) != k | k <= 1){
    stop("k has to be a positive integer larger than 1.")
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  folds <- createFolds(m$y, k)
  unl <- unlist(folds)

  resmat <- matrix(rep(NA, k*length(mstops)), nrow = k, ncol = length(mstops))

  X <- m$x
  y <- m$y
  Z <- m$z

  temp <- m

  for(i in 1:k){
    test_idx <- folds[[i]]
    train_idx <- as.vector(subset(unl, !(unl %in% test_idx)))

    temp$x <- m$x[train_idx,]
    temp$z <- m$z[train_idx,]
    temp$y <- m$y[train_idx]

    for(j in 1:length(mstops)){
      temp <- estimate(temp, maxit = mstops[j])
      fitloc <- X[test_idx,] %*% temp$coefficients$location
      fitscale <- exp(Z[test_idx,] %*% temp$coefficients$scale)

      resmat[i,j] <- sum(dnorm(y[test_idx], fitloc, fitscale, log = TRUE))
    }
  }
  avgres <- apply(resmat, 2, mean)
  max_loglik <- avgres[which(avgres == max(avgres))]
  max_mstop <- mstops[which(avgres == max(avgres))]

  outlist <- list(
              mean_logliks = avgres,
              max_loglik = max_loglik,
              max_mstop = max_mstop
              )

  return(outlist)
}
