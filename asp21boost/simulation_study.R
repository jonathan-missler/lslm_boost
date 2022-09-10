library(asp21boost)
library(gamboostLSS)
library(lmls)



showcaselist <- simulate_data(1000,5,1866)
showcasedata <- data.frame(cbind(showcaselist$y, showcaselist$X))
showcasemodel <- lslm_boost(X1~.,~X2+X3+X4+X5+X6,data = showcasedata, mstop = 3700)
showcaseml <- lmls(X1~.,~X2+X3+X4+X5+X6,data = showcasedata)

coef(showcasemodel)
coef(showcaseml)
showcaselist$beta_coef
showcaselist$gamma_coef


showcaseCV <- boost_CV(showcasemodel, 5, c(500, 1000, 5000, 10000, 20000), 1337)
showcaseCVtest <- boost_CV(showcasemodel, 10, c(1000, 5000, 10000), 1337)

bigdatalist <- simulate_data(150,20, 1866)
bigdata <- data.frame(cbind(bigdatalist$y,bigdatalist$X))
bigdatamodel <- lslm_boost(X1~., ~bigdatalist$X,
                           data = bigdata, mstop = 10)
bigdataml <- lmls(X1~.,~bigdatalist$X,data = bigdata)

bigdatalss <- glmboostLSS(X1~., data = bigdata,
                          control = boost_control(mstop = 10000, nu = 0.1))
coef(bigdatalss)

bigdataCV <- boost_CV(bigdatamodel, 5, c(1000, 10000, 20000, 35000, 50000), 1337)

bigmodel <- lslm_boost(X1~., ~bigdatalist$X,
                       data = bigdata, mstop = 35000)





#---------------FAVORABLE FOR BOOST-------------------------------------------

###############################################################################
MSE <- function(est, true){
  resmat <- sum((est - true)^2)/(prod(dim(est)))
  return(resmat)
}

simlist <- list()
for(i in 1:20){
  simlist[[i]] <- simulate_data(100,20, i*123)
}

betamat <- matrix(NA, 21, 20)
for(j in 1:length(simlist)){
  betamat[,j] <- simlist[[j]]$beta_coef
}

gammamat <- matrix(NA, 21, 20)
for(j in 1:length(simlist)){
  gammamat[,j] <- simlist[[j]]$gamma_coef
}

simdatalist <- list()
for(i in 1:length(simlist)){
  simdatalist[[i]] <- data.frame(cbind(simlist[[i]]$y,simlist[[i]]$X))
}

modellist <- list()
for(i in 1:20){
  modellist[[i]] <- lslm_boost(X1~., ~simlist[[i]]$X, 
                               data = simdatalist[[i]], mstop = 35000)
}

betaest <- matrix(NA, 21, 20)
for(j in 1:length(modellist)){
  betaest[,j] <- modellist[[j]]$coefficients$location
}

gammaest <- matrix(NA, 21, 20)
for(j in 1:length(modellist)){
  gammaest[,j] <- modellist[[j]]$coefficients$scale
}



MLlist <- list()
for(i in 1:20){
  MLlist[[i]] <- lmls(X1~., ~simlist[[i]]$X, data = simdatalist[[i]], maxit = 10000)
}

betaML <- matrix(NA, 21, 20)
for(j in 1:length(modellist)){
  betaML[,j] <- MLlist[[j]]$coefficients$location
}

gammaML <- matrix(NA, 21, 20)
for(j in 1:length(modellist)){
  gammaML[,j] <- MLlist[[j]]$coefficients$scale
}




boostMSE_beta <- MSE(betaest, betamat)
boostMSE_gamma <- MSE(gammaest, gammamat)

MLMSE_beta <- MSE(betaML, betamat)
MLMSE_gamma <- MSE(gammaML, gammamat)



###############################################################################
loglikscore <- function(newdata, betacoef, gammacoef){
  y <- newdata[,1]
  X <- cbind(1,newdata[,2:ncol(newdata)])
  
  locfit <- X%*% betacoef
  scalefit <- exp(X %*% gammacoef)
  
  out <- sum(dnorm(y, locfit, scalefit, log = TRUE))
  
  return(out)
}

trainmodellist <- list()
trainMLlist <- list()
boostscorelist <- list()
MLscorelist <- list()

simlistloglik <- list()
simdatalistloglik <- list()

for(i in 1:20){
  simlistloglik[[i]] <- simulate_data(150,20, i*123)
}


for(i in 1:length(simlistloglik)){
  simdatalistloglik[[i]] <- data.frame(cbind(simlistloglik[[i]]$y,simlistloglik[[i]]$X))
}

for(i in 1:length(simdatalistloglik)){
  set.seed(1234*i)
  sample <- sample.int(n = nrow(simdatalistloglik[[i]]), 
                       size = floor(.75*nrow(simdatalistloglik[[i]])), replace = F)
  train <- data.frame(simdatalistloglik[[i]][sample, ])
  test  <- data.frame(simdatalistloglik[[i]][-sample, ])

  trainmodellist[[i]] <- lslm_boost(X1~., ~as.matrix(train[,2:ncol(train)]), 
                                    data = train, mstop = 35000)
  trainMLlist[[i]] <- lmls(X1~., ~as.matrix(train[,2:ncol(train)]), data = train, maxit = 1000)

  boostscorelist[[i]] <- loglikscore(as.matrix(test), trainmodellist[[i]]$coefficients$location, 
                                     trainmodellist[[i]]$coefficients$scale)
  MLscorelist[[i]] <- loglikscore(as.matrix(test), trainMLlist[[i]]$coefficients$location,
                                  trainMLlist[[i]]$coefficients$scale)
}

avgboostscore <- mean(unlist(boostscorelist))
avgmlscore <- mean(unlist(MLscorelist))

max(unlist(boostscorelist))
max(unlist(MLscorelist))

min(unlist(boostscorelist))
min(unlist(MLscorelist))



#--------------------FAVORABLE FOR ML------------------------------------------
###############################################################################
simlist2 <- list()
for(i in 1:20){
  simlist2[[i]] <- simulate_data(1000, 5, i*789)
}

betamat2 <- matrix(NA, 6, 20)
for(j in 1:length(simlist2)){
  betamat2[,j] <- simlist2[[j]]$beta_coef
}

gammamat2 <- matrix(NA, 6, 20)
for(j in 1:length(simlist2)){
  gammamat2[,j] <- simlist2[[j]]$gamma_coef
}

simdatalist2 <- list()
for(i in 1:length(simlist2)){
  simdatalist2[[i]] <- data.frame(cbind(simlist2[[i]]$y,simlist2[[i]]$X))
}

modellist2 <- list()
for(i in 1:20){
  modellist2[[i]] <- lslm_boost(X1~., ~simlist2[[i]]$X, 
                                data = simdatalist2[[i]], mstop = 5000)
}

betaest2 <- matrix(NA, 6, 20)
for(j in 1:length(modellist2)){
  betaest2[,j] <- modellist2[[j]]$coefficients$location
}

gammaest2 <- matrix(NA, 6, 20)
for(j in 1:length(modellist2)){
  gammaest2[,j] <- modellist2[[j]]$coefficients$scale
}



MLlist2 <- list()
for(i in 1:20){
  MLlist2[[i]] <- lmls(X1~., ~simlist2[[i]]$X, 
                       data = simdatalist2[[i]], maxit = 10000)
}

betaML2 <- matrix(NA, 6, 20)
for(j in 1:length(modellist2)){
  betaML2[,j] <- MLlist2[[j]]$coefficients$location
}

gammaML2 <- matrix(NA, 6, 20)
for(j in 1:length(modellist2)){
  gammaML2[,j] <- MLlist2[[j]]$coefficients$scale
}




boostMSE_beta2 <- MSE(betaest2, betamat2)
boostMSE_gamma2 <- MSE(gammaest2, gammamat2)

MLMSE_beta2 <- MSE(betaML2, betamat2)
MLMSE_gamma2 <- MSE(gammaML2, gammamat2)





###############################################################################
trainmodellist2 <- list()
trainMLlist2 <- list()
boostscorelist2 <- list()
MLscorelist2 <- list()

simlistloglik2 <- list()
simdatalistloglik2 <- list()

for(i in 1:20){
  simlistloglik2[[i]] <- simulate_data(1000,5, i*420)
}


for(i in 1:length(simlistloglik2)){
  simdatalistloglik2[[i]] <- data.frame(cbind(simlistloglik2[[i]]$y,simlistloglik2[[i]]$X))
}

for(i in 1:length(simdatalistloglik2)){
  set.seed(1234*i)
  sample <- sample.int(n = nrow(simdatalistloglik2[[i]]), 
                       size = floor(.75*nrow(simdatalistloglik2[[i]])), replace = F)
  train <- data.frame(simdatalistloglik2[[i]][sample, ])
  test  <- data.frame(simdatalistloglik2[[i]][-sample, ])
  
  trainmodellist2[[i]] <- lslm_boost(X1~., ~as.matrix(train[,2:ncol(train)]), 
                                     data = train, mstop = 5000)
  trainMLlist2[[i]] <- lmls(X1~., ~as.matrix(train[,2:ncol(train)]), 
                            data = train, maxit = 1000)
  
  boostscorelist2[[i]] <- loglikscore(as.matrix(test), trainmodellist2[[i]]$coefficients$location, 
                                     trainmodellist2[[i]]$coefficients$scale)
  MLscorelist2[[i]] <- loglikscore(as.matrix(test), trainMLlist2[[i]]$coefficients$location,
                                  trainMLlist2[[i]]$coefficients$scale)
}

avgboostscore2 <- mean(unlist(boostscorelist2))
avgmlscore2 <- mean(unlist(MLscorelist2))

####################################################################################
#------------testing the extremes---------------------------------------------------

tinyset <- simulate_data(10,10, 4578)
tinydata <- data.frame(cbind(tinyset$y, tinyset$X))

tinymodel <- lslm_boost(X1~., ~tinyset$X, data = tinydata, mstop = 2000)

tinyset$beta_coef
tinymodel$coefficients$location

tiny_CV <- boost_CV(tinymodel, k= 10, c(10, 100, 1000, 2000, 5000, 10000), 1996)

###############################################################################
tinylist <- list()
tinydatalist <- list()

for(i in 1:20){
  tinylist[[i]] <- simulate_data(10,10, i*457)
  tinydatalist[[i]] <- data.frame(cbind(tinylist[[i]]$y,tinylist[[i]]$X))
}

tinybeta <- matrix(NA, 11, 20)
for(j in 1:length(tinylist)){
  tinybeta[,j] <- tinylist[[j]]$beta_coef
}

tinygamma <- matrix(NA, 11, 20)
for(j in 1:length(tinylist)){
  tinygamma[,j] <- tinylist[[j]]$gamma_coef
}

tinymodellist <- list()
for(i in 1:length(tinylist)){
  tinymodellist[[i]] <- lslm_boost(X1~., ~tinylist[[i]]$X, 
                                   data = tinydatalist[[i]], mstop = 5000)
}

tinybetaest <- matrix(NA, 11, 20)
for(j in 1:length(tinymodellist)){
  tinybetaest[,j] <- tinymodellist[[j]]$coefficients$location
}

tinygammaest <- matrix(NA, 11, 20)
for(j in 1:length(tinymodellist)){
  tinygammaest[,j] <- tinymodellist[[j]]$coefficients$scale
}

tinyMSEbeta <- MSE(tinybetaest, tinybeta)
tinyMSEgamma <- MSE(tinygammaest, tinygamma)

