# Comparison server - LB - MG
library(intRinsic)
library(Rdimtools)
library(intrinsicDimension)
library(parallel)


##############################################################################
# Comparison Levina-Bickel and MacKay-Gaharamni
##############################################################################
# ---------------------------------------------------- Functions

shorter.mudots <- function(K,n1,n2){
  res <- K[,n2]/K[,n1]
  return(res)
}

mle1 <- function(matK,k1,k2){
  mkhat = rep(0, (k2 - k1 + 1))
  for (k in k1:k2) {
    mkhat[k - k1 + 1] = 
      mean(1/((log(matK[, k]) - rowMeans(as.matrix(log(matK)[,1:(k - 1)]) ) )))
  }
  estdim = sum(mkhat)/(k2 - k1 + 1)
  return(estdim)
}
##############
mle2 <- function(matK,k1,k2){
  n = nrow(matK)
  mkhat = rep(0, (k2 - k1 + 1))
  for (k in k1:k2) {
    mkhat[k - k1 + 1] = 1/(sum(log(matK[, k]) * (k - 1) - 
                                 rowSums(as.matrix(log(matK)[, 1:(k - 1)])))/(n * (k - 1)))
  }
  estdim = sum(mkhat)/(k2 - k1 + 1)
  return(estdim)
}

#############
gridess = function(K,k1,k2){
  mudots <-  shorter.mudots(K,k1,k2)
  xx <- stats::optimize( intRinsic:::gride_log_likelihood,
                         interval = c(0.01, 50),
                         n1 = k1,
                         n2 = k2,
                         mus_n1_n2 = mudots,
                         maximum = T)$max
  xx
}
##############################################################################
# -------------------------------------------- Functions for estimating
#######################################
Three_estimators_N1_N2_varies_2 <- function(ND,Q=75){
  res <-  matrix(0,Q,5)
  for(i in 2:(Q+1)){
    res[i-1,1]=gridess(ND,   k1 = i,k2 = i*2)
    res[i-1,2]=mle1(   ND,   k1 = i,k2 = i*2)
    res[i-1,3]=mle2(  matK =  ND,   k1 = i,k2 = i*2)
    res[i-1,4:5]=colMeans(ND[,c(i,i*2)])
  }
  return(res)
}
##############################################################################
# ---------------------------------------------------- Run id =5
###############################################################################

Comparison <- function(id,sample.size,Nzeros=5,NSIM=30,CORES=4){
results = list()
ZZ      = matrix(0,sample.size,Nzeros)

for(i in 1:30){
  set.seed(1234*i)
  D0 <- cbind(replicate(id,rnorm(sample.size)),ZZ)
  D1 <- D0 + rnorm(prod(dim(D0)),0,.1)
  D2 <- D0 + rnorm(prod(dim(D0)),0,.25)
  D3 <- D0 + rnorm(prod(dim(D0)),0,.5)
  Dlist <- list(D0,D1,D2,D3)
  ##############################################################################
  NN_orders <- mclapply(1:4, function(zz)  
    FNN::get.knn(data = Dlist[[zz]],k = 210)$nn.dist )
  ##############################################################################
  Res1 <- mclapply(1:4, function(jj)   Three_estimators_N1_N2_varies_2(ND = NN_orders[[jj]], Q=50),mc.cores = CORES )
  Res2 <- NULL #OLD VERSION: mclapply(1:4, function(jj)   Three_estimators_N1_N2_varies_4(NN_orders[[jj]],Q=50),mc.cores = CORES )
  g = list(Res1,Res2)
  cat(i)
  results[[i]] = g
}

return(results)
}

# examples
# gridess(K = NN_orders[[2]],k1 = 50,k2 = 100)
# gride(D1,n1=50,n2=100)
