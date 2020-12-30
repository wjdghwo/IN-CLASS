#######################문제1#################################
#install.packages("mnormt")
library(mnormt)
RNGkind(sample.kind = "Rounding")
set.seed(1111)
K <- 100; n <- 200; p <- 10
x.tran <- x.test <- x.vald <- array(0, c(n, p, K))
z <- rep(c(1,2,3), each=n/2)
covm <- matrix(0.6, p, p); diag(covm) <- 1
for (i in 1:K) {
  t <- sample(1:p, 1); s <- sample(1:p, t)
  mu <- rep(0,p); mu[s] <- runif(t,-1, 1)
  x1 <- rmnorm(3*n/2, mu, covm)
  x2 <- rmnorm(3*n/2, rep(0,p), covm)
  x.tran[,,i] <- rbind(x1[z==1,], x2[z==1,])
  x.test[,,i] <- rbind(x1[z==2,], x2[z==2,])
  x.vald[,,i] <- rbind(x1[z==3,], x2[z==3,])
}


#install.packages('gbm')
library(gbm)

dim(x.tran)
dim(x.vald)
dim(x.test)

y <- c(rep(1,100),rep(0,100))
cer_test <- matrix(NA,100,3)

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(cbind(y,x.tran[,,i])) 
  vald <- data.frame(cbind(y,x.vald[,,i])) 
  test <- data.frame(cbind(y,x.test[,,i]))
  
  cer_val <- c()
  
  for(j in 1:3){
    boost <- gbm(y~., data=tran, distribution="bernoulli",
                 n.trees=1000, interaction.depth=j)
    yhat <- predict(boost, newdata=test,
                    type="response", n.trees=1000)
    phat <- rep(0, length(yhat))
    phat[yhat > 0.5] <- 1
    cer_val[j] <- mean(phat!=test[,1])
  }
  cer_test[i,] <- cer_val
}
cer_test
apply(cer_test,2,mean)
q1 <- apply(cer_test,2,mean)
which(min(q1)==q1)

#######################문제2#################################
y <- c(rep(1,100),rep(0,100))
cer_test <- matrix(NA,100,3)

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(cbind(y,x.tran[,,i])) 
  vald <- data.frame(cbind(y,x.vald[,,i])) 
  test <- data.frame(cbind(y,x.test[,,i]))
  
  cer_val <- rep(NA,3)
  
  for(j in 1:3){
    cer_tran <- rep(NA,1000)
    
    boost <- gbm(y~., data=tran, distribution="bernoulli",
                 n.trees=1000, interaction.depth=j)
    
    for(k in 1:1000){
      yhat <- predict(boost, newdata=vald,
                      type="response", n.trees=k)
      phat <- rep(0, length(yhat))
      phat[yhat > 0.5] <- 1
      cer_tran[k] <- mean(phat!=vald[,1])
    }
    
    n <- max(which(min(cer_tran)==cer_tran))
    boost <- gbm(y~., data=tran, distribution="bernoulli",
                 n.trees=1000, interaction.depth=j)
    yhat <- predict(boost, newdata=test,
                    type="response", n.trees=n)
    phat <- rep(0, length(yhat))
    phat[yhat > 0.5] <- 1
    cer_val[j] <- mean(phat!=test[,1])
  }
  cer_test[i,] <- cer_val
}
cer_test
apply(cer_test,2,mean)
q2 <- apply(cer_test,2,mean)
which(min(q2)==q2)

#######################문제3#################################
#install.packages("caTools")
library(caTools)

y <- as.factor(c(rep(1,100),rep(0,100)))
cer_test <- rep(NA,100)

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  tran <- data.frame(y,x.tran[,,i]) 
  vald <- data.frame(y,x.vald[,,i])
  test <- data.frame(y,x.test[,,i])
  
  cer_val <- rep(NA,3)
  
  cer_tran <- rep(NA,500)
  
  a <- seq(1,1000,2)
  
  tran_y <- tran[,1]  
  tran_x <- tran[,-1]
  
  boost <- LogitBoost(tran_x, tran_y, nIter = 1000)
  
  for(k in 1:500){
    vald_x <- vald[,-1]
    yhat <- predict(boost, xtest=vald_x, nIter=a[k], type="raw")[,2]
    phat <- rep(0, length(yhat))
    phat[yhat > 0.5] <- 1
    cer_tran[k] <- mean(phat!=vald[,1])
  }
  
  n <- max(which(min(cer_tran)==cer_tran))
  
  test_x <- test[,-1]
  
  boost <- LogitBoost(tran_x, tran_y, nIter = 1000)
  yhat <- predict(boost, xtest=test_x, nIter=n, type="raw")[,2]
  phat <- rep(0, length(yhat))
  phat[yhat > 0.5] <- 1
  cer_test[i] <- mean(phat!=test[,1])
}

mean(cer_test)
q3 <- mean(cer_test)


#######################문제4#################################
#install.packages("kernlab")
library(kernlab)
c <- seq (-10,15)
a <- matrix(NA, 200, 26)
thr <- matrix(NA, 200, 26)
cer_test <- rep(NA,100)
y <- as.factor(c(rep(1,100),rep(0,100)))

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(y,x.tran[,,i]) 
  vald <- data.frame(y,x.vald[,,i])
  test <- data.frame(y,x.test[,,i])
  
  ypred <- matrix(NA, 200, 26)
  
  for(j in 1:26){
    svm <- ksvm(y~.,data=tran, kernel="vanilladot", C=2^c[j])
    vald_x <- vald[,-1]
    ypred[,j] <- predict(svm, vald_x, type="decision")
    thr[,j] <- sort(ypred[,j])
    
    for(k in 1:200){
      phat <- rep(0, length(ypred[,j]))
      phat[ypred[,j]> thr[k,j]] <- 1
      a[k,j] <- mean(phat!=vald[,1])
    }
  }
  fin_c <- min(which(min(apply(a,2,min))==apply(a,2,min)))
  fin_thr <- mean(thr[which(min(a[,fin_c])==a[,fin_c]),fin_c])
  svm <- ksvm(y~.,data=tran, kernel="vanilladot", C=2^c[fin_c])
  test_x <- test[,-1]
  yhat <- predict(svm, test_x, type="decision")
  phat <- rep(0, length(yhat))
  phat[yhat > fin_thr] <- 1
  cer_test[i] <- mean(phat!=test[,1])
}

mean(cer_test)
vanilladot <- mean(cer_test)



#######################문제5#################################
#install.packages("kernlab")
library(kernlab)
c <- seq (-10,15)
a <- matrix(NA, 200, 26)
thr <- matrix(NA, 200, 26)
cer_test <- rep(NA,100)
y <- as.factor(c(rep(1,100),rep(0,100)))

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(y,x.tran[,,i]) 
  vald <- data.frame(y,x.vald[,,i])
  test <- data.frame(y,x.test[,,i])
  
  ypred <- matrix(NA, 200, 26)
  
  for(j in 1:26){
    svm <- ksvm(y~.,data=tran, kernel="rbfdot", C=2^c[j])
    vald_x <- vald[,-1]
    ypred[,j] <- predict(svm, vald_x, type="decision")
    thr[,j] <- sort(ypred[,j])
    
    for(k in 1:200){
      phat <- rep(0, length(ypred[,j]))
      phat[ypred[,j]> thr[k,j]] <- 1
      a[k,j] <- mean(phat!=vald[,1])
    }
  }
  fin_c <- min(which(min(apply(a,2,min))==apply(a,2,min)))
  fin_thr <- mean(thr[which(min(a[,fin_c])==a[,fin_c]),fin_c])
  svm <- ksvm(y~.,data=tran, kernel="rbfdot", C=2^c[fin_c])
  test_x <- test[,-1]
  yhat <- predict(svm, test_x, type="decision")
  phat <- rep(0, length(yhat))
  phat[yhat > fin_thr] <- 1
  cer_test[i] <- mean(phat!=test[,1])
}

mean(cer_test)
rbfdot <- mean(cer_test)


#################
#install.packages("kernlab")
library(kernlab)
c <- seq (-10,15)
a <- matrix(NA, 200, 26)
thr <- matrix(NA, 200, 26)
cer_test <- rep(NA,100)
y <- as.factor(c(rep(1,100),rep(0,100)))

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(y,x.tran[,,i]) 
  vald <- data.frame(y,x.vald[,,i])
  test <- data.frame(y,x.test[,,i])
  
  ypred <- matrix(NA, 200, 26)
  
  for(j in 1:26){
    svm <- ksvm(y~.,data=tran, kernel="polydot", C=2^c[j])
    vald_x <- vald[,-1]
    ypred[,j] <- predict(svm, vald_x, type="decision")
    thr[,j] <- sort(ypred[,j])
    
    for(k in 1:200){
      phat <- rep(0, length(ypred[,j]))
      phat[ypred[,j]> thr[k,j]] <- 1
      a[k,j] <- mean(phat!=vald[,1])
    }
  }
  fin_c <- min(which(min(apply(a,2,min))==apply(a,2,min)))
  fin_thr <- mean(thr[which(min(a[,fin_c])==a[,fin_c]),fin_c])
  svm <- ksvm(y~.,data=tran, kernel="polydot", C=2^c[fin_c])
  test_x <- test[,-1]
  yhat <- predict(svm, test_x, type="decision")
  phat <- rep(0, length(yhat))
  phat[yhat > fin_thr] <- 1
  cer_test[i] <- mean(phat!=test[,1])
}

mean(cer_test)
polydot <- mean(cer_test)


#################
#install.packages("kernlab")
library(kernlab)
c <- seq (-10,15)
a <- matrix(NA, 200, 26)
thr <- matrix(NA, 200, 26)
cer_test <- rep(NA,100)
y <- as.factor(c(rep(1,100),rep(0,100)))

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(y,x.tran[,,i]) 
  vald <- data.frame(y,x.vald[,,i])
  test <- data.frame(y,x.test[,,i])
  
  ypred <- matrix(NA, 200, 26)
  
  for(j in 1:26){
    svm <- ksvm(y~.,data=tran, kernel="tanhdot", C=2^c[j])
    vald_x <- vald[,-1]
    ypred[,j] <- predict(svm, vald_x, type="decision")
    thr[,j] <- sort(ypred[,j])
    
    for(k in 1:200){
      phat <- rep(0, length(ypred[,j]))
      phat[ypred[,j]> thr[k,j]] <- 1
      a[k,j] <- mean(phat!=vald[,1])
    }
  }
  fin_c <- min(which(min(apply(a,2,min))==apply(a,2,min)))
  fin_thr <- mean(thr[which(min(a[,fin_c])==a[,fin_c]),fin_c])
  svm <- ksvm(y~.,data=tran, kernel="tanhdot", C=2^c[fin_c])
  test_x <- test[,-1]
  yhat <- predict(svm, test_x, type="decision")
  phat <- rep(0, length(yhat))
  phat[yhat > fin_thr] <- 1
  cer_test[i] <- mean(phat!=test[,1])
}

mean(cer_test)
tanhdot <- mean(cer_test)


#################
#install.packages("kernlab")
library(kernlab)
c <- seq (-10,15)
a <- matrix(NA, 200, 26)
thr <- matrix(NA, 200, 26)
cer_test <- rep(NA,100)
y <- as.factor(c(rep(1,100),rep(0,100)))

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(y,x.tran[,,i]) 
  vald <- data.frame(y,x.vald[,,i])
  test <- data.frame(y,x.test[,,i])
  
  ypred <- matrix(NA, 200, 26)
  
  for(j in 1:26){
    svm <- ksvm(y~.,data=tran, kernel="laplacedot", C=2^c[j])
    vald_x <- vald[,-1]
    ypred[,j] <- predict(svm, vald_x, type="decision")
    thr[,j] <- sort(ypred[,j])
    
    for(k in 1:200){
      phat <- rep(0, length(ypred[,j]))
      phat[ypred[,j]> thr[k,j]] <- 1
      a[k,j] <- mean(phat!=vald[,1])
    }
  }
  fin_c <- min(which(min(apply(a,2,min))==apply(a,2,min)))
  fin_thr <- mean(thr[which(min(a[,fin_c])==a[,fin_c]),fin_c])
  svm <- ksvm(y~.,data=tran, kernel="laplacedot", C=2^c[fin_c])
  test_x <- test[,-1]
  yhat <- predict(svm, test_x, type="decision")
  phat <- rep(0, length(yhat))
  phat[yhat > fin_thr] <- 1
  cer_test[i] <- mean(phat!=test[,1])
}

mean(cer_test)
laplacedot <- mean(cer_test)


#################
#install.packages("kernlab")
library(kernlab)
c <- seq (-10,15)
a <- matrix(NA, 200, 26)
thr <- matrix(NA, 200, 26)
cer_test <- rep(NA,100)
y <- as.factor(c(rep(1,100),rep(0,100)))

for(i in 1:100){
  cat(paste0("i : ", i, "\n"))
  
  tran <- data.frame(y,x.tran[,,i]) 
  vald <- data.frame(y,x.vald[,,i])
  test <- data.frame(y,x.test[,,i])
  
  ypred <- matrix(NA, 200, 26)
  
  for(j in 1:26){
    svm <- ksvm(y~.,data=tran, kernel="besseldot", C=2^c[j])
    vald_x <- vald[,-1]
    ypred[,j] <- predict(svm, vald_x, type="decision")
    thr[,j] <- sort(ypred[,j])
    
    for(k in 1:200){
      phat <- rep(0, length(ypred[,j]))
      phat[ypred[,j]> thr[k,j]] <- 1
      a[k,j] <- mean(phat!=vald[,1])
    }
  }
  fin_c <- min(which(min(apply(a,2,min))==apply(a,2,min)))
  fin_thr <- mean(thr[which(min(a[,fin_c])==a[,fin_c]),fin_c])
  svm <- ksvm(y~.,data=tran, kernel="besseldot", C=2^c[fin_c])
  test_x <- test[,-1]
  yhat <- predict(svm, test_x, type="decision")
  phat <- rep(0, length(yhat))
  phat[yhat > fin_thr] <- 1
  cer_test[i] <- mean(phat!=test[,1])
}

mean(cer_test)
besseldot <- mean(cer_test)



q1
q2
q3
vanilladot
rbfdot
polydot
tanhdot
laplacedot
besseldot