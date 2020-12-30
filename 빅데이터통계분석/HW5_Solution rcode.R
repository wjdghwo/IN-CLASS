library(mnormt)
library(gbm)
library(caTools)
library(kernlab)


RNGkind(sample.kind = "Rounding")
set.seed(1111)
K <- 100; n <- 200; p <- 10
x.tran <- x.test <- x.vald <- array(0, c(n, p, K))
z <- rep(c(1,2,3), each=n/2)
covm <- matrix(0.6, p, p); diag(covm) <- 1
for(i in 1:K){
  t <- sample(1:p, 1); s <- sample(1:p, t)
  mu <- rep(0,p); mu[s] <- runif(t,-1, 1)
  x1 <- rmnorm(3*n/2, mu, covm)
  x2 <- rmnorm(3*n/2, rep(0,p), covm)
  x.tran[,,i] <- rbind(x1[z==1,], x2[z==1,])
  x.test[,,i] <- rbind(x1[z==2,], x2[z==2,])
  x.vald[,,i] <- rbind(x1[z==3,], x2[z==3,])
}
y <- rep(c(1,0),c(100,100))


###Q1
boost.miss <- matrix(NA,K,3)
for(i in 1:K){
  data.tran <- cbind(y,x.tran[,,i])
  data.test <- cbind(y,x.test[,,i])
  colnames(data.tran) <- colnames(data.test) <- c('y',paste0('x',1:10))
  for(j in 1:3){
    boost.fit <-
      gbm(y~.,data=data.frame(data.tran),distribution='bernoulli',n.trees=1000,interaction.de
          pth=j)
    boost.pred <-
      predict(boost.fit,newdata=data.frame(data.test),type='response',n.trees=1000)
    decision <- rep(0,dim(data.test)[1])
    decision[boost.pred > 0.5] <- 1
    boost.miss[i,j] <- mean(decision!=y)
  }
}
ans1 <- matrix(apply(boost.miss,2,mean),1,3)
colnames(ans1) <- c('Depth=1','Depth=2','Depth=3')
rownames(ans1) <- 'Boosting'
ans1



###Q2
depth <- 1:3
ntrees <- 1:1000
boost.opt.miss <- matrix(NA,K,length(depth))
for(i in 1:K){
  data.tran <- cbind(y,x.tran[,,i])
  data.test <- cbind(y,x.test[,,i])
  data.vald <- cbind(y,x.vald[,,i])
  colnames(data.tran) <- colnames(data.test) <- colnames(data.vald) <-
    c('y',paste0('x',1:10))
  boost.miss <- matrix(NA,length(depth),length(ntrees))
  for(k in 1:length(depth)){
    boost.fit <-
      gbm(y~.,data=data.frame(data.tran),distribution='bernoulli',n.trees=1000,interaction.de
          pth=k)
    for(j in 1:length(ntrees)){
      boost.pred <-
        predict(boost.fit,newdata=data.frame(data.vald),type='response',n.trees=j)
      decision <- rep(0,dim(data.vald)[1])
      decision[boost.pred > 0.5] <- 1
      boost.miss[k,j] <- mean(decision!=y)
    }
  }
  opt.size <- apply(boost.miss,1,function(x) max(which(x==min(x))))
  for(k in 1:length(depth)){
    boost.refit <-
      gbm(y~.,data=data.frame(data.tran),distribution='bernoulli',n.trees=opt.size[k],interacti
          on.depth=depth[k])
    boost.repred <-
      predict(boost.refit,newdata=data.frame(data.test),type='response',n.trees=opt.size[k])
    decision <- rep(0,dim(data.test)[1])
    decision[boost.repred > 0.5] <- 1
    boost.opt.miss[i,k] <- mean(decision!=y)
  }
}
ans2 <- matrix(apply(boost.opt.miss,2,mean),1,3)
colnames(ans2) <- c('Depth=1','Depth=2','Depth=3')
rownames(ans2) <- 'Boosting'
ans2



###Q3
iter <- seq(1,999,2)
logit.boot.miss <- NA
for(i in 1:K){
  logit.boot.fit <- LogitBoost(xlearn=x.tran[,,i], ylearn=y, nIter=1000)
  vald.miss <- NA
  for(j in 1:length(iter)){
    logit.pred <- predict(logit.boot.fit, x.vald[,,i], type ='raw', nIter=iter[j])[,2]
    decision <- rep(0,dim(x.vald[,,i])[1])
    decision[logit.pred > 0.5] <- 1
    vald.miss[j] <- mean(y!=decision)
  }
  opt.iter <- max(iter[which(vald.miss==min(vald.miss))])
  logit.repred <- predict(logit.boot.fit,x.test[,,i],type ='raw',nIter=opt.iter)[,2]
  decision <- rep(0,dim(x.test[,,i])[1])
  decision[logit.repred > 0.5] <- 1
  logit.boot.miss[i] <- mean(y!=decision)
}
ans3 <- mean(logit.boot.miss)



###Q4
y.fac <- as.factor(y)
cons <- 2^seq(-10,15)
test.miss <- NA
for(i in 1:K){
  res.van.temp <- matrix(NA,length(cons),2)
  for(j in 1:length(cons)){
    svm.van.fit <-
      ksvm(x=x.tran[,,i],y=y.fac,kernel='vanilladot',C=cons[j],kpar=list(),type="C-svc")
    svm.van.pred <- predict(svm.van.fit,x.vald[,,i],type='decision')
    svm.van.sort <- sort(svm.van.pred,decreasing=FALSE)
    vald.miss <- NA
    for(k in 1:length(svm.van.sort)){
      cx.hat <- ifelse(svm.van.pred > svm.van.sort[k],1,0)
      vald.miss[k] <- mean(y.fac!=cx.hat)
    }
    res.van.temp[j,1] <- min(vald.miss)
    res.van.temp[j,2] <- mean(svm.van.sort[vald.miss==min(vald.miss)])
  }
  opt.c <- cons[which.min(res.van.temp[,1])]
  opt.thred <- res.van.temp[which.min(res.van.temp[,1]),2]
  svm.van.fit <-
    ksvm(x=x.tran[,,i],y=y.fac,kernel='vanilladot',C=opt.c,kpar=list(),type="C-svc")
  svm.van.pred <- predict(svm.van.fit,x.test[,,i],type='decision')
  cx.hat <- ifelse(svm.van.pred > opt.thred,1,0)
  test.miss[i] <- mean(y.fac!=cx.hat)
}
ans4 <- mean(test.miss)



###Q5
test.miss.res <- matrix(NA,K,5)
for(i in 1:K){
  res.rbf.temp <- res.poly.temp <- res.tanh.temp <- res.lap.temp <- res.bess.temp
  <- matrix(NA,length(cons),2)
  for(j in 1:length(cons)){
    ###
    svm.rbf.fit <-
      ksvm(x=x.tran[,,i],y=y.fac,kernel='rbfdot',C=cons[j],kpar=list(),type="C-svc")
    svm.rbf.pred <- predict(svm.rbf.fit,x.vald[,,i],type='decision')
    svm.rbf.sort <- sort(svm.rbf.pred,decreasing=FALSE)
    ###
    svm.poly.fit <-
      ksvm(x=x.tran[,,i],y=y.fac,kernel='polydot',C=cons[j],kpar=list(),type="C-svc")
    svm.poly.pred <- predict(svm.poly.fit,x.vald[,,i],type='decision')
    svm.poly.sort <- sort(svm.poly.pred,decreasing=FALSE)
    ###
    svm.tanh.fit <-
      ksvm(x=x.tran[,,i],y=y.fac,kernel='tanhdot',C=cons[j],kpar=list(),type="C-svc")
    svm.tanh.pred <- predict(svm.tanh.fit,x.vald[,,i],type='decision')
    svm.tanh.sort <- sort(svm.tanh.pred,decreasing=FALSE)
    ###
    svm.lap.fit <-
      ksvm(x=x.tran[,,i],y=y.fac,kernel='laplacedot',C=cons[j],kpar=list(),type="C-svc")
    svm.lap.pred <- predict(svm.lap.fit,x.vald[,,i],type='decision')
    svm.lap.sort <- sort(svm.lap.pred,decreasing=FALSE)
    ###
    svm.bess.fit <-
      ksvm(x=x.tran[,,i],y=y.fac,kernel='besseldot',C=cons[j],kpar=list(),type="C-svc")
    svm.bess.pred <- predict(svm.bess.fit,x.vald[,,i],type='decision')
    svm.bess.sort <- sort(svm.bess.pred,decreasing=FALSE)
    ###
    vald.miss.rbf <- vald.miss.poly <- vald.miss.tanh <- vald.miss.lap <-
      vald.miss.bess <- NA
    for(k in 1:length(y.fac)){
      ###
      cx.hat.rbf <- ifelse(svm.rbf.pred > svm.rbf.sort[k],1,0)
      vald.miss.rbf[k] <- mean(y.fac!=cx.hat.rbf)
      ###
      cx.hat.poly <- ifelse(svm.poly.pred > svm.poly.sort[k],1,0)
      vald.miss.poly[k] <- mean(y.fac!=cx.hat.poly)
      ###
      cx.hat.tanh <- ifelse(svm.tanh.pred > svm.tanh.sort[k],1,0)
      vald.miss.tanh[k] <- mean(y.fac!=cx.hat.tanh)
      ###
      cx.hat.lap <- ifelse(svm.lap.pred > svm.lap.sort[k],1,0)
      vald.miss.lap[k] <- mean(y.fac!=cx.hat.lap)
      ###
      cx.hat.bess <- ifelse(svm.bess.pred > svm.bess.sort[k],1,0)
      vald.miss.bess[k] <- mean(y.fac!=cx.hat.bess)
    }
    res.rbf.temp[j,1] <- min(vald.miss.rbf)
    res.rbf.temp[j,2] <- mean(svm.rbf.sort[vald.miss.rbf==min(vald.miss.rbf)])
    res.poly.temp[j,1] <- min(vald.miss.poly)
    res.poly.temp[j,2] <- mean(svm.poly.sort[vald.miss.poly==min(vald.miss.poly)])
    res.tanh.temp[j,1] <- min(vald.miss.tanh)
    res.tanh.temp[j,2] <- mean(svm.tanh.sort[vald.miss.tanh==min(vald.miss.tanh)])
    res.lap.temp[j,1] <- min(vald.miss.lap)
    res.lap.temp[j,2] <- mean(svm.lap.sort[vald.miss.lap==min(vald.miss.lap)])
    res.bess.temp[j,1] <- min(vald.miss.bess)
    res.bess.temp[j,2] <- mean(svm.bess.sort[vald.miss.bess==min(vald.miss.bess)]) 
  }
  ###
  opt.c.rbf <- cons[which.min(res.rbf.temp[,1])]
  opt.thred.rbf <- res.rbf.temp[which.min(res.rbf.temp[,1]),2]
  svm.rbf.fit <-
    ksvm(x=x.tran[,,i],y=y.fac,kernel='rbfdot',C=opt.c.rbf,kpar=list(),type="C-svc")
  svm.rbf.pred <- predict(svm.rbf.fit,x.test[,,i],type='decision')
  cx.hat.rbf <- ifelse(svm.rbf.pred > opt.thred.rbf,1,0)
  test.miss.res[i,1] <- mean(y.fac!=cx.hat.rbf)
  ###
  opt.c.poly <- cons[which.min(res.poly.temp[,1])]
  opt.thred.poly <- res.poly.temp[which.min(res.poly.temp[,1]),2]
  svm.poly.fit <-
    ksvm(x=x.tran[,,i],y=y.fac,kernel='polydot',C=opt.c.poly,kpar=list(),type="C-svc")
  svm.poly.pred <- predict(svm.poly.fit,x.test[,,i],type='decision')
  cx.hat.poly <- ifelse(svm.poly.pred > opt.thred.poly,1,0)
  test.miss.res[i,2] <- mean(y.fac!=cx.hat.poly)
  ###
  opt.c.tanh <- cons[which.min(res.tanh.temp[,1])]
  opt.thred.tanh <- res.tanh.temp[which.min(res.tanh.temp[,1]),2]
  svm.tanh.fit <-
    ksvm(x=x.tran[,,i],y=y.fac,kernel='tanhdot',C=opt.c.tanh,kpar=list(),type="C-svc")
  svm.tanh.pred <- predict(svm.tanh.fit,x.test[,,i],type='decision')
  cx.hat.tanh <- ifelse(svm.tanh.pred > opt.thred.tanh,1,0)
  test.miss.res[i,3] <- mean(y.fac!=cx.hat.tanh)
  ###
  opt.c.lap <- cons[which.min(res.lap.temp[,1])]
  opt.thred.lap <- res.lap.temp[which.min(res.lap.temp[,1]),2]
  svm.lap.fit <-
    ksvm(x=x.tran[,,i],y=y.fac,kernel='laplacedot',C=opt.c.lap,kpar=list(),type="C-svc")
  svm.lap.pred <- predict(svm.lap.fit,x.test[,,i],type='decision')
  cx.hat.lap <- ifelse(svm.lap.pred > opt.thred.lap,1,0)
  test.miss.res[i,4] <- mean(y.fac!=cx.hat.lap)
  ###
  opt.c.bess <- cons[which.min(res.bess.temp[,1])]
  opt.thred.bess <- res.bess.temp[which.min(res.bess.temp[,1]),2]
  svm.bess.fit <-
    ksvm(x=x.tran[,,i],y=y.fac,kernel='besseldot',C=opt.c.bess,kpar=list(),type="C-svc")
  svm.bess.pred <- predict(svm.bess.fit,x.test[,,i],type='decision')
  cx.hat.bess <- ifelse(svm.bess.pred > opt.thred.bess,1,0)
  test.miss.res[i,5] <- mean(y.fac!=cx.hat.bess)
}
ans5 <- matrix(apply(test.miss.res,2,mean),1,5)
colnames(ans5) <- c('rbfdot','polydot','tanhdot','laplacedot','besseldot')
rownames(ans5) <- 'SVM'
ans5
