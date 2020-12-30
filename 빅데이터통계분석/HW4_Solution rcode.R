library(ISLR)
library(MASS)
library(e1071)
library(tree)
library(randomForest)
data(College)
RNGkind(sample.kind = "Rounding")
set.seed(1111)
id <- rep(c("tr", "te"), c(600, 177))
index <- matrix(id, length(id), 100)
index <- apply(index, 2, sample)
rep <- dim(index)[2]


###Q1
omnibus.fun <- function(pred.class,test.class){
  acc <- 1-mean(test.class[index[,i]=='te',1]!=pred.class)
  tp <- sum(pred.class[test.class[index[,i]=='te',1]=='Yes']=='Yes')
  tn <- sum(pred.class[test.class[index[,i]=='te',1]=='No']=='No')
  fp <- sum(pred.class[test.class[index[,i]=='te',1]=='No']=='Yes')
  fn <- sum(pred.class[test.class[index[,i]=='te',1]=='Yes']=='No')
  num <- tp*tn-fp*fn
  denum <- sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  if(denum==0){
    mcc <- 0
  }else{
    mcc <- num/denum
  }
  f1 <- 2*tp/(2*tp+fp+fn)
  res <- c(acc,mcc,f1)
  return(res)
}


###Q2
lda.res <- qda.res <- nb.res <- matrix(NA,rep,3)
for(i in 1:rep){
  
  ###LDA
  lda.tran <- lda(Private~.,data=College[index[,i]=='tr',])
  lda.pred <- predict(lda.tran,College[index[,i]=='te',])$class
  lda.res[i,] <- omnibus.fun(pred.class=lda.pred,test.class=College)
  
  ###QDA
  qda.tran <- qda(Private~.,data=College[index[,i]=='tr',])
  qda.pred <- predict(qda.tran,College[index[,i]=='te',])$class
  qda.res[i,] <- omnibus.fun(pred.class=qda.pred,test.class=College)
  
  ###NB
  nb.tran <- naiveBayes(Private~.,data=College[index[,i]=='tr',])
  nb.pred <- predict(nb.tran,College[index[,i]=='te',])
  nb.res[i,] <- omnibus.fun(pred.class=nb.pred,test.class=College)
}
ans2 <- rbind(apply(lda.res,2,mean),apply(qda.res,2,mean),apply(nb.res,2,mean))
colnames(ans2) <- c('ACC','MCC','F1')
rownames(ans2) <- c('LDA','QDA','NB')
ans2



###Q3
tree.res <- matrix(NA,rep,3)
for(i in 1:rep){
  tree.tran <- tree(Private~.,data=College[index[,i]=='tr',])
  tree.pred <- predict(tree.tran,College[index[,i]=='te',],type='class')
  tree.res[i,] <- omnibus.fun(pred.class=tree.pred,test.class=College)
}
ans3 <- matrix(apply(tree.res,2,mean),1,3)
colnames(ans3) <- c('ACC','MCC','F1')
rownames(ans3) <- 'Tree'
ans3



###Q4
RNGkind(sample.kind = "Rounding")
set.seed(4444)
cv.id <- rep(seq(10), 60)
cv.index <- matrix(cv.id, length(cv.id), 100)
cv.index <- apply(cv.index, 2, sample)
cv.tree.res <- matrix(NA,rep,3)
for(i in 1:rep){
  tree.tran <- tree(Private~.,data=College[index[,i]=='tr',])
  cvtree.tran <- cv.tree(tree.tran,rand=cv.index[,i],FUN=prune.misclass)
  w <- max(cvtree.tran$size[which(cvtree.tran$dev==min(cvtree.tran$dev))])
  prune.college <- prune.misclass(tree.tran, best=w)
  tree.pred <- predict(prune.college,College[index[,i]=='te',],type='class')
  cv.tree.res[i,] <- omnibus.fun(pred.class=tree.pred,test.class=College)
}
ans4 <- matrix(apply(cv.tree.res,2,mean),1,3)
colnames(ans4) <- c('ACC','MCC','F1'); rownames(ans4) <- 'CV Tree'
ans4



###Q5
RNGkind(sample.kind = "Rounding")
set.seed(5555)
bt.index <- array(0, c(600, 300, 100))
for (t in 1:100) {
  for (b in 1:300) {
    u <- unique(sample(1:600, replace=TRUE))
    bt.index[u, b, t] <- 1
  }
}
num.test <- sum(id=='te')
bagging <- matrix(NA,rep,3)
for(i in 1:rep){
  mat.rep <- bt.index[,,i]
  bt.rep <- dim(mat.rep)[2]
  bagging.res <- matrix(NA,bt.rep,num.test)
  for(b in 1:bt.rep){
    College.temp <- College[index[,i]=='tr',][mat.rep[,b]==1,]
    bagging.tran <- tree(Private~.,data=College.temp)
    bagging.pred <- predict(bagging.tran,College[index[,i]=='te',],type='class')
    bagging.res[b,] <- bagging.pred
  }
  vote <- apply(bagging.res,2,function(x) sum(x==2))
  decision <- rep('No',num.test)
  decision[vote >= 150] <- 'Yes'
  bagging[i,] <- omnibus.fun(pred.class=decision,test.class=College)
}
ans5 <- matrix(apply(bagging,2,mean),1,3)
colnames(ans5) <- c('ACC','MCC','F1')
rownames(ans5) <- 'Bagging'
ans5



###Q6
m <- 1:6
K <- 10
rf.res <- matrix(NA,rep,3)
for(i in 1:rep){
  College.tran <- College[index[,i]=='tr',]
  gr <- cv.index[,i]
  rf.miss <- matrix(NA,K,length(m))
  for(k in 1:K){
    train <- which(gr!=k)
    test <- which(gr==k)
    for(j in 1:length(m)){
      rf.model <-
        randomForest(x=College.tran[train,-1],y=College.tran[train,1],xtest=College.tran[test,-1]
                     , ytest=College.tran[test,1],mtry=m[j],importance=TRUE,ntree=1000)
      rf.conf <- rf.model$test$confusion[1:2,1:2]
      rf.miss[k,j] <- 1- sum(diag(rf.conf))/sum(rf.conf)
    }
  }
  cv.ave <- apply(rf.miss,2,mean)
  m.best <- min(m[which(cv.ave==min(cv.ave))])
  rf.refit <-
    randomForest(x=College.tran[,-1],y=College.tran[,1],xtest=College[index[,i]=='te',-1],
                 ytest=College[index[,i]=='te',1],mtry=m.best,importance=TRUE,ntree=1000)
  rf.refit.pred <- rf.refit$test$predicted
  rf.res[i,] <- omnibus.fun(pred.class=rf.refit.pred,test.class=College)
}
ans6 <- matrix(apply(rf.res,2,mean),1,3)
colnames(ans6) <- c('ACC','MCC','F1')
rownames(ans6) <- 'RandomForest'
ans6



###Q7
ans7 <- rbind(ans2,ans3,ans4,ans5,ans6)
ans7


