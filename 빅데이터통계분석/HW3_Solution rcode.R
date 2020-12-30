library(ISLR)
library(MASS)
library(ROCR)
library(e1071)
data(College)
y <- College[,1]
y.num <- rep(0,length(y))
y.num[y=='Yes'] <- 1
x <- as.matrix(College[,-1])
RNGkind(sample.kind = "Rounding")
set.seed(1234)
tran <- sample(dim(College)[1], floor(dim(College)[1]*0.7))
test <- setdiff(1:dim(College)[1],tran)


###Q1
logit.tran <- glm(y.num~x,family='binomial',subset=tran)
logit.pred <- predict(logit.tran,data.frame(x),type='response')[test]
logit.class <- ifelse(logit.pred>=0.5,1,0)
logit.miss <- mean(y.num[test]!=logit.class)

lda.tran <- lda(y.num~x,subset=tran)
lda.class <- predict(lda.tran,data.frame(x))$class[test]
lda.miss <- mean(y.num[test]!=lda.class)

qda.tran <- qda(y.num~x,subset=tran)
qda.class <- predict(qda.tran,data.frame(x))$class[test]
qda.miss <- mean(y.num[test]!=qda.class)

nb.tran <- naiveBayes(Private~.,data=College,subset=tran)
nb.class <- predict(nb.tran,College)[test]
nb.miss <- mean(y[test]!=nb.class)

ans1 <- c(logit.miss,lda.miss,qda.miss,nb.miss)
names(ans1) <- c('LR','LDA','QDA','NB')
ans1


###Q2
q2.logit.pred <- predict(logit.tran,data.frame(x),type='response')[test]
q2.lda.pred <- predict(lda.tran,data.frame(x))$posterior[,2][test]
q2.qda.pred <- predict(qda.tran,data.frame(x))$posterior[,2][test]
q2.nb.pred <- predict(nb.tran,College,type='raw')[,2][test]


label <- factor(y[test],levels=c("Yes","No"),labels=c("TRUE","FALSE"))
logit.preds <- prediction(q2.logit.pred,label)
lda.preds <- prediction(q2.lda.pred,label)
qda.preds <- prediction(q2.qda.pred,label)
nb.preds <- prediction(q2.nb.pred,label)
logit.perf <- performance(logit.preds,"tpr","fpr")
lda.perf <- performance(lda.preds,"tpr","fpr")
qda.perf <- performance(qda.preds,"tpr","fpr")
nb.perf <- performance(nb.preds,"tpr","fpr")


plot(logit.perf, lwd=2, col="darkblue",lty=1,main='ROC Curves')
plot(lda.perf,lwd=2,col='darkgreen',lty=2,add=TRUE)
plot(qda.perf,lwd=2,col='red',lty=3,add=TRUE)
plot(nb.perf,lwd=2,col='black',lty=4,add=TRUE)
abline(a=0, b=1, lty=2)
legend('bottomright',legend=c('LR','LDA','QDA','NB'),
       col=c('darkblue','darkgreen','red','black'),lwd=rep(2,4),lty=1:4,bty='n')


logit.auc <- performance(logit.preds, "auc")@y.values
logit.auc
lda.auc <- performance(lda.preds, "auc")@y.values
lda.auc
qda.auc <- performance(qda.preds, "auc")@y.values
qda.auc
nb.auc <- performance(nb.preds, "auc")@y.values
nb.auc


ans2 <- c(logit.auc[[1]],lda.auc[[1]],qda.auc[[1]],nb.auc[[1]])
names(ans2) <- c('LR','LDA','QDA','NB')
ans2
LR



###Q3
RNGkind(sample.kind = "Rounding")
set.seed(12345)
N.lab <- sample(rep(seq(10), length=sum(College$Private[tran]=="No")))
Y.lab <- sample(rep(seq(10), length=sum(College$Private[tran]=="Yes")))
gr <- rep(0, length(tran))
gr[College$Private[tran]=="No"] <- N.lab
gr[College$Private[tran]=="Yes"] <- Y.lab
q3.yy <- y[tran]
q3.College <- College[tran,]
q3.y <- y.num[tran]
q3.x <- x[tran,]
alpha <- seq(0,1,0.01)
K <- 10
q3.logit.miss <- q3.lda.miss <- q3.qda.miss <- q3.nb.miss <-
  matrix(NA,K,length(alpha))
for(k in 1:K){
  q3.tran <- which(gr!=k)
  q3.test <- which(gr==k)
  for(i in 1:length(alpha)){
    logit.tran <- glm(q3.y~q3.x,family='binomial',subset=q3.tran)
    logit.pred <- predict(logit.tran,data.frame(q3.x),type='response')[q3.test]
    logit.class <- ifelse(logit.pred > alpha[i],1,0)
    q3.logit.miss[k,i] <- mean(q3.y[q3.test]!=logit.class)
    
    
    lda.tran <- lda(q3.y~q3.x,subset=q3.tran)
    lda.pred <- predict(lda.tran,data.frame(q3.x))$posterior[,2][q3.test]
    lda.class <- ifelse(lda.pred > alpha[i],1,0)
    q3.lda.miss[k,i] <- mean(q3.y[q3.test]!=lda.class)
    
    
    qda.tran <- qda(q3.y~q3.x,subset=q3.tran)
    qda.pred <- predict(qda.tran,data.frame(q3.x))$posterior[,2][q3.test]
    qda.class <- ifelse(qda.pred > alpha[i],1,0)
    q3.qda.miss[k,i] <- mean(q3.y[q3.test]!=qda.class)
    
    
    nb.tran <- naiveBayes(Private~.,data=q3.College,subset=q3.tran)
    nb.pred <- predict(nb.tran,q3.College,type='raw')[,2][q3.test]
    nb.class <- ifelse(nb.pred > alpha[i],'Yes','No')
    q3.nb.miss[k,i] <- mean(q3.yy[q3.test]!=nb.class)
  }
}
q3.logit.cvs <- apply(q3.logit.miss,2,mean)
q3.logit.alpha <- alpha[which(q3.logit.cvs==min(q3.logit.cvs))]
q3.logit.class <- ifelse(q2.logit.pred > q3.logit.alpha,1,0)
q3.logit.miss <- mean(y.num[test]!=q3.logit.class)


q3.lda.cvs <- apply(q3.lda.miss,2,mean)
q3.lda.alpha <- mean(alpha[which(q3.lda.cvs==min(q3.lda.cvs))])
q3.lda.class <- ifelse(q2.lda.pred > q3.lda.alpha,1,0)
q3.lda.miss <- mean(y.num[test]!=q3.lda.class)


q3.qda.cvs <- apply(q3.qda.miss,2,mean)
q3.qda.alpha <- mean(alpha[which(q3.qda.cvs==min(q3.qda.cvs))])
q3.qda.class <- ifelse(q2.qda.pred > q3.qda.alpha,1,0)
q3.qda.miss <- mean(y.num[test]!=q3.qda.class)


q3.nb.cvs <- apply(q3.nb.miss,2,mean)
q3.nb.alpha <- mean(alpha[which(q3.nb.cvs==min(q3.nb.cvs))])
q3.nb.class <- ifelse(q2.nb.pred > q3.nb.alpha,1,0)
q3.nb.miss <- mean(y.num[test]!=q3.nb.class)


q3.alpha <- c(q3.logit.alpha,q3.lda.alpha,q3.qda.alpha,q3.nb.alpha)
q3.miss <- c(q3.logit.miss,q3.lda.miss,q3.qda.miss,q3.nb.miss)
ans3 <- rbind(q3.alpha,q3.miss)
colnames(ans3) <- c('LR','LDA','QDA','NB')
rownames(ans3) <- c('alpha_hat','Mis')
ans3



###Q4
q4.yy <- y[tran]
q4.College <- College[tran,]
q4.y <- y.num[tran]
q4.x <- x[tran,]
alpha <- seq(0,1,0.01)
K <- 10
logit.mcc <- lda.mcc <- qda.mcc <- nb.mcc <- matrix(NA,K,length(alpha))
for(k in 1:K){
  q4.tran <- which(gr!=k)
  q4.test <- which(gr==k)
  for(i in 1:length(alpha)){
    
    ##LR
    logit.tran <- glm(q4.y~q4.x,family='binomial',subset=q4.tran)
    logit.pred <- predict(logit.tran,data.frame(q4.x),type='response')[q4.test]
    logit.desn <- rep(0,length(q4.test))
    logit.desn[logit.pred > alpha[i]] <- 1
    logit.tp <- sum(logit.desn[q4.y[q4.test]==1]==1)
    logit.tn <- sum(logit.desn[q4.y[q4.test]==0]==0)
    logit.fp <- sum(logit.desn[q4.y[q4.test]==0]==1)
    logit.fn <- sum(logit.desn[q4.y[q4.test]==1]==0)
    logit.num <- logit.tp*logit.tn-logit.fp*logit.fn
    logit.den <-
      sqrt((logit.tp+logit.fp)*(logit.tp+logit.fn)*(logit.tn+logit.fp)*(logit.tn+logit.fn))
    if(logit.den==0){
      logit.mcc[k,i] <- 0
    }else{
      logit.mcc[k,i] <- logit.num/logit.den
    }
    
    ##LDA
    lda.tran <- lda(q4.y~q4.x,subset=q4.tran)
    lda.pred <- predict(lda.tran,data.frame(q4.x))$posterior[,2][q4.test]
    lda.desn <- rep(0,length(q4.test))
    lda.desn[lda.pred > alpha[i]] <- 1
    lda.tp <- sum(lda.desn[q4.y[q4.test]==1]==1)
    lda.tn <- sum(lda.desn[q4.y[q4.test]==0]==0)
    lda.fp <- sum(lda.desn[q4.y[q4.test]==0]==1)
    lda.fn <- sum(lda.desn[q4.y[q4.test]==1]==0)
    lda.num <- lda.tp*lda.tn-lda.fp*lda.fn
    lda.den <- sqrt((lda.tp+lda.fp)*(lda.tp+lda.fn)*(lda.tn+lda.fp)*(lda.tn+lda.fn))
    if(lda.den==0){
      lda.mcc[k,i] <- 0
    }else{
      lda.mcc[k,i] <- lda.num/lda.den
    }
    
    ##QDA
    qda.tran <- qda(q4.y~q4.x,subset=q4.tran)
    qda.pred <- predict(qda.tran,data.frame(q4.x))$posterior[,2][q4.test]
    qda.desn <- rep(0,length(q4.test))
    qda.desn[qda.pred > alpha[i]] <- 1
    qda.tp <- sum(qda.desn[q4.y[q4.test]==1]==1)
    qda.tn <- sum(qda.desn[q4.y[q4.test]==0]==0)
    qda.fp <- sum(qda.desn[q4.y[q4.test]==0]==1)
    qda.fn <- sum(qda.desn[q4.y[q4.test]==1]==0)
    qda.num <- qda.tp*qda.tn-qda.fp*qda.fn
    qda.den <- sqrt((qda.tp+qda.fp)*(qda.tp+qda.fn)*(qda.tn+qda.fp)*(qda.tn+qda.fn))
    if(qda.den==0){
      qda.mcc[k,i] <- 0
    }else{
      qda.mcc[k,i] <- qda.num/qda.den
    }
    
    ##NB
    nb.tran <- naiveBayes(Private~.,data=q4.College,subset=q4.tran)
    nb.pred <- predict(nb.tran,q4.College,type='raw')[,2][q4.test]
    nb.desn <- rep(0,length(q4.test))
    nb.desn[nb.pred > alpha[i]] <- 1
    nb.tp <- sum(nb.desn[q4.y[q4.test]==1]==1)
    nb.tn <- sum(nb.desn[q4.y[q4.test]==0]==0)
    nb.fp <- sum(nb.desn[q4.y[q4.test]==0]==1)
    nb.fn <- sum(nb.desn[q4.y[q4.test]==1]==0)
    nb.num <- nb.tp*nb.tn-nb.fp*nb.fn
    nb.den <- sqrt((nb.tp+nb.fp)*(nb.tp+nb.fn)*(nb.tn+nb.fp)*(nb.tn+nb.fn))
    if(nb.den==0){
      nb.mcc[k,i] <- 0
    }else{
      nb.mcc[k,i] <- nb.num/nb.den
    }
  }
}
q4.logit.cvs <- apply(logit.mcc,2,mean)
q4.logit.alpha <- alpha[which(q4.logit.cvs==max(q4.logit.cvs))]
q4.logit.desn <- rep(0,length(test))
q4.logit.desn[q2.logit.pred > q4.logit.alpha] <- 1
q4.logit.tp <- sum(q4.logit.desn[y.num[test]==1]==1)
q4.logit.tn <- sum(q4.logit.desn[y.num[test]==0]==0)
q4.logit.fp <- sum(q4.logit.desn[y.num[test]==0]==1)
q4.logit.fn <- sum(q4.logit.desn[y.num[test]==1]==0)
q4.logit.num <- q4.logit.tp*q4.logit.tn-q4.logit.fp*q4.logit.fn
q4.logit.den <-
  sqrt((q4.logit.tp+q4.logit.fp)*(q4.logit.tp+q4.logit.fn)*(q4.logit.tn+q4.logit.fp)*(q4.logit.tn+
                                                                                        q4.logit.fn))
q4.logit.mcc <- q4.logit.num/q4.logit.den


q4.lda.cvs <- apply(lda.mcc,2,mean)
q4.lda.alpha <- alpha[which(q4.lda.cvs==max(q4.lda.cvs))]
q4.lda.desn <- rep(0,length(test))
q4.lda.desn[q2.lda.pred > q4.lda.alpha] <- 1
q4.lda.tp <- sum(q4.lda.desn[y.num[test]==1]==1)
q4.lda.tn <- sum(q4.lda.desn[y.num[test]==0]==0)
q4.lda.fp <- sum(q4.lda.desn[y.num[test]==0]==1)
q4.lda.fn <- sum(q4.lda.desn[y.num[test]==1]==0)
q4.lda.num <- q4.lda.tp*q4.lda.tn-q4.lda.fp*q4.lda.fn
q4.lda.den <-
  sqrt((q4.lda.tp+q4.lda.fp)*(q4.lda.tp+q4.lda.fn)*(q4.lda.tn+q4.lda.fp)*(q4.lda.tn+q4.lda.fn)
  )
q4.lda.mcc <- q4.lda.num/q4.lda.den


q4.qda.cvs <- apply(qda.mcc,2,mean)
q4.qda.alpha <- alpha[which(q4.qda.cvs==max(q4.qda.cvs))]
q4.qda.desn <- rep(0,length(test))
q4.qda.desn[q2.qda.pred > q4.qda.alpha] <- 1
q4.qda.tp <- sum(q4.qda.desn[y.num[test]==1]==1)
q4.qda.tn <- sum(q4.qda.desn[y.num[test]==0]==0)
q4.qda.fp <- sum(q4.qda.desn[y.num[test]==0]==1)
q4.qda.fn <- sum(q4.qda.desn[y.num[test]==1]==0)
q4.qda.num <- q4.qda.tp*q4.qda.tn-q4.qda.fp*q4.qda.fn
q4.qda.den <- sqrt((q4.qda.tp+q4.qda.fp)*(q4.qda.tp+q4.qda.fn)*(q4.qda.tn+q4.qda.fp)*(q4.qda.tn+q4.qda.fn))
q4.qda.mcc <- q4.qda.num/q4.qda.den


q4.nb.cvs <- apply(nb.mcc,2,mean)
q4.nb.alpha <- mean(alpha[which(q4.nb.cvs==max(q4.nb.cvs))])
q4.nb.desn <- rep(0,length(test))
q4.nb.desn[q2.nb.pred > q4.nb.alpha] <- 1
q4.nb.tp <- sum(q4.nb.desn[y.num[test]==1]==1)
q4.nb.tn <- sum(q4.nb.desn[y.num[test]==0]==0)
q4.nb.fp <- sum(q4.nb.desn[y.num[test]==0]==1)
q4.nb.fn <- sum(q4.nb.desn[y.num[test]==1]==0)
q4.nb.num <- q4.nb.tp*q4.nb.tn-q4.nb.fp*q4.nb.fn
q4.nb.den <-
  sqrt((q4.nb.tp+q4.nb.fp)*(q4.nb.tp+q4.nb.fn)*(q4.nb.tn+q4.nb.fp)*(q4.nb.tn+q4.nb.fn))
q4.nb.mcc <- q4.nb.num/q4.nb.den


q4.alpha <- c(q4.logit.alpha,q4.lda.alpha,q4.qda.alpha,q4.nb.alpha)
q4.mcc <- c(q4.logit.mcc,q4.lda.mcc,q4.qda.mcc,q4.nb.mcc)
ans4 <- rbind(q4.alpha,q4.mcc)
colnames(ans4) <- c('LR','LDA','QDA','NB')
rownames(ans4) <- c('alpha_hat','MCC')
ans4


###Q5
q5.yy <- y[tran]
q5.College <- College[tran,]
q5.y <- y.num[tran]
q5.x <- x[tran,]
alpha <- seq(0,1,0.01)
K <- 10
fun.f1 <- function(x,y=q5.y,z=q5.test,thred=alpha){
  f1 <- NA
  for(i in 1:length(thred)){
    desn <- rep(0,length(z))
    desn[x > thred[i]] <- 1
    tp <- sum(desn[y[z]==1]==1)
    tn <- sum(desn[y[z]==0]==0)
    fp <- sum(desn[y[z]==0]==1)
    fn <- sum(desn[y[z]==1]==0)
    f1[i] <- 2*tp/(2*tp+fp+fn)
  }
  return(f1)
}
logit.f1 <- lda.f1 <- qda.f1 <- nb.f1 <- matrix(NA,K,length(alpha))
for(k in 1:K){
  q5.tran <- which(gr!=k)
  q5.test <- which(gr==k)
  
  ##LR
  logit.tran <- glm(q5.y~q5.x,family='binomial',subset=q5.tran)
  logit.pred <- predict(logit.tran,data.frame(q5.x),type='response')[q5.test]
  logit.f1[k,] <- fun.f1(x=logit.pred,y=q5.y,z=q5.test,thred=alpha)
  
  ##LDA
  lda.tran <- lda(q5.y~q5.x,subset=q5.tran)
  lda.pred <- predict(lda.tran,data.frame(q5.x))$posterior[,2][q5.test]
  lda.f1[k,] <- fun.f1(x=lda.pred,y=q5.y,z=q5.test,thred=alpha)
  
  ##QDA
  qda.tran <- qda(q5.y~q5.x,subset=q5.tran)
  qda.pred <- predict(qda.tran,data.frame(q5.x))$posterior[,2][q5.test]
  qda.f1[k,] <- fun.f1(x=qda.pred,y=q5.y,z=q5.test,thred=alpha)
  
  ##NB
  nb.tran <- naiveBayes(Private~.,data=q5.College,subset=q5.tran)
  nb.pred <- predict(nb.tran,q5.College,type='raw')[,2][q5.test]
  nb.f1[k,] <- fun.f1(x=nb.pred,y=q5.y,z=q5.test,thred=alpha)
}
q5.logit.cvs <- apply(logit.f1,2,mean)
q5.logit.alpha <- alpha[which(q5.logit.cvs==max(q5.logit.cvs))]
q5.logit.f1 <- fun.f1(x=q2.logit.pred,y=y.num,z=test,thred=q5.logit.alpha)


q5.lda.cvs <- apply(lda.f1,2,mean)
q5.lda.alpha <- alpha[which(q5.lda.cvs==max(q5.lda.cvs))]
q5.lda.f1 <- fun.f1(x=q2.lda.pred,y=y.num,z=test,thred=q5.lda.alpha)


q5.qda.cvs <- apply(qda.f1,2,mean)
q5.qda.alpha <- mean(alpha[which(q5.qda.cvs==max(q5.qda.cvs))])
q5.qda.f1 <- fun.f1(x=q2.qda.pred,y=y.num,z=test,thred=q5.qda.alpha)


q5.nb.cvs <- apply(nb.f1,2,mean)
q5.nb.alpha <- mean(alpha[which(q5.nb.cvs==max(q5.nb.cvs))])
q5.nb.f1 <- fun.f1(x=q2.nb.pred,y=y.num,z=test,thred=q5.nb.alpha)


q5.alpha <- c(q5.logit.alpha,q5.lda.alpha,q5.qda.alpha,q5.nb.alpha)
q5.f1 <- c(q5.logit.f1,q5.lda.f1,q5.qda.f1,q5.nb.f1)
ans5 <- rbind(q5.alpha,q5.f1)
colnames(ans5) <- c('LR','LDA','QDA','NB')
rownames(ans5) <- c('alpha_hat','F1')
ans5




###Q6
q6.alpha <- rbind(q3.alpha,q4.alpha,q5.alpha)
colnames(q6.alpha) <- c('LR','LDA','QDA','NB')
rownames(q6.alpha) <- paste0('Q',1:3)
q6.alpha

q6.perf <- rbind(ans1,ans2,q3.miss,q4.mcc,q5.f1)
colnames(q6.perf) <- c('LR','LDA','QDA','NB')
rownames(q6.perf) <- c('Q1(Mis)','Q2(AUC)','Q3(Mis)','Q4(MCC)','Q5(F1)')
q6.perf





