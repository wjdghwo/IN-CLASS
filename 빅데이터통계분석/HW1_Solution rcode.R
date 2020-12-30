library(MASS)
data(Boston)
y <- Boston[-55, 1]
x <- Boston[-55, -c(1,4,8,9)]
x <- as.matrix(scale(x))
RNGkind(sample.kind = "Rounding")
set.seed(123)
tran <- sample(nrow(x), 450)
###Q1
model <- 10
df <- 15
pe.poly <- matrix(0,model,df)
for(i in 1:model){
  for(j in 1:df){
    m.poly <- lm(y~poly(x[,i],j),subset=tran)
    pe.poly[i,j] <- sqrt(mean((y - predict(m.poly,as.data.frame(x[,i])))[-tran]^2))
  }
}
q1.pe.ploy <- apply(pe.poly,1,min)
q1.pos.ploy <- apply(pe.poly,1,function(x) which(x==min(x)))
q1.ans <- cbind(q1.pos.ploy,q1.pe.ploy)
colnames(q1.ans) <- c('k','PE')
rownames(q1.ans) <- paste0('Model',1:10)
q1.ans



pe.spline <- matrix(0,model,df)
for(i in 1:model){
  for(j in 2:df){
    m.spline <- smooth.spline(x[tran,i],y[tran],df=j)
    pe.spline[i,j] <- sqrt(mean((y-predict(m.spline,x[,i])$y)[-tran]^2))
  }
}
q2.pe.spline <- apply(pe.spline[,-1],1,min)
q2.pos.spline <- apply(pe.spline[,-1],1,function(x) which(x==min(x)))+1
q2.ans <- cbind(q2.pos.spline,q2.pe.spline)
colnames(q2.ans) <- c('k','PE')
rownames(q2.ans) <- paste0('Model',1:10)
q2.ans


###Q3
pe.step <- matrix(0,model,df)
for(i in 1:model){
  for(j in 2:df){
    step.temp <- cut(x[,i],j)
    m.step <- lm(y~step.temp,subset=tran)
    pe.step[i,j] <- sqrt(mean((y-predict(m.step,as.data.frame(step.temp)))[-tran]^2))
  }
}
q3.pe.step <- apply(pe.step[,-1],1,min)
q3.pos.step <- apply(pe.step[,-1],1,function(x) which(x==min(x)))+1
q3.ans <- cbind(q3.pos.step,q3.pe.step)
colnames(q3.ans) <- c('k','PE')
rownames(q3.ans) <- paste0('Model',1:10)
q3.ans



###Q4
RNGkind(sample.kind = "Rounding")
set.seed(123)
u <- sample(rep(seq(10), length=length(y)))
K <- 10
q4.pe.poly <- q4.pe.spline <- q4.pe.step <- array(0,c(model,df,K))
for(k in 1:K){
  tran <- which(u!=k)
  test <- which(u==k)
  for(i in 1:model){
    for(j in 1:df){
      m.poly <- lm(y~poly(x[,i],j),subset=tran)
      q4.pe.poly[i,j,k] <- sqrt(mean((y - predict(m.poly,as.data.frame(x[,i])))[-tran]^2))
    }
    for(j in 2:df){
      m.spline <- smooth.spline(x[tran,i],y[tran],df=j)
      q4.pe.spline[i,j,k] <- sqrt(mean((y-predict(m.spline,x[,i])$y)[-tran]^2))
      step.temp <- cut(x[,i],j)
      pos.zero.tran <- which(table(step.temp[tran])==0)
      pos.test <- table(step.temp[test])[pos.zero.tran]
      if(sum(pos.test!=0)!=0){
        q4.pe.step[i,j,k] <- NA
      }else{
        m.step <- lm(y~step.temp,subset=tran)
        q4.pe.step[i,j,k] <-
          sqrt(mean((y-predict(m.step,as.data.frame(step.temp)))[-tran]^2))
      }
    }
  }
}
pe.ave.poly <- apply(q4.pe.poly,c(1,2),mean)
pe.poly.res <- cbind(apply(pe.ave.poly,1,function(x)
  which(x==min(x))),apply(pe.ave.poly,1,min))
pe.ave.spline <- apply(q4.pe.spline,c(1,2),mean)
pe.spline.res <- cbind(apply(pe.ave.spline[,-1],1,function(x)
  which(x==min(x)))+1,apply(pe.ave.spline[,-1],1,min))
pe.ave.step <- apply(q4.pe.step,c(1,2),function(x) mean(x,na.rm=TRUE))
pe.step.res <- cbind(apply(pe.ave.step[,-1],1,function(x)
  which.min(x))+1,apply(pe.ave.step[,-1],1,function(x) min(x,na.rm=TRUE)))
q4.ans <- cbind(pe.poly.res,pe.spline.res,pe.step.res)
colnames(q4.ans) <- rep(c('k','PE'),3)
rownames(q4.ans) <- paste0('Model',1:10)
q4.ans

'''
###Q5
The best model when we use 10-fold cross-validation is the step function (model
                                                                          7) with k equals to 13. The best model when we only use validation set is the step
function (model 2) when k equals to 10.
Note: The step function is used to convert a continuous variable into an ordered
categorical variable. However, in our homework, distributions of predictor are
varied. For example, we converted x[,1] to categorical data when k equals to 11,
interval    only includes 3 observations and all of them are alloted to test
set, so that there would be a problem when use the predict function because the
trained model does not include this categorical level. So, we will ignore these
similar cases during our homework.
'''