#######################문제1#################################
#install.packages('ISLR')
library(ISLR)
data('Wage')
str(Wage)
w <- Wage
Wage<-w[,-c(6,11)]
str(Wage)

x <- Wage[,-9]
y <- Wage[,9]
dim(x)
head(y)

w1 <- Wage[Wage$logwage>median(Wage$logwage),]
w2 <- Wage[Wage$logwage<=median(Wage$logwage),]
dim(w1)
dim(w2)

#install.packages("leaps")
library(leaps)
g.big1 <- regsubsets(logwage ~., data=w1, nvmax = 16)
g.big2 <- regsubsets(logwage ~., data=w2, nvmax = 16)



# nbest:기록할 각각의 크기의 하위 집합/ nvmax : 검사 할 하위 집합의 최대 크기
sg1 <- summary(g.big1)
names(sg1)
dim(sg1$which)
sg1$which[1:5,]
sg1.size <- as.numeric(rownames(sg1$which))
table(sg1.size)
names(sg1)
# "which"  "rsq":r^2  "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   
length(sg1$bic)
summary(sg1$bic)
sg1.bic <- tapply(sg1$bic, sg1.size, min)
sg1.bic

length(sg1$cp)
summary(sg1$cp)
sg1.cp <- tapply(sg1$cp, sg1.size, min)
sg1.cp



sg2 <- summary(g.big2)
names(sg2)
dim(sg2$which)
sg2$which[1:5,]
sg2.size <- as.numeric(rownames(sg2$which))
table(sg2.size)
names(sg2)
# "which"  "rsq":r^2  "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   
length(sg2$bic)
summary(sg2$bic)
sg2.bic <- tapply(sg2$bic, sg2.size, min)
sg2.bic

length(sg2$cp)
summary(sg2$cp)
sg2.cp <- tapply(sg2$cp, sg2.size, min)
sg2.cp

which.min(sg1$bic)
sg1$bic[which.min(sg1$bic)]
which.min(sg1$cp)
sg1$cp[which.min(sg1$cp)]
which.min(sg2$bic)
sg2$bic[which.min(sg2$bic)]
which.min(sg2$cp)
sg2$cp[which.min(sg2$cp)]

par(mfrow=c(2,2))
plot(1:16, sg1.bic, type="b", xlab ="Number of Predictors", ylab=
       "Group 1 BIC", col=1, pch=19,
     ylim=c(min(sg1$bic),max(sg1$bic)))
points(which.min(sg1$bic), sg1$bic[which.min(sg1$bic)], pch=19, col="red", cex=1.5)


plot(1:16, sg1.cp, type="b", xlab ="Number of Predictors", ylab=
       "Group 1 AIC", col=1, pch=19,
     ylim=c(min(sg1$cp),max(sg1$cp)))
points(which.min(sg1$cp), sg1$cp[which.min(sg1$cp)], pch=19, col="red", cex=1.5)

plot(1:16, sg2.bic, type="b", xlab ="Number of Predictors", ylab=
       "Group 2 BIC", col=1, pch=19,
     ylim=c(min(sg2$bic),max(sg2$bic)))
points(which.min(sg2$bic), sg2$bic[which.min(sg2$bic)], pch=19, col="red", cex=1.5)


plot(1:16, sg2.cp, type="b", xlab ="Number of Predictors", ylab=
       "Group 2 AIC", col=1, pch=19,
     ylim=c(min(sg2$cp),max(sg2$cp)))
points(which.min(sg2$cp), sg2$cp[which.min(sg2$cp)], pch=19, col="red", cex=1.5)

model1 <- coef(g.big1, which.min(sg1$bic))
model2 <- coef(g.big1, which.min(sg1$cp))
model3 <- coef(g.big2, which.min(sg2$bic))
model4 <- coef(g.big2, which.min(sg2$cp))

RES <- matrix(0, 17, 4)
gfull <- lm(logwage ~., data=w1)
full.names <- names(gfull$coef)
for (i in 1:4) {
  model <- get(paste("model", i, sep=""))
  w <- match(names(model), full.names)
  RES[w, (i)] <- model
}
rownames(RES) <- full.names
colnames(RES) <- c("w1_bic", "w1_aic","w2_bic", "w2_aic")
RES


#######################문제2#################################
RNGkind(sample.kind = "Rounding")
set.seed(111)
u1 <- sample(rep(seq(10), length=sum(Wage$logwage>median(Wage$logwage))))
u2 <- sample(rep(seq(10), length=sum(Wage$logwage<=median(Wage$logwage))))

## Define new 'predict' function on regsubset
predict.regsubsets <- function(object ,newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  mat[ ,names(coefi)]%*%coefi
}

K <- 10
cv.errors <- matrix(NA, K, 16)
colnames(cv.errors) <- paste(1:16)
for (i in 1:K) {
  fit <- regsubsets(logwage ~ ., data=w1[u1!=i, ], nvmax=16)
  for (j in 1:16) {
    pred <- predict(fit, w1[u1==i, ], id = j)
    cv.errors[i, j] <- sqrt(mean((w1$logwage[u1==i] - pred)^2))
  }
}

par(mfrow=c(2,2))
sq <- apply(cv.errors, 2, mean)
sq
plot(1:16, sq, type="l", col="red", xlab="Number of Predictors",
     ylab="Group 1 Cross-Validation Error")
points(1:16, sq, pch=19, col="blue")
points(which.min(sq), sq[which.min(sq)], pch="x", col="blue", cex=2)
PE_min_1 <- sq[which.min(sq)]


## One-standard-error rule
se <- apply(cv.errors, 2, sd)
avg <- apply(cv.errors, 2, mean)
PE <- cbind(avg - se, avg, avg + se) # boundary
matplot(1:16, PE, type="l", col=c(1,2,1), lty=c(3,1,3),
        xlab="Number of Predictors", ylab="Group1 Cross-Validation Error",
        ylim=c(min(PE), max(PE)))
points(1:16, PE[,2], pch=19, col="blue")
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
up
points(min(up), PE[min(up),2], pch="x", col="blue", cex=2)
PE_1se_1 <- PE[min(up),2]

fit <- regsubsets(logwage ~ ., data=w1, nvmax=16)
coef(fit, which.min(sq))
coef(fit, min(up))
model1 <- coef(fit, which.min(sq))
model2 <- coef(fit, min(up))


#data2
K <- 10
cv.errors <- matrix(NA, K, 16)
colnames(cv.errors) <- paste(1:16)
for (i in 1:K) {
  fit <- regsubsets(logwage ~ ., data=w2[u2!=i, ], nvmax=16)
  for (j in 1:16) {
    pred <- predict(fit, w2[u2==i, ], id = j)
    cv.errors[i, j] <- sqrt(mean((w2$logwage[u2==i] - pred)^2))
  }
}

sq <- apply(cv.errors, 2, mean)
sq
plot(1:16, sq, type="l", col="red", xlab="Number of Predictors",
     ylab="Group 2 Cross-Validation Error")
points(1:16, sq, pch=19, col="blue")
points(which.min(sq), sq[which.min(sq)], pch="x", col="blue", cex=2)
PE_min_2 <- sq[which.min(sq)]


## One-standard-error rule
se <- apply(cv.errors, 2, sd)
avg <- apply(cv.errors, 2, mean)
PE <- cbind(avg - se, avg, avg + se) # boundary
matplot(1:16, PE, type="l", col=c(1,2,1), lty=c(3,1,3),
        xlab="Number of Predictors", ylab="Group2 Cross-Validation Error",
        ylim=c(min(PE), max(PE)))
points(1:16, PE[,2], pch=19, col="blue")
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
up
points(min(up), PE[min(up),2], pch="x", col="blue", cex=2)
PE_1se_2 <- PE[min(up),2]

PE_min_1
PE_1se_1
PE_min_2
PE_1se_2


fit <- regsubsets(logwage ~ ., data=w2, nvmax=16)
coef(fit, which.min(sq))
coef(fit, min(up))
model3 <- coef(fit, which.min(sq))
model4 <- coef(fit, min(up))

RES <- matrix(0, 17, 4)
gfull <- lm(logwage ~., data=w1)
full.names <- names(gfull$coef)
for (i in 1:4) {
  model <- get(paste("model", i, sep=""))
  w <- match(names(model), full.names)
  RES[w, (i)] <- model
}
rownames(RES) <- full.names
colnames(RES) <- c("w1_min", "w1_1se","w2_min", "w2_1se")
RES
RES <- RES[-1,]
RES[RES!=0] <- 1
RES
PE <- c(PE_min_1, PE_1se_1, PE_min_2, PE_1se_2)
RES <- rbind(RES, PE)
RES

#######################문제3#################################
#install.packages("glmnet")
library(glmnet)

data(NCI60)
x <- NCI60$data
RNGkind(sample.kind = "Rounding")
set.seed(123)
beta <- rep(0, ncol(x))
beta[1:50] <- runif(50, -2, 2)
y <- x %*% beta + rnorm(nrow(x))
lambda <- 10^seq(2, -2, length=300)
foldid <- sample(rep(seq(5), length=length(y)))
dim(x)


alpha <- seq(0, 1, 0.05)
table(foldid)
cvm <- NULL
for (i in 1:length(alpha)) {
  fit = cv.glmnet(x, y, alpha=alpha[i], lambda = lambda, foldid = foldid)
  cvm[i] <- min(fit$cvm)
}
# alpha 1개에 대해서 lambda가 300개, 그 중 미니멈

cbind(alpha,cvm) #여기선 alpha=0.65일때 cvm이 가장 낮았다

a <- alpha[which.min(cvm)] #alpha=0.65일때로 설정
a
cvfit <- cv.glmnet(x, y, alpha=a, lambda = lambda, foldid=foldid) # 위에서 지정한 5-fold 그룹 사용
cvfit
cvfit$lambda.min
cvfit$lambda.1se

par(mfrow=c(1,1))
plot(cvfit)

fit1 <- coef(cvfit, s = "lambda.min")
fit2 <- coef(cvfit, s = "lambda.1se")
c(sum(as.matrix(fit1)!=0)-1, sum(as.matrix(fit2)!=0)-1)
wh1 <- which(fit1!=0)
w1 <- wh1[-1]-1
w1
wh2 <- which(fit2!=0)
w2 <- wh2[-1]-1
w2
#######################문제4#################################
#install.packages("tidyverse")
library(tidyverse)
g <- glmnet(x, y, alpha=1, lambda=lambda)
pred <- NULL
temp <- NULL
K <- 5

for (j in 1:length(lambda)) {
  bhat <- coef(g, s=lambda[j])
  wh <- which(as.matrix(bhat)!=0) # 어느 지점이 0이 아닌지
  w <- wh[-1]-1
  w <- na.omit(w)
  
  for (i in 1:K) {
    tran <- which(foldid!=i)
    bhat1 <- rep(0, length(bhat)+1)
    xt<-cbind(1,x)
    
    if(length(w)<nrow(x[tran,])){
      if (is_empty(w)) {
        bhat1[wh] <- lm(y[tran] ~ 1)$coef
        bhat1 <- bhat1[-2]
        
      } else {
        bhat2 <- lm(y[tran] ~ x[tran, w], data = data.frame(x[tran,]))$coef
        
        for (p in 1:length(wh)) {
          bhat1[wh[p]+1] <- bhat2[p]}
        bhat1 <- bhat1[-1]
        bhat1[is.na(bhat1)] <- 0
      }
      temp[i] <- sqrt(mean((y[-tran] - xt[-tran,] %*% bhat1)^2))
      
    } else {
      w <- w[1:nrow(x[tran,])-1]
      bhat2 <- lm(y[tran] ~ x[tran, w], data = data.frame(x[tran,]))$coef
      ww <- wh[1:nrow(x[tran,])-1]
      
      for (p in 1:length(ww)) {
        bhat1[ww[p]] <- bhat2[p]}
      bhat1 <- bhat1[-1]
      bhat1[is.na(bhat1)] <- 0
      temp[i] <- sqrt(mean((y[-tran] - xt[-tran,] %*% bhat1)^2))
    }
  }
  pred <- cbind(pred, temp)
}
t(pred)
pred
colnames(pred) <- lambda
dim(pred)
se <- apply(pred, 2, sd)
avg <- apply(pred, 2, mean)
PE <- cbind(avg - se, avg, avg + se) # boundary
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
up
PE
fit1 <- lambda[min(up)]
fit2 <- lambda[min(which.min(avg))]
coef(g, lambda[min(up)])
coef(g, lambda[min(which.min(avg))])



which.min(PE[,2])
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
min(up)
lambda[min(up)]
min <- coef(g, lambda[which.min(PE[,2])])
which(min!=0)-1  # first term means intercept
dim(x[,which(min!=0)[-1]-1])
one_se <- coef(g, lambda[min(up)])
which(one_se!=0)-1  # first term means intercept
dim(x[,which(one_se!=0)[-1]-1])


par(mfrow=c(2,1))
sq <- apply(pred, 2, mean)
sq
plot(1:300, sq, type="l", col="red", xlab="which min lambda",
     ylab="lasso PE")
points(which.min(sq), sq[which.min(sq)], pch="x", col="blue", cex=2)

## One-standard-error rule
se <- apply(pred, 2, sd)
avg <- apply(pred, 2, mean)
PE <- cbind(avg - se, avg, avg + se) # boundary
matplot(1:300, PE, type="l", col=c(1,2,1), lty=c(3,1,3),
        xlab="which 1se lambda", ylab="lasso PE",
        ylim=c(min(PE), max(PE)))
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
min(up)
points(min(up), PE[min(up),2], pch="x", col="blue", cex=2)


#######################문제5#################################
alpha <- seq(0, 1, 0.05)
temp <- NULL
K <- 5
PE <- NULL

for (a in 1:length(alpha)) {
  g <- glmnet(x, y, alpha=alpha[a], lambda=lambda)
  pred <- NULL
  
  for (j in 1:length(lambda)) {
    bhat <- coef(g, s=lambda[j])
    wh <- which(as.matrix(bhat)!=0) # 어느 지점이 0이 아닌지
    w <- wh[-1]-1
    w <- na.omit(w)
    
    for (i in 1:K) {
      tran <- which(foldid!=i)
      bhat1 <- rep(0, length(bhat)+1)
      xt<-cbind(1,x)
      
      if(length(w)<nrow(x[tran,])){
        if (is_empty(w)) {
          bhat1[wh] <- lm(y[tran] ~ 1)$coef
          bhat1 <- bhat1[-2]
          
        } else {
          bhat2 <- lm(y[tran] ~ x[tran, w], data = data.frame(x[tran,]))$coef
          
          for (p in 1:length(wh)) {
            bhat1[wh[p]+1] <- bhat2[p]}
          bhat1 <- bhat1[-1]
          bhat1[is.na(bhat1)] <- 0
        }
        temp[i] <- sqrt(mean((y[-tran] - xt[-tran,] %*% bhat1)^2))
        
      } else {
        w <- w[1:nrow(x[tran,])-1]
        bhat2 <- lm(y[tran] ~ x[tran, w], data = data.frame(x[tran,]))$coef
        ww <- wh[1:nrow(x[tran,])-1]
        
        for (p in 1:length(ww)) {
          bhat1[ww[p]] <- bhat2[p]}
        bhat1 <- bhat1[-1]
        bhat1[is.na(bhat1)] <- 0
        temp[i] <- sqrt(mean((y[-tran] - xt[-tran,] %*% bhat1)^2))
      }
    }
    pred[j] <- mean(temp, na.rm=T)
  }
  PE[a] <- min(pred, na.rm=T)
}

PE
cbind(alpha,PE) #여기선 alpha=1일때 PE가 가장 낮았다

a <- alpha[which.min(PE)] #alpha=1일때로 설정
a


g <- glmnet(x, y, alpha=a, lambda=lambda)
pred <- NULL
temp <- NULL
K <- 5

for (j in 1:length(lambda)) {
  bhat <- coef(g, s=lambda[j])
  wh <- which(as.matrix(bhat)!=0) # 어느 지점이 0이 아닌지
  w <- wh[-1]-1
  w <- na.omit(w)
  
  for (i in 1:K) {
    tran <- which(foldid!=i)
    bhat1 <- rep(0, length(bhat)+1)
    xt<-cbind(1,x)
    
    if(length(w)<nrow(x[tran,])){
      if (is_empty(w)) {
        bhat1[wh] <- lm(y[tran] ~ 1)$coef
        bhat1 <- bhat1[-2]
        
      } else {
        bhat2 <- lm(y[tran] ~ x[tran, w], data = data.frame(x[tran,]))$coef
        
        for (p in 1:length(wh)) {
          bhat1[wh[p]+1] <- bhat2[p]}
        bhat1 <- bhat1[-1]
        bhat1[is.na(bhat1)] <- 0
      }
      temp[i] <- sqrt(mean((y[-tran] - xt[-tran,] %*% bhat1)^2))
      
    } else {
      w <- w[1:nrow(x[tran,])-1]
      bhat2 <- lm(y[tran] ~ x[tran, w], data = data.frame(x[tran,]))$coef
      ww <- wh[1:nrow(x[tran,])-1]
      
      for (p in 1:length(ww)) {
        bhat1[ww[p]] <- bhat2[p]}
      bhat1 <- bhat1[-1]
      bhat1[is.na(bhat1)] <- 0
      temp[i] <- sqrt(mean((y[-tran] - xt[-tran,] %*% bhat1)^2))
    }
  }
  pred <- cbind(pred, temp)
}



colnames(pred) <- lambda
dim(pred)
se <- apply(pred, 2, sd)
avg <- apply(pred, 2, mean)
PE <- cbind(avg - se, avg, avg + se) # boundary
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
up
PE
fit1 <- lambda[min(up)]
fit2 <- lambda[min(which.min(avg))]
coef(g, lambda[min(up)])
coef(g, lambda[min(which.min(avg))])



which.min(PE[,2])
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
min(up)
lambda[min(up)]
min <- coef(g, lambda[which.min(PE[,2])])
which(min!=0)-1  # first term means intercept
dim(x[,which(min!=0)[-1]-1])
one_se <- coef(g, lambda[min(up)])
which(one_se!=0)-1  # first term means intercept
dim(x[,which(one_se!=0)[-1]-1])


par(mfrow=c(2,1))
sq <- apply(pred, 2, mean)
sq
plot(1:300, sq, type="l", col="red", xlab="which min lambda",
     ylab="fit alpha PE")
points(which.min(sq), sq[which.min(sq)], pch="x", col="blue", cex=2)

## One-standard-error rule
se <- apply(pred, 2, sd)
avg <- apply(pred, 2, mean)
PE <- cbind(avg - se, avg, avg + se) # boundary
matplot(1:300, PE, type="l", col=c(1,2,1), lty=c(3,1,3),
        xlab="which 1se lambda", ylab="fit alpha PE",
        ylim=c(min(PE), max(PE)))
up <- which(PE[,2] < PE[which.min(PE[,2]),3]) # 최솟값 PE 범위에 있는 모델
up
points(min(up), PE[min(up),2], pch="x", col="blue", cex=2)


