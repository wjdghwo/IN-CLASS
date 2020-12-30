###Q1
library(ISLR)
data(Wage)
wage <- Wage[,-c(6,11)]
y <- wage[,9]
grid <- expand.grid(rep(list(c(0,1)),8))[-1,]
grp1 <- (y > median(y))
grp2 <- !grp1
grp1.size <- sum(grp1)
grp2.size <- sum(grp2)
grp1.aic <- grp1.bic <- grp2.aic <- grp2.bic <- 0
for(i in 1:nrow(grid)){
  wage.data <- wage[,c(grid[i,] %in% 1,TRUE)]
  grp1.lm <- lm(logwage~.,data=wage.data,subset=grp1)
  grp1.mse <- mean((y[grp1]- grp1.lm$fitted)^2)
  grp1.aic[i] <- grp1.size*log(grp1.mse)+2*length(coef(grp1.lm))
  grp1.bic[i] <- grp1.size*log(grp1.mse)+length(coef(grp1.lm))*log(grp1.size)
  grp2.lm <- lm(logwage~.,data=wage.data,subset=grp2)
  grp2.mse <- mean((y[grp2]- grp2.lm$fitted)^2)
  grp2.aic[i] <- grp2.size*log(grp2.mse)+2*length(coef(grp2.lm))
  grp2.bic[i] <- grp2.size*log(grp2.mse)+length(coef(grp2.lm))*log(grp2.size)
}
grp1.aic.pos <- which(grp1.aic==min(grp1.aic))
grp1.aic.min <- min(grp1.aic)
grp1.bic.pos <- which(grp1.bic==min(grp1.bic))
grp1.bic.min <- min(grp1.bic)
grp1.aic.best <- grid[grp1.aic.pos,]
grp1.bic.best <- grid[grp1.bic.pos,]

grp1.aic.ans <- cbind(grp1.aic.best,grp1.aic.min)
grp1.bic.ans <- cbind(grp1.bic.best,grp1.bic.min)

colnames(grp1.aic.ans) <- c(colnames(wage)[-9],'AIC')
colnames(grp1.bic.ans) <- c(colnames(wage)[-9],'BIC')

grp2.aic.pos <- which(grp2.aic==min(grp2.aic))
grp2.aic.min <- min(grp2.aic)
grp2.bic.pos <- which(grp2.bic==min(grp2.bic))
grp2.bic.min <- min(grp2.bic)
grp2.aic.best <- grid[grp2.aic.pos,]
grp2.bic.best <- grid[grp2.bic.pos,]

grp2.aic.ans <- cbind(grp2.aic.best,grp2.aic.min)
grp2.bic.ans <- cbind(grp2.bic.best,grp2.bic.min)

colnames(grp2.aic.ans) <- c(colnames(wage)[-9],'AIC')
colnames(grp2.bic.ans) <- c(colnames(wage)[-9],'BIC')

grp1.aic.ans
grp1.bic.ans
grp2.aic.ans
grp2.bic.ans

# '1' means corresponding predictor is included in the model, otherwise '0'.

###Q2
RNGkind(sample.kind = "Rounding")
set.seed(111)
u1 <- sample(rep(seq(10), length=sum(Wage$logwage>median(Wage$logwage))))
u2 <- sample(rep(seq(10), length=sum(Wage$logwage<=median(Wage$logwage))))
K <- 10
grp1.pe <- grp2.pe <- matrix(NA,nrow(grid),K)
for(k in 1:K){
  grp1.tran <- which(u1!=k)
  grp1.test <- which(u1==k)
  grp2.tran <- which(u2!=k)
  grp2.test <- which(u2==k)
  for(i in 1:nrow(grid)){
    #group 1
    grp1.wage <- wage[grp1,c(grid[i,] %in% 1,TRUE)]
    grp1.lm <- lm(logwage~.,data=grp1.wage,subset=grp1.tran)
    grp1.pred <- predict(grp1.lm,grp1.wage)
    grp1.pe[i,k] <- sqrt(mean((y[grp1]-grp1.pred)[grp1.test]^2))
    #group 2
    grp2.wage <- wage[grp2,c(grid[i,] %in% 1,TRUE)]
    grp2.lm <- lm(logwage~.,data=grp2.wage,subset=grp2.tran)
    grp2.pred <- predict(grp2.lm,grp2.wage)
    grp2.pe[i,k] <- sqrt(mean((y[grp2]-grp2.pred)[grp2.test]^2))
  }
}
grp1.ave.pe <- apply(grp1.pe,1,mean)
grp1.min.pe <- min(grp1.ave.pe)
grp1.min.pos <- which(grp1.ave.pe==min(grp1.ave.pe))
grp1.min.model <- c(grid[grp1.min.pos,],grp1.min.pe)
grp1.se.pe <- apply(grp1.pe,1,sd)
grp1.add.pe <- grp1.ave.pe + grp1.se.pe
grp1.1se.temp <- which(grp1.ave.pe < grp1.add.pe[grp1.min.pos])
grp1.1se.num <- apply(grid[grp1.1se.temp,],1,sum)
grp1.1se.pe <- grp1.ave.pe[grp1.1se.temp[which(grp1.1se.num==min(grp1.1se.num))]]
grp1.1se.model <-
  c(grid[grp1.1se.temp[which(grp1.1se.num==min(grp1.1se.num))],],grp1.1se.pe)

grp1.ans <- rbind(grp1.min.model,grp1.1se.model)
colnames(grp1.ans) <- c(colnames(wage)[-9],'PE')
rownames(grp1.ans) <- c('minPE','1SE')

grp2.ave.pe <- apply(grp2.pe,1,mean)
grp2.min.pe <- min(grp2.ave.pe)
grp2.min.pos <- which(grp2.ave.pe==min(grp2.ave.pe))
grp2.min.model <- c(grid[grp2.min.pos,],grp2.min.pe)
grp2.se.pe <- apply(grp2.pe,1,sd)
grp2.add.pe <- grp2.ave.pe + grp2.se.pe
grp2.1se.temp <- which(grp2.ave.pe < grp2.add.pe[grp2.min.pos])
grp2.1se.num <- apply(grid[grp2.1se.temp,],1,sum)

grp2.1se.pe <- grp2.ave.pe[grp2.1se.temp[which(grp2.1se.num==min(grp2.1se.num))]]
grp2.1se.sm.pe <- min(grp2.1se.pe)
grp2.1se.sm.pos <- which.min(grp2.1se.pe)
grp2.1se.pos <- which(grp2.1se.num==min(grp2.1se.num))[grp2.1se.sm.pos]
grp2.1se.model <- c(grid[grp2.1se.pos,],grp2.1se.sm.pe)

grp2.ans <- rbind(grp2.min.model,grp2.1se.model)
colnames(grp2.ans) <- c(colnames(wage)[-9],'PE')
rownames(grp2.ans) <- c('minPE','1SE')

grp1.ans
grp2.ans


###Q3
library(glmnet)
data(NCI60)
x <- NCI60$data
RNGkind(sample.kind = "Rounding")
set.seed(123)
beta <- rep(0, ncol(x))
beta[1:50] <- runif(50, -2, 2)
y <- x %*% beta + rnorm(nrow(x))
colnames(x) <- paste0('G',1:6830)
lambda <- 10^seq(2, -2, length=300)
foldid <- sample(rep(seq(5), length=length(y)))
alpha <- seq(0,1,0.05)
cvm.q3 <- NA
lam.mat <- matrix(NA,2,length(alpha))
for(i in 1:length(alpha)){
  cv.fit.q3 <-
    cv.glmnet(x,y,type.measure='mse',alpha=alpha[i],lambda=lambda,foldid=foldid)
  cvm.q3[i] <- min(cv.fit.q3$cvm)
  lam.mat[1,i] <- cv.fit.q3$lambda.min
  lam.mat[2,i] <- cv.fit.q3$lambda.1se
}
alpha.pos <- which(cvm.q3==min(cvm.q3))
alpha.hat <- alpha[alpha.pos]
lambda.min.hat <- lam.mat[1,alpha.pos]
lambda.1se.hat <- lam.mat[2,alpha.pos]
cv.fit.q3 <-
  cv.glmnet(x,y,type.measure='mse',alpha=alpha.hat,lambda=lambda,foldid=foldid)
q3.min.coef.num <- sum(coef(cv.fit.q3, s = 'lambda.min')!=0)-1
q3.1se.coef.num <- sum(coef(cv.fit.q3, s = 'lambda.1se')!=0)-1

alpha.hat
lambda.min.hat
lambda.1se.hat
q3.min.coef.num
q3.1se.coef.num


###Q4
K <- 5
lambda.num <- length(lambda)
pe.new <- matrix(NA,K,lambda.num)
for(k in 1:K){
  tran <- which(foldid!=k)
  test <- which(foldid==k)
  fit.q4 <- glmnet(x[tran,],y[tran],alpha=1,lambda=lambda,family='gaussian')
  for(i in 1:lambda.num){
    if(sum(fit.q4$beta[,i]!=0)!=0){
      gene.pos <- which(fit.q4$beta[,i]!=0)
      q <- length(gene.pos)
      q0 <- min(q,length(tran)-1)
      lm.fit <- lm(y~x[,gene.pos[1:q0]],subset=tran)
      lm.pred <- predict(lm.fit,data.frame(x[,gene.pos[1:q0]]))
      pe.new[k,i] <- sqrt(mean((y-lm.pred)[test]^2))
    }else{
      beta0 <- mean(y[tran])
      pe.new[k,i] <- sqrt(mean((y[test]-beta0)^2))
    }
  }
}
pe.new.ave <- apply(pe.new,2,mean)
lambda.min <- lambda[which(pe.new.ave==min(pe.new.ave))]
pe.new.sd <- apply(pe.new,2,sd)
pe.new.up <- pe.new.ave+pe.new.sd
lambda.1se <- lambda[min(which(pe.new.ave <
                                 pe.new.up[which(pe.new.ave==min(pe.new.ave))]))]
fit.min <- glmnet(x,y,alpha=1,lambda=lambda.min,family='gaussian')
num.min <- sum(fit.min$beta!=0)
fit.1se <- glmnet(x,y,alpha=1,lambda=lambda.1se,family='gaussian')
num.1se <- sum(fit.1se$beta!=0)

lambda.min
num.min
lambda.1se
num.1se




###Q5
pe.new <- array(NA,c(length(alpha),lambda.num,K))
for(k in 1:K){
  tran <- which(foldid!=k)
  test <- which(foldid==k)
  for(i in 1:length(alpha)){
    fit.q5 <- glmnet(x[tran,],y[tran],alpha=alpha[i],lambda=lambda,family='gaussian')
    for(j in 1:lambda.num){
      if(sum(fit.q5$beta[,j]!=0)!=0){
        gene.pos <- which(fit.q5$beta[,j]!=0)
        q <- length(gene.pos)
        q0 <- min(q,length(tran)-1)
        lm.fit <- lm(y~x[,gene.pos[1:q0]],subset=tran)
        lm.pred <- predict(lm.fit,data.frame(x[,gene.pos[1:q0]]))
        pe.new[i,j,k] <- sqrt(mean((y-lm.pred)[test]^2))
      }else{
        beta0 <- mean(y[tran])
        pe.new[i,j,k] <- sqrt(mean((y[test]-beta0)^2))
      }
    }
  }
}
cv.ave <- apply(pe.new,c(1,2),mean)
cv.alpha <- apply(cv.ave,1,min)
alpha.hat <- alpha[which(cv.alpha==min(cv.alpha))]

cv.lambda <- pe.new[which(cv.alpha==min(cv.alpha)),,]
cv.lambda.ave <- apply(cv.lambda,1,mean)
cv.lambda.se <- apply(cv.lambda,1,sd)
cv.lambda.up <- cv.lambda.ave + cv.lambda.se

lambda.min.pos <- which(cv.lambda.ave==min(cv.lambda.ave))
lambda.min <- lambda[lambda.min.pos]

lambda.1se.pos <- min(which(cv.lambda.ave <
                              cv.lambda.up[which(cv.lambda.ave==min(cv.lambda.ave))]))

lambda.1se <- lambda[lambda.1se.pos]

fit.min <- glmnet(x,y,alpha=alpha.hat,lambda=lambda.min,family='gaussian')
num.min <- sum(fit.min$beta!=0)

fit.1se <- glmnet(x,y,alpha=alpha.hat,lambda=lambda.1se,family='gaussian')
num.1se <- sum(fit.1se$beta!=0)

alpha.hat
lambda.min
lambda.1se
num.min
num.1se














