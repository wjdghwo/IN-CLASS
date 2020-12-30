x <- matrix(rnorm(20*30),20,30)
y<-rnorm(20)
g <- lm(y~x[,1:18])
summary(g)
#target
solve(t(x)%*%x)
det(t(x)%*%x)
#det가 거의 0

round(eigen(t(x)%*%x)$val,3)

#eigenvalue도 거의 0








set.seed(123)
n <- 100
pp <- c(10, 50, 80, 95, 97, 98, 99)
B <- matrix(0, 100, length(pp))
for (i in 1:100) {
  for (j in 1:length(pp)) {
    beta <- rep(0, pp[j])
    beta[1] <- 1
    x <- matrix(rnorm(n*pp[j]), n, pp[j])
    y <- x %*% beta + rnorm(n)
    g <- lm(y~x)
    B[i,j] <- g$coef[2] # beta2에 대한 값만 살펴봄
  }
}
boxplot(B, col="orange", boxwex=0.6, ylab="Coefficient estimates",
        names=pp, xlab="The number of predictors", ylim=c(-5,5))
abline(h=1, col=2, lty=2, lwd=2)
apply(B, 2, mean)
apply(B, 2, var)
colnames(B) <- pp
#값이 증가할수록 var증가









Credit <- read.csv("http://faculty.marshall.usc.edu/
gareth-james/ISL/Credit.csv", h=T)
Credit <- Credit[,-1]
str(Credit)
model.matrix(Balance ~., data=Credit) # 문자형 변수 dummy변수로 변경

# install.packages("leaps")
library(leaps)
g <- regsubsets(Balance ~., data=Credit) # 모든 가능한 모델과 단계적 알고리즘을 사용해 모델을 선택한다.
summary(g)
g.big <- regsubsets(Balance ~., data=Credit, nbest=1000, nvmax=11, 
                    really.big=T)
# nbest:기록할 각각의 크기의 하위 집합/ nvmax : 검사 할 하위 집합의 최대 크기
sg <- summary(g.big)
names(sg)
dim(sg$which)
sg$which[1:11,]
sg.size <- as.numeric(rownames(sg$which))
table(sg.size)
names(sg)
# "which"  "rsq":r^2  "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   
length(sg$rss)
summary(sg$rss)
sg.cp <- tapply(sg$cp, sg.size, min)
sg.cp
sg.bic <- tapply(sg$bic, sg.size, min)
sg.bic
sg.adjr2 <- tapply(sg$adjr2, sg.size, min)
sg.adjr2


par(mfrow=c(1,2))
## Residual Sum of Squares
sg.rss <- tapply(sg$rss, sg.size, min) # 어디에서 최솟값을 가지고 있니??
tapply(sg$rss, sg.size, max)
plot(1:11, sg.rss, type="b", xlab ="Number of Predictors", ylab=
       "Residual Sum of Squares", col=2, pch=19,
     ylim=c(min(sg$rss),max(sg$rss)))
points(sg.size, sg$rss, pch=20, col="gray", cex=0.8) # 회색점은 모든 가능한 점을 기입
## R square
sg.rsq <- tapply(sg$rsq, sg.size, max)
plot(1:11, sg.rsq, type="b", xlab ="Number of Predictors", ylab=
       expression(R^2), col=2, pch=19, ylim=c(0,1))
points(sg.size, sg$rsq, pch=20, col="gray", cex=0.8)







g.full <- regsubsets(Balance ~., data=Credit)
g.forw <- regsubsets(Balance ~., data=Credit, method="forward")
g.back <- regsubsets(Balance ~., data=Credit, method="backward")
full <- summary(g.full)$which[,-1]
full[full==TRUE] <- 1
forw <- summary(g.forw)$which[,-1]
forw[forw==TRUE] <- 1
back <- summary(g.back)$which[,-1]
back[back==TRUE] <- 1
full
coef(g.full, 1:8)
forw
coef(g.forw, 1:8)
back
coef(g.back, 1:8)

fun <- function(x,y,z,j) cbind(full=x[j,],forw=y[j,],back=z[j,])
fun(full, forw, back,1)
# 전체 모델에서는 rating변수를 선택 하지만 back은 limit을 선택 
fun(full, forw, back,2)
fun(full, forw, back,3)
fun(full, forw, back,4)

# 전반적으로 선택변수가 작을땐 전진선택법이 후진은 선택변수가 클때로 각 방법에서 놓치는 모델이 생긴다.
fun(full, forw, back,5) # 다같은 경우


par(mfrow=c(1,3))
## Cp statistic
sg.cp <- tapply(sg$cp, sg.size, min)
plot(1:11, sg.cp, type="b", xlab ="Number of Predictors", ylab=
       expression(C[p]), col=2, pch=19, ylim=c(min(sg$cp),max(sg$cp)))
points(sg.size, sg$cp, pch=20, col="gray", cex=0.8)
## Bayesian information criterion
sg.bic <- tapply(sg$bic, sg.size, min)
plot(1:11, sg.bic, type="b", xlab ="Number of Predictors", ylab=
       "Bayesian information criterion", col=2, pch=19,
     ylim=c(min(sg$bic),max(sg$bic)))
points(sg.size, sg$bic, pch=20, col="gray", cex=0.8)
## Adjusted R square
sg.adjr2 <- tapply(sg$adjr2, sg.size, max)
plot(1:11, sg.adjr2, type="b", xlab ="Number of Predictors", ylab=
       expression(paste("Adjusted ", R^2)), col=2, pch=19,
     ylim=c(min(sg$adjr2),max(sg$adjr2)))
points(sg.size, sg$adjr2, pch=20, col="gray", cex=0.8)








names(sg)
model1 <- coef(g.big, which.max(sg$rsq))
model2 <- coef(g.big, which.min(sg$rss))
model3 <- coef(g.big, which.max(sg$adjr2))
model4 <- coef(g.big, which.min(sg$cp))
model5 <- coef(g.big, which.min(sg$bic))
RES <- matrix(0, 12, 6)
gfull <- lm(Balance ~., data=Credit)
RES[,1] <- gfull$coef
full.names <- names(gfull$coef)
for (i in 1:5) {
  model <- get(paste("model", i, sep=""))
  w <- match(names(model), full.names)
  RES[w, (i+1)] <- model
}
rownames(RES) <- full.names
colnames(RES) <- c("full", "rsq", "rss", "adjr2", "cp", "bic")
RES



set.seed(404040)
train <- sample(c(TRUE,FALSE),nrow(Credit),rep=TRUE ,prob=c(0.75,0.25))
test <- (!train)
g.best <- regsubsets(Balance ~., data=Credit[train, ], nvmax=11)
test.mat <- model.matrix(Balance ~. , data=Credit[test, ])
val.errors <- rep(NA, 11)
for (i in 1:11) {
  coef <- coef(g.best, i)
  pred <- test.mat[, names(coef)] %*% coef
  val.errors[i] <- mean((Credit$Balance[test] - pred)^2)
}
par(mfrow=c(1,2))
sq <- sqrt(val.errors)
sq
plot(1:11, sq, type="l", col="red", xlab="Number of Predictors",
     ylab="Validation Set Error")
points(1:11, sq, pch=19, col="blue")
points(which.min(sq), sq[which.min(sq)], pch="x", col="blue", cex=2)





## Define new 'predict' function on regsubset
predict.regsubsets <- function(object ,newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  mat[ ,names(coefi)]%*%coefi
}


set.seed(313131)
K <- 10
folds <- sample(1:K, nrow(Credit), replace=TRUE)
cv.errors <- matrix(NA , K, 11)
colnames(cv.errors) <- paste(1:11)
for (i in 1:K) {
  fit <- regsubsets(Balance ~ ., data=Credit[folds!=i, ], nvmax=11)
  for (j in 1:11) {
    pred <- predict(fit, Credit[folds==i, ], id = j)
    cv.errors[i, j] <- mean((Credit$Balance[folds==i] - pred)^2)
  }
}

sq <- apply(sqrt(cv.errors), 2, mean)
sq
plot(1:11, sq, type="l", col="red", xlab="Number of Predictors",
     ylab="Cross-Validation Error")
points(1:11, sq, pch=19, col="blue")
points(which.min(sq), sq[which.min(sq)], pch="x", col="blue", cex=2)





## One-standard-error rule
se <- apply(sqrt(cv.errors), 2, sd)
avg <- apply(sqrt(cv.errors), 2, mean)
MSE <- cbind(avg - se, avg, avg + se) # boundary
matplot(1:11, MSE, type="l", col=c(1,2,1), lty=c(3,1,3),
        xlab="Number of Predictors", ylab="Cross-Validation Error",
        ylim=c(min(MSE), max(MSE)))
points(1:11, MSE[,2], pch=19, col="blue")
up <- which(MSE[,2] < MSE[which.min(MSE[,2]),3]) # 최솟값 MSE 범위에 있는 모델
up
points(min(up), MSE[min(up),2], pch="x", col="blue", cex=2)








library(glmnet)
str(Credit)
x <- model.matrix(Balance ~., Credit)[, -1] # yes no같은 값들을 수치형으로 변경
head(x)
y <- Credit$Balance
grid <- 10^seq(5, -2, length=100)
round(grid,5) #lamda 값 지정

g <- glmnet(x, y, alpha=0, lambda=grid) # alpha = 0 : ridge/alpha = 1 : lasso / 0 < alpha < 1
# 나중엔 x에 train, test데이터를 넣음
par(mfrow=c(1,2))
plot(g, "lambda", label=TRUE)
plot(g, "norm", label=TRUE)
# 앞서 봤던 그래프와 다름 limit변수가 너무 커서 이런 결과가 나옴 그래서 스케일을 해야함

fun <- function(x) sqrt(var(x)*(length(x)-1)/length(x))
sdx <- matrix(apply(x, 2, fun), dim(x)[2], dim(x)[1])
nx <- x/t(sdx) # scale한 new data
head(nx)

gn <- glmnet(nx, y, alpha=0, lambda=grid)
par(mfrow=c(1,2))
plot(gn, "lambda", label=TRUE)
plot(gn, "norm", label=TRUE)
# 그림의 11은 변수 갯수
dim(g$beta) # 11개 변수 갯수, 100은 람다 갯수



set.seed(1234)
K <- 100; p <- 45; n <- 50
beta <- runif(p, -1, 1)
lam <- 10^seq(5, -3, -0.1)
RES <- array(0, c(n, length(lam), K))
x <- matrix(rnorm(n * p), n, p)
y <- x %*% beta + rnorm(n, 0, 0.5)
for (i in 1:K) {
  gr <- sample(rep(seq(5), length=length(y)))
  yhat <- array(0, c(n, length(lam)))
  for (j in 1:5) {
    tran <- gr!=j
    g <- glmnet(x[tran,], y[tran], alpha=0, lambda=lam)
    wh <- which(gr==j)
    xbeta <- x[wh,] %*% g$beta
    yhat[wh, ] <- as.matrix(xbeta)
  }
  RES[,,i] <- yhat
}





MSE0 <- Bias0 <- Vars0 <- matrix(0, n, length(lam))
for (k in 1:length(lam)) {
  PE <- (RES[,k,] - matrix(y, n, K))^2
  MSE0[,k] <- apply(PE, 1, mean)
  Bias0[,k] <- abs(apply(RES[,k,], 1, mean) - y)
  Vars0[,k] <- apply(RES[,k,], 1, var)
}
MSE <- apply(MSE0, 2, mean)
Bias <- apply(Bias0, 2, mean)
Vars <- apply(Vars0, 2, mean)
MAT <- apply(cbind(Bias^2, Vars, MSE), 2, rev)
rlam <- rev(lam)
matplot(MAT, type="l", col=c(1,3,2), lty=1, xlab=expression(lambda),
        xaxt="n", ylab="Mean Squared Error")
legend("topleft", c("Bias", "Variance", "MSE"), col=c(1,3,2), lty=1)
w <- which.min(MAT[,3])
points(w, MAT[w, 3], pch=4, col="red", cex=2)
cc <- c(1,21,41,61,81)
axis(1, at=cc, labels=round(rlam[cc],1))






x <- model.matrix(Balance ~., Credit)[, -1]
y <- Credit$Balance
grid <- 10^seq(5, -2, length=100)

g <- glmnet(x, y, alpha=1, lambda=grid)
par(mfrow=c(1,2))
plot(g, "lambda", label=TRUE)
plot(g, "norm", label=TRUE)

fun <- function(x) sqrt(var(x)*(length(x)-1)/length(x)) # scale
sdx <- matrix(apply(x, 2, fun), dim(x)[2], dim(x)[1])
nx <- x/t(sdx)

gn <- glmnet(nx, y, alpha=1, lambda=grid)
par(mfrow=c(1,2))
plot(gn, "lambda", label=TRUE)
plot(gn, "norm", label=TRUE)

g$df # 선택된 변수갯수가 다름

g$beta[,40:50] # 람다가 작아질수록 제거되는 변수가 작아짐
round(g$lambda,3)





set.seed(1234)
K <- 100; p <- 45; n <- 50
beta <- runif(p, -1, 1)
lam <- 10^seq(1, -3, -0.05)

RES <- array(0, c(n, length(lam), K))
x <- matrix(rnorm(n * p), n, p)
y <- x %*% beta + rnorm(n, 0, 0.5)
for (i in 1:K) {
  gr <- sample(rep(seq(5), length=length(y))) # test, train set 분할
  yhat <- array(0, c(n, length(lam)))
  for (j in 1:5) {
    tran <- gr!=j
    g <- glmnet(x[tran,], y[tran], alpha=1, lambda=lam)
    wh <- which(gr==j) #test set
    xbeta <- x[wh,] %*% g$beta
    yhat[wh, ] <- as.matrix(xbeta)
  }
  RES[,,i] <- yhat
}





MSE <- Bias <- Vars <- matrix(0, n, length(lam))
for (k in 1:length(lam)) {
  PE <- (RES[,k,] - matrix(y, n, K))^2
  MSE[,k] <- apply(PE, 1, mean)
  Bias[,k] <- abs(apply(RES[,k,], 1, mean) - y)
  Vars[,k] <- apply(RES[,k,], 1, var)
}
MSE <- apply(MSE, 2, mean)
Bias <- apply(Bias, 2, mean)
Vars <- apply(Vars, 2, mean)


MAT <- apply(cbind(Bias^2, Vars, MSE), 2, rev)
rlam <- rev(lam)
matplot(MAT, type="l", col=c(1,3,2), lty=1, xlab=expression(lambda),
        xaxt="n", ylab="Mean Squared Error")
legend("topleft", c("Bias", "Variance", "MSE"), col=c(1,3,2), lty=1)
w <- which.min(MAT[,3])
points(w, MAT[w, 3], pch=4, col="red", cex=2)
cc <- c(1,21,41,61,81)
axis(1, at=cc, labels=rlam[cc])





# 라쏘와 릿지 비교
MSE.fun <- function(n, p, K, beta, lam, xtest, ytest) {
  yhat0 <- yhat1 <- array(0, c(n, length(lam), K))
  for (i in 1:K) {
    x <- matrix(rnorm(n * p), n, p)
    y <- x %*% beta + rnorm(n)
    g0 <- glmnet(x, y, alpha=0, lambda=lam) #alpha=0 릿지
    g1 <- glmnet(x, y, alpha=1, lambda=lam) #alpha=1 라쏘
    yhat0[1:n, 1:length(lam), i] <- predict(g0, x.test)
    yhat1[1:n, 1:length(lam), i] <- predict(g1, x.test)
  }
  MSE0 <- Bias0 <- Vars0 <- array(0, c(n,length(lam)))
  MSE1 <- Bias1 <- Vars1 <- array(0, c(n,length(lam)))
  for (j in 1:length(lam)) {
    PE0 <-(yhat0[,j,] - matrix(y.test, n, K))^2
    PE1 <-(yhat1[,j,] - matrix(y.test, n, K))^2
    MSE0[ ,j] <- apply(PE0, 1, mean)
    MSE1[ ,j] <- apply(PE1, 1, mean)
    BS0 <- abs(yhat0[,j,] - matrix(y.test, n, K))
    BS1 <- abs(yhat1[,j,] - matrix(y.test, n, K))
    
    ### The function "MSE.fun" continues
    Bias0[,j] <- apply(BS0, 1, mean)
    Bias1[,j] <- apply(BS1, 1, mean)
    Vars0[,j] <- apply(yhat0[,j,], 1, var)
    Vars1[,j] <- apply(yhat1[,j,], 1, var)
  }
  MSE.r <- apply(MSE0, 2, mean)
  MSE.l <- apply(MSE1, 2, mean)
  Bia.r <- apply(Bias0, 2, mean)
  Bia.l <- apply(Bias1, 2, mean)
  Var.r <- apply(Vars0, 2, mean)
  Var.l <- apply(Vars1, 2, mean)
  ridge <- apply(cbind(Bia.r^2, Var.r, MSE.r), 2, rev)
  lasso <- apply(cbind(Bia.l^2, Var.l, MSE.l), 2, rev)
  newlam <-rev(lam)
  list(ridge=ridge,lasso=lasso, lambda=newlam)
}






## The case that all predictors have non-zero coefficients
set.seed(111000)
K <- 100; p <- 120; n <- 100
beta <- runif(p, -1, 1)
lam <- 10^seq(3, -3, -0.05)
x.test <- matrix(rnorm(n * p), n, p)
y.test <- x.test %*% beta + rnorm(n)
g <- MSE.fun(n, p, K, beta, lam, xtest, ytest)
RES <- cbind(g$lasso, g$ridge)
matplot(RES, type="l", col=c(1,3,2), lty=rep(1:2,each=3),
        xlab=expression(lambda), xaxt="n",
        ylab="Mean Squared Error")
legend("topleft",c("Bias_Lasso", "Variance_Lasso", "MSE_Lasso",
                   "Bias_Ridge", "Variance_Ridge", "MSE_Ridge"), col=c(1,3,2),
       lty=rep(1:2,each=3))
cc <- c(1,21,41,61,81,101,121)
axis(1, at=cc, labels=g$lambda[cc])






## The case that only 5 predictors have non-zero coefficients
set.seed(111222)
K <- 100; p <- 120; n <- 100
beta <- runif(p, -1, 1)
beta[6:p] <- 0
lam <- 10^seq(3, -3, -0.05)
x.test <- matrix(rnorm(n * p), n, p)
y.test <- x.test %*% beta + rnorm(n)
g <- MSE.fun(n, p, K, beta, lam, xtest, ytest)
RES <- cbind(g$lasso, g$ridge)
matplot(RES, type="l", col=c(1,3,2), lty=rep(1:2,each=3),
        xlab=expression(lambda), xaxt="n",
        ylab="Mean Squared Error")
legend("topright",c("Bias_Lasso", "Variance_Lasso", "MSE_Lasso",
                    "Bias_Ridge", "Variance_Ridge", "MSE_Ridge"), col=c(1,3,2),
       lty=rep(1:2,each=3))
cc <- c(1,21,41,61,81,101,121)
axis(1, at=cc, labels=g$lambda[cc])












x <- model.matrix(Balance ~., Credit)[, -1]
y <- Credit$Balance
set.seed(12345)
g0 <- cv.glmnet(x, y, alpha=0, nfolds=10) # cv.glmnet은 위의 복잡한 과정을 모두 생략, 데이터 분할 람다값 지정 등등
g1 <- cv.glmnet(x, y, alpha=1, nfolds=10) # default값은 nfolds=10
par(mfrow=c(1,2))
plot(g0); plot(g1)
## Tuning parameter selection for ridge
## Minimum CV error
g0$lambda[which.min(g0$cvm)]
g0$lambda.min
## One-standard-error rule
wh <- min(which(g0$cvm < g0$cvup[which.min(g0$cvm)]))
g0$lambda[wh]
g0$lambda.1se # 1 standard error rule 적용했을때 가장 작은 값
g1$lambda #71개 뿐
g0$lambda
which.min(g1$cvm)
g1$lambda[71]



g1$cvm # 가장 작은 값이 좋음

names(g1)
# "lambda"     "cvm"        "cvsd"       "cvup"       "cvlo"       "nzero" 0아닌 변수들
# "call"       "name"       "glmnet.fit"
# "lambda.min" "lambda.1se"
#첫번째 라인 가장 작은 mse 두번째 라인 one standard error rule을 적용한 최적의 값


coef(g0, s="lambda.min") #각각 선택된 람다의 회귀 계수를 비교
coef(g0, s="lambda.1se")

## Tuning parameter selection for lasso
## Minimum CV error
g1$lambda[which.min(g1$cvm)] # 가장 작은 mse값의 람다값 출력
g1$lambda.min # 가장 작은 lambda값 출력
## One-standard-error rule
wh <- min(which(g1$cvm < g1$cvup[which.min(g1$cvm)]))
g1$lambda[wh]
g1$lambda.1se
coef(g1, s="lambda.min")
coef(g1, s="lambda.1se")







## Generate X matrix and Y vector
sim.fun <- function(n, p, beta, family=c("gaussian", "binomial")) {
  family <- match.arg(family)
  if (family=="gaussian") {
    x <- matrix(rnorm(n*p), n, p)
    y <- x %*% beta + rnorm(n)
  }
  else {
    x <- matrix(rnorm(n*p), n, p)
    xb <- x %*% beta
    z <- exp(xb) / (1+exp(xb))
    u <- runif(n)
    y <- rep(0, n)
    y[z > u] <- 1
  }
  list(x=x, y=y)
}





set.seed(1234)
n <- 200
p <- 2000
beta <- rep(0, p)
beta[1:20] <- runif(20, -1, 1) # 처음 20개만 값을 지정
sim <- sim.fun(n, p, beta)
x <- sim$x; y <- sim$y
## Fit the lasso with two different lambda values
g <- cv.glmnet(x, y, alpha=1, nfolds = 10)
bhat1 <- coef(g, s="lambda.min")
bhat2 <- coef(g, s="lambda.1se")
wh1 <- which(as.matrix(bhat1)!=0) # 어느 지점이 0이 아닌지
wh2 <- which(as.matrix(bhat2)!=0)




## Compute ordinary least square estimates (unbiased estimates)
bhat3 <- bhat4 <- bhat5 <- rep(0, p+1)
w1 <- wh1[-1]-1; w2 <- wh2[-1]-1
bhat3[wh1] <- lm(y ~ x[, w1])$coef # 0이 아닌 변수를 설정해서 계수출력
bhat4[wh2] <- lm(y ~ x[, w2])$coef
bhat5[1:21] <- lm(y ~ x[, 1:20])$coef # 오직 1:20의 변수만 지정
set.seed(56789)
## Generate test sets
test <- sim.fun(n, p, beta)
xt <- cbind(1, test$x)
yt <- test$y
## Test set prediction errors of 6 coefficient estimates
mean((yt - xt %*% bhat1)^2) # lasso_lambda.min (M1)
mean((yt - xt %*% bhat2)^2) # lasso_lambda.1se (M2)
mean((yt - xt %*% bhat3)^2) # least square_lambda.min (M3)
mean((yt - xt %*% bhat4)^2) # least square_lambda.1se (M4)
mean((yt - xt %*% bhat5)^2) # least square_nonzero beta (M5)
mean((yt - xt %*% c(0, beta))^2) # true beta (M6)





set.seed(1)
## Generate new test sets 100 times
K <- 100
pred <- matrix(NA, K, 6)
for (i in 1:K) {
  test <- sim.fun(n, p, beta)
  xt <- cbind(1, test$x)
  yt <- test$y
  pred[i, 1] <- mean((yt - xt %*% bhat1)^2)
  pred[i, 2] <- mean((yt - xt %*% bhat2)^2)
  pred[i, 3] <- mean((yt - xt %*% bhat3)^2)
  pred[i, 4] <- mean((yt - xt %*% bhat4)^2)
  pred[i, 5] <- mean((yt - xt %*% bhat5)^2)
  pred[i, 6] <- mean((yt - xt %*% c(0, beta))^2)
}
apply(pred, 2, mean)
boxplot(pred, col=c(2,2,4,4,3,"orange"), boxwex=0.6, names=c("M1",
                                                             "M2", "M3", "M4", "M5", "M6"), ylab="Prediction Error")



set.seed(111)
n <- 200
p <- 2000
beta <- rep(0, p)
beta[1:20] <- runif(20, -1, 1)
sim <- sim.fun(n, p, beta, family="binomial")
x <- sim$x; y <- sim$y
g <- cv.glmnet(x, y, alpha=1, nfolds = 10, family="binomial")
bhat1 <- coef(g, s="lambda.min")
bhat2 <- coef(g, s="lambda.1se")
wh1 <- which(as.matrix(bhat1)!=0)
wh2 <- which(as.matrix(bhat2)!=0)
bhat3 <- bhat4 <- bhat5 <- rep(0, p+1)
w1 <- wh1[-1]-1; w2 <- wh2[-1]-1
bhat3[wh1] <- glm(y ~ x[, w1], family="binomial")$coef
bhat4[wh2] <- glm(y ~ x[, w2], family="binomial")$coef
bhat5[1:21] <- glm(y ~ x[, 1:20], family="binomial")$coef






## Classification Error
class.fun <- function(test.x, test.y, beta, k=0.5) {
  xb <- test.x %*% beta
  exb <- exp(xb) / (1 + exp(xb))
  y <- rep(0, length(test.y))
  y[as.logical(exb > k)] <- 1
  min(mean(test.y!=y), mean(test.y!=(1-y))) # 오분류 확인
}
## Generate test sets
set.seed(56789)
test <- sim.fun(n, p, beta, family="binomial")
xt <- cbind(1, test$x); yt <- test$y
## Prediction error comparison
class.fun(xt, yt, bhat1) # lasso_lambda.min (M1)
class.fun(xt, yt, bhat2) # lasso_lambda.1se (M2)
class.fun(xt, yt, bhat3) # least square_lambda.min (M3)
class.fun(xt, yt, bhat4) # least square_lambda.1se (M4)
class.fun(xt, yt, bhat5) # least square_nonzero beta (M5)
class.fun(xt, yt, c(0, beta)) # true beta (M6)







set.seed(35791)
## Generate new test sets 100 times
K <- 100
pred <- matrix(NA, K, 6)
for (i in 1:K) {
  test <- sim.fun(n, p, beta, family="binomial")
  xt <- cbind(1, test$x)
  yt <- test$y
  pred[i, 1] <- class.fun(xt, yt, bhat1)
  pred[i, 2] <- class.fun(xt, yt, bhat2)
  pred[i, 3] <- class.fun(xt, yt, bhat3)
  pred[i, 4] <- class.fun(xt, yt, bhat4)
  pred[i, 5] <- class.fun(xt, yt, bhat5)
  pred[i, 6] <- class.fun(xt, yt, c(0, beta))
}
apply(pred, 2, mean)
boxplot(pred, col=c(2,2,4,4,3,"orange"), boxwex=0.6, names=c("M1",
                                                             "M2", "M3", "M4", "M5", "M6"), ylab="Prediction Error")



###############################################################

data(QuickStartExample)
dim(x)
dim(y)
alpha <- seq(0, 1, length=6)
par(mfrow=c(2,3))
for (i in 1:length(alpha)) {
  fit = glmnet(x, y, alpha=alpha[i])
  if (i==1) {
    beta <- coef(fit, s=0.1)
    pred <- predict(fit, x[1:10,], s=0.1) # 1-10행을 예측
  }
  else {
    beta <- cbind(beta, coef(fit, s=0.1))
    pred <- cbind(pred, predict(fit, x[1:10,], s=0.1))
  }
  plot(fit)
  title(paste("alpha = ", round(alpha[i], 2)), cex.main=1.2)
}
colnames(beta) <- colnames(pred) <- alpha
beta
pred
cbind(y[1:10],pred)



set.seed(101)
alpha <- seq(0, 1, 0.05)
foldid = sample(1:10, size=length(y), replace=TRUE) # 10-fold 각각 지정
table(foldid)
cvm <- NULL
for (i in 1:length(alpha)) {
  fit = cv.glmnet(x, y, alpha=alpha[i], foldid = foldid)
  cvm[i] <- min(fit$cvm)
}

cbind(alpha,cvm) #여기선 alpha=1일때 cvm이 가장 낮았다

a <- alpha[which.min(cvm)] #alpha=1일때로 설정정
cvfit <- cv.glmnet(x, y, alpha=a, foldid=foldid) # 위에서 지정한 10-fold 그룹 사용
plot(fit)
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = x[1:5,], s = "lambda.min")
cvfit = cv.glmnet(x, y, type.measure="mse", nfolds=10) # R에서 10-fold 사용
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = x[1:5,], s = "lambda.min")





set.seed(100)
foldid = sample(1:10, size=length(y), replace=TRUE)
cv1 = cv.glmnet(x, y, foldid = foldid, alpha = 1) # 라쏘
cv.5 = cv.glmnet(x, y, foldid = foldid, alpha = .5)
cv0 = cv.glmnet(x, y, foldid = foldid, alpha = 0) # 릿지
par(mfrow=c(1,3))
plot(cv1) # 라쏘
plot(cv.5)
plot(cv0) # 릿지
plot(log(cv1$lambda), cv1$cvm, pch=19, col="red", xlab="log(Lambda)",
     ylab=cv1$name)
points(log(cv.5$lambda), cv.5$cvm,pch=19,col="grey")
points(log(cv0$lambda), cv0$cvm,pch=19,col="blue")
legend("topleft", legend=c("alpha= 1", "alpha= .5", "alpha= 0"),
       pch=19, col=c("red", "grey", "blue"), cex=1.5)




####################################################################
data(BinomialExample)
dim(x)
y
fit = glmnet(x, y, family="binomial", alpha=0.1) #mix model
par(mfrow=c(1,2))
plot(fit, label=TRUE)
plot(fit, xvar="dev", label=TRUE)
predict(fit, newx=x[1:5,], type="class", s=c(0.05, 0.01)) # lamda 0.05와 0.01
# type="class"이면 연속이 아닌 0,1으로 이진값 예측

cvfit1 = cv.glmnet(x, y, family="binomial", type.measure="class") # not continuous
cvfit2 = cv.glmnet(x, y, family="binomial", type.measure="deviance")
par(mfrow=c(1,2))
plot(cvfit1);plot(cvfit2)
c(cvfit1$lambda.min, cvfit1$lambda.1se)
c(cvfit2$lambda.min, cvfit2$lambda.1se)
cbind(coef(cvfit1, s="lambda.min"), coef(cvfit2, s="lambda.min"))
predict(cvfit1, newx=x[1:10,], s="lambda.min")
predict(cvfit1, newx=x[1:10,], s="lambda.min", type="response")
predict(cvfit1, newx=x[1:10,], s="lambda.min", type="class")







## Install bioconductor package 'golubEsets'
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("golubEsets") # data set
library(golubEsets)
data(Golub_Merge)

## Golub data
?Golub_Merge
dim(Golub_Merge) # 유전자가 열이 아닌 행렬로 분석함
varLabels(Golub_Merge)

## Some features
Golub_Merge$ALL.AML #focus
Golub_Merge$Gender
Golub_Merge$BM.PB

## Expression data
dim(exprs(Golub_Merge))
head(exprs(Golub_Merge))
tail(exprs(Golub_Merge))




library(glmnet)
set.seed(1234)

## Transpose the predictor x into n by p matrix
x <- t(as.matrix(exprs(Golub_Merge)))
y <- Golub_Merge$ALL.AML
dim(x)
sum(is.na(x))
g <- cv.glmnet(x, y, alpha=1, family="binomial", K=5)
fit1 <- coef(g, s = "lambda.min")
fit2 <- coef(g, s = "lambda.1se")
c(sum(as.matrix(fit1)!=0), sum(as.matrix(fit2)!=0))
rownames(exprs(Golub_Merge))[as.matrix(fit1) != 0]
rownames(exprs(Golub_Merge))[as.matrix(fit2) != 0]
pt <- apply(x, 2, function(t) t.test(t[y=="ALL"], t[y=="AML"])$p.val)
adj.p <- p.adjust(pt, "bonferroni")
sum(adj.p < 0.05)






## Seperate training sets and test sets
Golub_Merge$Samples
tran <- Golub_Merge$Samples < 39
test <- !tran
c(sum(tran), sum(test))

set.seed(1234567)
g <- cv.glmnet(x, y, alpha=1, family="binomial", K=5, subset=tran)
fit1 <- coef(g, s = "lambda.min")
fit2 <- coef(g, s = "lambda.1se")
c(sum(as.matrix(fit1)!=0), sum(as.matrix(fit2)!=0))

p1 <- predict(g, x[test,], s = "lambda.min", type="class")
p2 <- predict(g, x[test,], s = "lambda.1se", type="class")
table(p1, y[test])
table(p2, y[test])






set.seed(123)
K <- 100
MC <- DF <- array(0, c(K, 2))
for (i in 1:K) {
  tran <- as.logical(rep(0, 72))
  tran[sample(1:72, 38)] <- TRUE
  test <- !tran
  g <- cv.glmnet(x, y, alpha=1, family="binomial", K=5, subset=tran)
  p1 <- predict(g, x[test,], s = "lambda.min", type="class")
  p2 <- predict(g, x[test,], s = "lambda.1se", type="class")
  DF[i, 1] <- sum(coef(g, s="lambda.min")!=0) # 자유도, 0이 아닌 변수개수
  DF[i, 2] <- sum(coef(g, s="lambda.1se")!=0)
  MC[i, 1] <- sum(p1!=y[test])/sum(test) #오분류율
  MC[i, 2] <- sum(p2!=y[test])/sum(test)
}
apply(MC, 2, mean)
MC #min을 적용했을때 오분류율이 0임
DF #얼마나 많은 유전자가 선택되었나?

apply(DF, 2, var)

