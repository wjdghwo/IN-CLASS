## Open the dataset linked to the book website
Advertising <- read.csv("http://faculty.marshall.usc.edu/
gareth-james/ISL/Advertising.csv", h=T)
attach(Advertising)
# attach : 데이터를 R 검색 경로에 추가하여 변수명으로 바로 접근할 수 있게 한다.

## Least square fit for simple linear regression
par(mfrow = c(1,3))
plot(sales~TV, col=2, xlab="TV", ylab="Sales")
abline(lm(sales~TV)$coef, lwd=3, col="darkblue")
plot(sales~radio, col=2, xlab="Radio", ylab="Sales")
abline(lm(sales~radio)$coef, lwd=3, col="darkblue")
plot(sales~newspaper, col=2, xlab="Newspaper", ylab="Sales")
abline(lm(sales~newspaper)$coef, lwd=3, col="darkblue")






#income data
Income <- read.csv("http://faculty.marshall.usc.edu/
gareth-james/ISL//Income1.csv", h=T)
## Polynomial regression fit
par(mfrow = c(1,2))
plot(Income~Education, col=2, pch=19, xlab="Years of Education",
     ylab="Income", data=Income)
g <- lm(Income ~ poly(Education, 3), data=Income)
plot(Income~Education, col=2, pch=19, xlab="Years of Education",
     ylab="Income", data=Income)
lines(Income$Education, g$fit, col="darkblue", lwd=4, ylab="Income",
      xlab="Years of Education")

# k를 증가시켜서 그래프 확인
par(mfrow=c(3,4))
for (k in 1:12) {
  g <- lm(Income ~ poly(Education, k), data=Income)
  plot(Income~Education, col=2, pch=19, xlab="Years of Education",
       ylab="Income", data=Income, main=paste("k =", k))
  lines(Income$Education,g$fit,col="darkblue",lwd=3,ylab="Income",
        xlab="Years of Education")
}








set.seed(12345)
## Simulate x and y based on a known function
fun <- function(x) - (x-100)*(x-30)*(x+15)/13^4+6 # true model
x <- runif(50,0,100) # random x값 생성
y <- fun(x) + rnorm(50) # rnorm으로 error term 생성
## Plot linear regression and splines
plot(x, y, xlab="X", ylab="Y", ylim=c(1,13))
lines(sort(x), fun(sort(x)), col=1, lwd=2) # true model 그래프 
abline(lm(y~x)$coef, col="orange", lwd=2) # 단순회귀 모델 적용
lines(smooth.spline(x,y, df=5), col="blue", lwd=2) # df=5인 그래프 적용
lines(smooth.spline(x,y, df=23), col="green", lwd=2) # df=23인그래프 적용
legend("topleft", lty=1, col=c(1, "orange", "blue", "green"),
       legend=c("True", "df = 1", "df = 5", "df =23"),lwd=2)









set.seed(45678)
## Simulate training and test data (x, y)
fun <- function(x) - (x-100)*(x-30)*(x+15)/13^4+6
tran.x <- runif(50,0,100) # train set 랜덤 생성
test.x <- runif(50,0,100) # test set 랜덤 생성
error <- rnorm(50)
tran.y <- fun(tran.x) + error
test.y <- fun(test.x) + error
cbind(tran.x, tran.y)
cbind(test.x, test.y)

# training set과 test set생성 validation set은 생성 x
## Compute MSE along with different df
df <- 2:40 # df 2부터 40까지 생성
MSE <- matrix(0, length(df), 2) # MSE가 들어갈 행렬 공간 생성
for (i in 1:length(df)) {
  tran.fit <- smooth.spline(tran.x, tran.y, df=df[i]) # 2부터 40까지 smooth 그래프 생성
  MSE[i,1] <- mean((tran.y - predict(tran.fit, tran.x)$y)^2) # train MSE
  MSE[i,2] <- mean((test.y - predict(tran.fit, test.x)$y)^2) # test MSE
}

MSE # 1번째 열에는 train MSE 2번째 열에는 test MSE 생성









## Plot both test and training errors
matplot(df, MSE, type="l", col=c("gray", "red"), xlab="Flexibility",
        ylab="Mean Squared Error", lwd=2, lty=1, ylim=c(0,4)) #2가지의 다른 그래프 생성
abline(h=mean(error^2), lty=2)
legend("top", lty=1, col=c("red", "gray"),lwd=2,
       legend=c("Test MSE", "Training MSE"))
abline(v=df[which.min(MSE[,1])], lty=3, col="gray")
abline(v=df[which.min(MSE[,2])], lty=3, col="red")

# Training MSE는 df가 높을수록 값이 낮지만 Test MSE는 값이 높았다.-> 과적합문제

set.seed(12345)
## Simulate x and y based on a known function
fun <- function(x) x/10 +2
x <- runif(50,0,100)
y <- fun(x) + rnorm(50)
## Plot linear regression and splines
plot(x, y, xlab="X", ylab="Y", ylim=c(1,13))
lines(sort(x), fun(sort(x)), col=1, lwd=2)
abline(lm(y~x)$coef, col="orange", lwd=2)
lines(smooth.spline(x,y, df=5), col="blue", lwd=2)
lines(smooth.spline(x,y, df=23), col="green", lwd=2)
legend("topleft", lty=1, col=c(1, "orange", "blue", "green"),
       legend=c("True", "df = 1", "df = 5", "df =23"),lwd=2)


set.seed(45678)
## Simulate training and test data (x, y)
fun <- function(x) x/10 +2 # true model 변경
tran.x <- runif(50,0,100)
test.x <- runif(50,0,100)
error <- rnorm(50)
tran.y <- fun(tran.x) + error
test.y <- fun(test.x) + error
## Compute MSE along with different df
df <- 2:40
MSE <- matrix(0, length(df), 2)
for (i in 1:length(df)) {
  tran.fit <- smooth.spline(tran.x, tran.y, df=df[i])
  MSE[i,1] <- mean((tran.y - predict(tran.fit, tran.x)$y)^2)
  MSE[i,2] <- mean((test.y - predict(tran.fit, test.x)$y)^2)
}
MSE

## Plot both test and training errors
matplot(df, MSE, type="l", col=c("gray", "red"), xlab="Flexibility",
        ylab="Mean Squared Error", lwd=2, lty=1)
abline(h=mean(error^2), lty=2)
legend("top", lty=1, col=c("red", "gray"),lwd=2,
       legend=c("Test MSE", "Training MSE"))
abline(v=df[which.min(MSE[,1])], lty=3, col="gray")
abline(v=df[which.min(MSE[,2])], lty=3, col="red")
# df=1 일때 Test MSE가 가장 낮았다.



set.seed(12345)
## Simulate x and y based on a known function
fun <- function(x) -(x-80)*(x-45)*(x-25)/15^3+10
x <- runif(50,0,100)
y <- fun(x) + rnorm(50)
## Plot linear regression and splines
plot(x, y, xlab="X", ylab="Y")
lines(sort(x), fun(sort(x)), col=1, lwd=2)
abline(lm(y~x)$coef, col="orange", lwd=2)
lines(smooth.spline(x,y, df=5), col="blue", lwd=2)
lines(smooth.spline(x,y, df=23), col="green", lwd=2)
legend("topright", lty=1, col=c(1, "orange", "blue", "green"),
       legend=c("True", "df = 1", "df = 5", "df =23"),lwd=2)


set.seed(45678)
## Simulate training and test data (x, y)
fun <- function(x) -(x-80)*(x-45)*(x-25)/15^3+10
tran.x <- runif(50,0,100)
test.x <- runif(50,0,100)
error <- rnorm(50)
tran.y <- fun(tran.x) + error
test.y <- fun(test.x) + error
## Compute MSE along with different df
df <- 2:40
MSE <- matrix(0, length(df), 2)
for (i in 1:length(df)) {
  tran.fit <- smooth.spline(tran.x, tran.y, df=df[i])
  MSE[i,1] <- mean((tran.y - predict(tran.fit, tran.x)$y)^2)
  MSE[i,2] <- mean((test.y - predict(tran.fit, test.x)$y)^2)
}



## Plot both test and training errors
matplot(df, MSE, type="l", col=c("gray", "red"), xlab="Flexibility",
        ylab="Mean Squared Error", lwd=2, lty=1)
abline(h=mean(error^2), lty=2)
legend("top", lty=1, col=c("red", "gray"),lwd=2,
       legend=c("Test MSE", "Training MSE"))
abline(v=df[which.min(MSE[,1])], lty=3, col="gray")
abline(v=df[which.min(MSE[,2])], lty=3, col="red")

install.packages("ISLR")
library(ISLR)
data(Auto)
str(Auto)
summary(Auto)

mpg <- Auto$mpg
horsepower <- Auto$horsepower

dg <- 1:9
u <- order(horsepower)
par(mfrow=c(3,3))
for (k in 1:length(dg)) {
  g <- lm(mpg ~ poly(horsepower, dg[k]))
  plot(mpg~horsepower, col=2, pch=20, xlab="Horsepower",
       ylab="mpg", main=paste("dg =", dg[k]))
  lines(horsepower[u], g$fit[u], col="darkblue", lwd=3)
}



set.seed(1)
n <- nrow(Auto)

## training set
tran <- sample(n, n/2)

MSE <- NULL
for (k in 1:length(dg)) {
  g <- lm(mpg ~ poly(horsepower, dg[k]), subset=tran)
  MSE[k] <- mean((mpg - predict(g, Auto))[-tran]^2) # train set을 mse계산에 포함시기지 않음
}

par(mfrow=c(1,3))
plot(dg, MSE, type="b", col=2, xlab="Degree of Polynomial",
     ylab="Mean Squared Error", ylim=c(15,30), lwd=2, pch=19)
# 2부터 MSE가 크게 다르지 않으므로 제일 간단한 모델 선택




K <- 10
MSE <- matrix(0, length(dg), K)

for (i in 1:K) {
  tran <- sample(392, 196)
  for (k in 1:length(dg)) {
    g <- lm(mpg ~ poly(horsepower, dg[k]), subset=tran)
    MSE[k, i] <- mean((mpg - predict(g, Auto))[-tran]^2)
  }
}
matplot(dg, MSE, type="l", xlab="Degree of Polynomial", lty=1,
        ylab="Mean Squared Error", col=1:10, ylim=c(15,30))

avg <- apply(MSE, 1, mean)
plot(dg, avg, type="b", col=2, xlab="Degree of Polynomial",
     ylab="Mean Squared Error", ylim=c(15,30), lwd=2, pch=19)

# 각 모델들의 k값 변화에 따른 평균을 그래프로 그림
#역시 2부터 MSE의 크게 다르지 않으므로 제일 간단한 2를 선택





#LOOCV
n <- nrow(Auto)
dg <- 1:9
MSE <- matrix(0, n, length(dg))
for (i in 1:n) {
  for (k in 1:length(dg)) {
    g <- lm(mpg ~ poly(horsepower, k), subset=(1:n)[-i])
    MSE[i, k] <- mean((mpg - predict(g, Auto))[i]^2)
  }
}
aMSE <- apply(MSE, 2, mean) # MSE평균
plot(dg, aMSE, type="b", col="darkblue", xlab="Degree of Polynomial",
     ylab="Mean Squared Error", ylim=c(15,29), lwd=2, pch=19)


ncv <- NULL
for (k in 1:length(dg)) {
  g <- lm(mpg ~ poly(horsepower, k))
  ncv[k] <- mean((g$res/(1-influence(g)$hat))^2)
}
lines(dg, ncv, col=2, lty=2, lwd=2)







# k-fold CV
N <- 9 ## number of simulation replications
K <- 10 ## 10-fold cross validation
KCV <- matrix(0, length(dg), N)


set.seed(54321)
for (j in 1:N) {
  u <- sample(rep(seq(K), length=n))
  for (i in 1:length(dg)) {
    MSE <- NULL
    for (k in 1:K) {
      tran <- which(u!=k)
      g <- lm(mpg ~ poly(horsepower, i), subset=tran)
      MSE[k] <- mean((mpg - predict(g, Auto))[-tran]^2)
    }
    KCV[i, j] <- mean(MSE)
  }
}


matplot(dg, KCV, type="l", xlab="Degree of Polynomial", lty=1,
        ylab="Mean Squared Error", ylim=c(15,29), main="10-fold CV")








library(boot)
set.seed(101010)

## Leave-one-out CV
MSE <- NULL
for (i in 1:length(dg)) {
  glm.fit <- glm(mpg ~ poly(horsepower ,i))
  MSE[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
plot(dg, MSE, type="b", col="darkblue", xlab="Degree of Polynomial",
     ylab="Mean Squared Error", ylim=c(15,29), lwd=2, pch=19)

## K-fold cross validation
K <- 10
KCV <- NULL
for (i in 1:length(dg)) {
  glm.fit <- glm(mpg ~ poly(horsepower ,i))
  KCV[i] <- cv.glm(Auto, glm.fit, K=K)$delta[1]
}
lines(dg, KCV, col=2, lwd=2, type="b", pch=19, lty=2)