library(ISLR)
data(Default)
summary(Default)
attach(Default)

plot(income ~ balance, xlab="Balance", pch=c(1,3)[unclass(default)],
     col=c("lightblue","red")[unclass(default)], ylab="Income")

set.seed(1234)
ss <- sample(which(default=="No"), sum(default=="Yes"))
ss <- c(ss, which(default=="Yes"))
us <- unclass(default[ss])
plot(income[ss] ~ balance[ss], xlab="Balance", pch=c(1,3)[us],
     col=c("lightblue","red")[us], ylab="Income")

par(mfrow=c(1,2))
boxplot(balance~default, col=c("lightblue","red"), boxwex=0.5,
        xlab="Default", ylab="Balance")
boxplot(income~default, col=c("lightblue","red"), boxwex=0.5,
        xlab="Default", ylab="Income")









ndef <- rep(0, length(default))
ndef[default=="Yes"] <- 1
table(ndef)

g1 <- glm(ndef ~ balance) #simple linear regression 
g2 <- glm(default ~ balance, family="binomial") # link ft이 binomial인 logistic regression

par(mfrow=c(1,2))
plot(balance, ndef, pch="|", col="orange", xlab="Balance",
     ylab="Probability of Default",ylim=c(-0.1,1.1))
abline(h=c(0,1), lty=2)
lines(balance, g1$fit, col="lightblue", lwd=2)
plot(balance, as.numeric(default)-1, pch="|", col="orange",
     xlab="Balance", ylab="Probability of Default",
     ylim=c(-0.1,1.1))
abline(h=c(0,1), lty=2)
u <- order(balance)
lines(balance[u], g2$fit[u], col="lightblue", lwd=3)

range(g1$fit) # 범위가 0과 1사이 아님
round(range(g2$fit),4) # 범위가 거의 0과 1사이







g2 <- glm(default ~ balance, family="binomial")
summary(g2)
summary(g2)$coef

## Fitted values
g2$fit

## inverse logistic function
ilogit <- function(x, coef) {
  exp(cbind(1, x) %*% coef) / (1 + exp(cbind(1, x) %*% coef))
}
cbind(g2$fit, ilogit(balance, g2$coef))
ilogit(1000, g2$coef) # 로짓모형에 x가 1000일때의 확률 예측

g3 <- glm(default ~ student, family="binomial")
summary(g3)$coef

## Student "Yes"
ilogit(1, g3$coef) #4.314%
## Student "No"
ilogit(0, g3$coef) #2.920%


g4 <- glm(default~ balance + income + student, family="binomial")
round(summary(g4)$coef,4)
# 변수를 추ㅜ가해주니 student는 더이상 양의 값을 가지지 않았다.







yst <- g4$fit[student=="Yes"]
nst <- g4$fit[student=="No"]
plot(balance, g2$fit, col="white", xlab="Credit Card Balance",
     ylab="Default Rate")
abline(h=0)
abline(h=mean(yst), lty=2, col="orange")
abline(h=mean(nst), lty=2, col="lightblue")
u1 <- order(balance[student=="Yes"])
u2 <- order(balance[student=="No"])
lines(balance[student=="Yes"][u1], yst[u1], col="orange", lwd=2)
lines(balance[student=="No"][u2], nst[u2], col="lightblue", lwd=2)
boxplot(balance ~ student, col=c("lightblue","orange"), xlab="Student
Status", ylab="Credit Card Balance", boxwex=0.6)

ilogit(cbind(1500, 40, 1), g4$coef) #balance, income, student
ilogit(cbind(1500, 40, 0), g4$coef) #balance, income, nonstudent
xb <- predict(g4, data.frame(balance=1500, income=40, student="Yes")) # predict function을 사용하여 확률 구하기
xb
exp(xb)/(1+exp(xb))
predict(g4, data.frame(balance=1500, income=40, student="Yes"),
        type="response") # 바로 확률 구하기






  
set.seed(1111)
n <- nrow(Default)
train <- sample(1:n, n*0.7)
test <- setdiff(1:n, train)
g1 <- glm(default ~ balance, family="binomial", subset=train)
g2 <- glm(default ~ student, family="binomial", subset=train)
g3 <- glm(default ~ income, family="binomial", subset=train)
g4 <- glm(default~ balance+student+income, family="binomial",
          subset=train)
miss <- NULL
for (k in 1:4) {
  g <- get(paste("g", k, sep=""))
  pred <- predict(g, Default, type="response")[test]
  yhat <- rep(0, length(test))
  yhat[pred <= 0.5] <- 1
  miss[k] <- mean(yhat!=as.numeric(default[test]))
}
miss # 오분류율
#모든 변수를 선택하는 것 보다는 특정 변수를 선택하는 것이 오분류율이 더 높은 경우가 있었다.




library(rattle.data)
library(nnet)
data(wine)
str(wine)

set.seed(1111)
n <- nrow(wine)
train <- sample(1:n, round(n*0.7))
test <- setdiff(1:n, train)
fit <- multinom(Type ~ Alcohol + Color, data=wine, subset=train)
summary(fit)
wine$Type <- relevel(wine$Type, ref = "3")
fit2 <- multinom(Type ~ Alcohol + Color, data=wine, subset=train)
summary(fit2)

pred <- predict(fit2, wine, type="class")
tab <- table(pred[test], wine$Type[test])
tab
1-sum(diag(tab))/sum(tab) #오분류율

summary(wine$Alcohol)
tapply(wine$Alcohol, wine$Type, mean)
tapply(wine$Color, wine$Type, mean) # 첫번째 모델이 3>1>2





fit3 <- multinom(Type ~ ., data=wine, subset=train) #full model
pred <- predict(fit3, wine, type="class")
pred
tab <- table(pred[test], wine$Type[test])
tab
1-sum(diag(tab))/sum(tab) #full model이 변수를 하나마 가지는 것 보다 전체모델을 가지느 ㄴ것이 더 오분류율이 낮다.

set.seed(12345)
miss <- NULL
for (k in 1:100) {
  train <- sample(1:n, round(n*0.7))
  test <- setdiff(1:n, train)
  g <- multinom(Type ~ ., data=wine, subset=train, trace=FALSE)
  pred <- predict(g, wine, type="class")
  tab <- table(pred[test], wine$Type[test])
  miss[k] <- 1-sum(diag(tab))/sum(tab)
}
miss
summary(miss)








x <- seq(-5, 5, 0.1)
plot(x, dnorm(x, -1.5, 1), type="l", col="darkgreen", xlab="", # 첫번째는 평균이 -1.5
     ylab="", yaxt="n", xlim=c(-5,5), lwd=2)
lines(x, dnorm(x, 1.5, 1), col="red", lwd=2) # 두번째는 평균이 1.5
abline(v=0, lwd=3, lty=2)

x1 <- rnorm(20, -1.5, 1)
x2 <- rnorm(20, 1.5, 1)
hist(x1, col=rgb(0.4,1,0,0.8), breaks=10, xlab="", ylab="", main="",
     xlim=c(-4,4))
hist(x2, col=rgb(0.7,0,0,0.7), breaks=10, add=T)
abline(v=0, lwd=3, lty=2)
abline(v=(mean(x1)+mean(x2))/2, lwd=3, lty=1)
legend("topleft", c("True", "Estimated"), lty=c(2,1), lwd=1, bty="n",
       cex=0.9)








## Open the iris dataset
data(iris)
?iris
str(iris)
summary(iris)
plot(iris[, -5], col=as.numeric(iris$Species) + 1)

## Apply LDA for iris data
library(MASS)
g <- lda(Species ~., data=iris)
g
plot(g, col=as.numeric(iris$Species) + 1)
plot(g, dimen=1)



## Compute misclassification error for training sets
pred <- predict(g)
table(pred$class, iris$Species)
mean(pred$class!=iris$Species) # 오분류율









## Randomly separate training sets and test sets #test, train분리후 시행
set.seed(1234)
tran <- sample(nrow(iris), size=floor(nrow(iris)*2/3))
g <- lda(Species ~., data=iris, subset=tran)

## Compute misclassification error for test sets
pred <- predict(g, iris)$class[-tran]
test <- iris$Species[-tran]
table(pred, test)
mean(pred!=test)

## Posterior probability (사후확률)
post <- predict(g, iris)$posterior[-tran,]
round(post,4)
length(post); length(test);
apply(post, 1, which.max) # 확률이 높은 class로 분류
as.numeric(pred)
# 이 경우는 분류하기 쉬운 예제였다.







library("nnet")
set.seed(1234)
K <- 100
RES <- array(0, c(K, 2))
for (i in 1:K) {
  tran.num <- sample(nrow(iris), size=floor(nrow(iris)*2/3))
  tran <- as.logical(rep(0, nrow(iris)))
  tran[tran.num] <- TRUE
  g1 <- lda(Species ~., data=iris, subset=tran) # lda
  g2 <- multinom(Species ~., data=iris, subset=tran, trace=FALSE) # multinomial regression
  pred1 <- predict(g1, iris[!tran,])$class
  pred2 <- predict(g2, iris[!tran,])
  RES[i, 1] <- mean(pred1!=iris$Species[!tran])
  RES[i, 2] <- mean(pred2!=iris$Species[!tran])
}
RES # 분류체계마다 오분류율은 다름
apply(RES, 2, mean)
#평균을 비교했을때 LDA의 경우가 오분류가 낮았음 하지만 모든경우에서 LDA가 좋다는 뜻은 아님






library(ISLR)
data(Default)
attach(Default)
str(Default)

library(MASS)
g <- lda(default~., data=Default)
pred <- predict(g, default)
table(pred$class, default)
mean(pred$class!=default)
# LDA로 오분류 2.76%





thre <- seq(0,1,0.01)
res <- matrix(NA, length(thre), 3)

## Compute overall error, false positives, false negatives
for (i in 1:length(thre)) {
  decision <- rep("No", length(default)) # 모두 다 NO
  decision[pred$posterior[,2] >= thre[i]] <- "Yes" # 임계값 조정
  res[i, 1] <- mean(decision != default) # 오분류율
  res[i, 2] <- mean(decision[default=="No"]=="Yes") # FPR
  res[i, 3] <- mean(decision[default=="Yes"]=="No") # FNR
}
pred$class #result of prediction
round(pred$posterior,4)[1:10,] #확률
apply(pred$posterior, 1, which.max)
table(pred$class, default)

decision[pred$posterior[,2] >= 0.7] <- "Yes" # 임계값 0.7
table(decision, default)
decision[pred$posterior[,2] >= 0.2] <- "Yes"
table(decision, default)

k <- 1:51
matplot(thre[k], res[k,], col=c(1,"orange",4), lty=c(1,4,2), type="l",
        xlab="Threshold", ylab="Error Rate", lwd=2)
legend("top", c("Overall Error", "False Positive", "False Negative"),
       col=c(1,"orange",4), lty=c(1,4,2), cex=1.2)
apply(res, 2, which.min)
thre[apply(res, 2, which.min)]
which(res[,1]==min(res[,1]))

decision[pred$posterior[,2] >= 0.36] <- "Yes" # 오분류율 가장 낮을때
table(decision, default)








thre <- seq(0,1,0.01)
Sen <- Spe <- NULL
RES <- matrix(NA, length(thre), 4)
colnames(RES) <- c("TP", "TN", "FP", "FN")
for (i in 1:length(thre)) {
  decision <- rep("No", length(default))
  decision[pred$posterior[,2] >= thre[i]] <- "Yes"
  Sen[i] <- mean(decision[default=="Yes"] == "Yes") #TPR
  Spe[i] <- mean(decision[default=="No"] == "No") #TNR
  RES[i,1] <- sum(decision[default=="Yes"] == "Yes")#TP
  RES[i,2] <- sum(decision[default=="No"] == "No")#TN
  RES[i,3] <- sum(decision=="Yes") - RES[i,1]#FP
  RES[i,4] <- sum(default=="Yes") - RES[i,1]#FN
}
plot(1-Spe, Sen, col="darkblue", pch=19, xlab="False positive rate",
     ylab="True positive rate", main="ROC Curve")
plot(1-Spe, Sen, col="darkblue", pch=19, xlab="False positive rate",
     ylab="True positive rate", main="ROC Curve", type='l')

abline(0,1,lty=3, col="gray")
sensitivity <- RES[,1] / (RES[,1] + RES[,4])
specificity <- RES[,2] / (RES[,2] + RES[,3])
cbind(sensitivity,1-specificity)



library(ROCR)
## Compute ROC curve
label <- factor(default,levels=c("Yes","No"),labels=c("TRUE","FALSE"))
label
preds <- prediction(pred$posterior[,2], label) # ROC curve 계산
perf <- performance(preds, "tpr", "fpr") #ROC curve 함수
plot(perf, lwd=4, col="darkblue")
abline(a=0, b=1, lty=2)
slotNames(perf)
perf@x.name
perf@y.name
perf@alpha.name

## Compute AUC
performance(preds, "auc")@y.values # AUC값 구하기
?performance





library(MASS)
## Misclassification error rate of LDA
g1 <- lda(default~., data=Default);pred1 <- predict(g1, default)
table(pred1$class, default)/10000
mean(pred1$class!=default)
## Misclassification error rate of QDA
g2 <- qda(default~., data=Default);pred2 <- predict(g2, default)
table(pred2$class, default)/10000
mean(pred2$class!=default)
#정확도 관점에서는 QDA가 조금 더 좋은 값을 나타낸다.

## AUC comparison between LDA and QDA
library(ROCR)
label <- factor(default,levels=c("Yes","No"),labels=c("TRUE","FALSE"))
preds1 <- prediction(pred1$posterior[,2], label) #LDA
preds2 <- prediction(pred2$posterior[,2], label) #QDA
performance(preds1, "auc")@y.values
performance(preds2, "auc")@y.values
# AUC관점에서는 LDA가 조금 더 좋은 값을 나타낸다. 

perf1 <- performance(preds1, "tpr", "fpr") #ROC curve 함수
plot(perf1, lwd=4, col="darkblue")
perf2 <- performance(preds2, "tpr", "fpr") #ROC curve 함수
plot(perf2, lwd=4, col="darkblue")



data(iris)
library(e1071)
g1 <- naiveBayes(Species ~ ., data = iris)
g1 <- naiveBayes(iris[,-5], iris[,5])
pred <- predict(g1, iris[,-5])
table(pred, iris[,5])
mean(pred!=iris$Species)

## Randomly separate training sets and test sets
set.seed(1234)
tran <- sample(nrow(iris), size=floor(nrow(iris)*2/3))

## Compute misclassification error for test sets
g2 <- naiveBayes(Species ~ ., data = iris, subset=tran)
pred2 <- predict(g2, iris)[-tran]
test <- iris$Species[-tran]
table(pred2, test)
mean(pred2!=test)








library(ISLR)
data(Default)
attach(Default)

## Misclassification error rate of Naive Bayes
g <- naiveBayes(default ~ ., data = Default)
pred <- predict(g, Default)
table(pred, default)/10000
mean(pred!=default)

## AUC of Naive Bayes
library(ROCR)
label <- factor(default,levels=c("Yes","No"),labels=c("TRUE","FALSE"))
pred <- predict(g, Default, type="raw")
preds <- prediction(pred[,2], label)
performance(preds, "auc")@y.values
# 이 경우에서는 LDA, QDA가 NB보다는 더 좋은 방법이었다.






library(ISLR)
data(Caravan)
dim(Caravan)
str(Caravan)
attach(Caravan)

# only 6% of people purchased caravan insurance.
summary(Purchase)
mean(Purchase=="Yes") #unbalance data 

## Standardize the data so that all has a mean of 0 and variance of 1
X <- scale(Caravan[,-86]) # target 값 제외하고 scale함
apply(Caravan[,1:5], 2, var)
apply(X[,1:5], 2, var) #분산 1으로 스케일
apply(Caravan[,1:5], 2, mean)
apply(X[,1:5], 2, mean) #평균 거의 0으로 스케일

## Separate training sets and test sets
test=1:1000
train.X=X[-test, ]
test.X=X[test, ]
train.Y=Purchase[-test]
test.Y=Purchase[test]
dim(X)





library(class)
## misclassification error rate of KNN
set.seed(1)
# r이 구분을 랜덤하게 진행하므로 시드값 지정
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
# 모두가 no로 설정했을때 오분류율이 5.9%인데 k=1일때는 오분류율이 11.6% 매우 높다 따라서 K=1일때의 분류는 좋은 분류가 아님

knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
mean(test.Y!=knn.pred)

knn.pred=knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
mean(test.Y!=knn.pred)

knn.pred=knn(train.X, test.X, train.Y, k=10)
table(knn.pred, test.Y)
mean(test.Y!=knn.pred)






library(mnormt); library(class)
set.seed(1010)

sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
x.tran1 <- rmnorm(100, c(0, 0.8), sigma)
x.tran2 <- rmnorm(100, c(0.8, 0), sigma)
x.test1 <- rmnorm(3430, c(0, 0.8), sigma)
x.test2 <- rmnorm(3430, c(0.8 ,0), sigma)

x.tran <- rbind(x.tran1, x.tran2)
x.test <- rbind(x.test1, x.test2)
y.tran <- factor(rep(0:1, each=100))

mn <- min(x.tran)
mx <- max(x.tran)
px1 <- seq(mn, mx, length.out=70)
px2 <- seq(mn, mx, length.out=98)
gd <- expand.grid(x=px1, y=px2)






g1 <- knn(x.tran, gd, y.tran, k = 1, prob=TRUE)
g2 <- knn(x.tran, gd, y.tran, k = 10, prob=TRUE)
g3 <- knn(x.tran, gd, y.tran, k = 100, prob=TRUE)

par(mfrow=c(1,3))

prob1 <- attr(g1, "prob")
prob1 <- ifelse(g1=="1", prob1, 1-prob1)
pp1 <- matrix(prob1, length(px1), length(px2))
contour(px1, px2, pp1, levels=0.5, labels="", xlab="", ylab="",
        main="KNN: K=1", axes=FALSE)
points(x.tran, col=ifelse(y.tran==1, "cornflowerblue", "coral" ))
points(gd, pch=".", cex=1.2, col=ifelse(pp1>0.5, "cornflowerblue",
                                        "coral"))
box()







prob2 <- attr(g2, "prob")
prob2 <- ifelse(g2=="1", prob2, 1-prob2)
pp2 <- matrix(prob2, length(px1), length(px2))
contour(px1, px2, pp2, levels=0.5, labels="", xlab="", ylab="",
        main="KNN: K=10", axes=FALSE)
points(x.tran, col=ifelse(y.tran==1, "cornflowerblue", "coral" ))
points(gd, pch=".", cex=1.2, col=ifelse(pp2>0.5, "cornflowerblue",
                                        "coral"))
box()
prob3 <- attr(g3, "prob")
prob3 <- ifelse(g3=="1", prob3, 1-prob3)
pp3 <- matrix(prob3, length(px1), length(px2))
contour(px1, px2, pp3, levels=0.5, labels="", xlab="", ylab="",
        main="KNN: K=100", axes=FALSE)
points(x.tran, col=ifelse(y.tran==1, "cornflowerblue", "coral" ))
points(gd, pch=".", cex=1.2, col=ifelse(pp3>0.5, "cornflowerblue",
                                        "coral"))
box()







library(mnormt);library(MASS);library(class);library(e1071)
## LDA, QDA, KNN(K=1), KNN(K=5), KNN(K=20), logistic, Naive Bayes
MissClassRate <- function(x.tran, x.test, y.test, y.tran) {
  ldafit <- predict(lda(x.tran, y.tran), x.test)$class
  qdafit <- predict(qda(x.tran, y.tran), x.test)$class
  knn1 <- knn(x.tran, x.test, y.tran, k=1)
  knn5 <- knn(x.tran, x.test, y.tran, k=5)
  knn20 <- knn(x.tran, x.test, y.tran, k=20)
  data <- data.frame(x=rbind(x.tran,x.test),
                     y=unlist(list(y.tran,y.test)))
  g <- glm(y~., family="binomial", subset=1:nrow(x.tran), data)
  logit <- predict(g, data, type="response")[-c(1:nrow(x.tran))] # logit은 0,1으로 출력하므로 response으로 출력 변경
  logit[logit >= 0.5] <- 1; logit[logit < 0.5] <- 0
  g2 <- naiveBayes(y~., subset=1:nrow(x.tran), data)
  NB <- predict(g2, data)[-c(1:nrow(x.tran))]
  c(lda=mean(ldafit!=y.test), qda=mean(qdafit!=y.test), knn1=mean(knn1!=y.test),
    knn5=mean(knn5!=y.test), knn20=mean(knn20!=y.test), logit=mean(logit!=y.test),
    NB=mean(NB!=y.test))
}







set.seed(12345)
K <- 100
RES1 <- matrix(NA, K, 7)
for (i in 1:K) {
  x.A <- rmnorm(150, rep(0, 2), diag(2))
  x.B <- rmnorm(150, rep(1, 2), diag(2))
  x.tran <- rbind(x.A[1:50, ], x.B[1:50, ])
  x.test <- rbind(x.A[-c(1:50), ], x.B[-c(1:50), ])
  y.tran <- factor(rep(0:1, each=50))
  y.test <- factor(rep(0:1, each=100))
  RES1[i,] <- MissClassRate(x.tran, x.test, y.test, y.tran)
}
boxplot(RES1, boxwex=0.5, col=2:8,names=c("LDA", "QDA", "KNN-1",
                                          "KNN-5", "KNN-20", "Logit", "NB"), main="Scenario 1",
        ylab="Test Error Rates")

apply(RES1, 2, mean)




RES2 <- matrix(NA, K, 7)
for (i in 1:K) {
  x.A <- rmnorm(150, rep(0, 2), matrix(c(1,-0.5,-0.5,1),2))
  x.B <- rmnorm(150, rep(1, 2), matrix(c(1,-0.5,-0.5,1),2))
  x.tran <- rbind(x.A[1:50, ], x.B[1:50, ])
  x.test <- rbind(x.A[-c(1:50), ], x.B[-c(1:50), ])
  y.tran <- factor(rep(0:1, each=50))
  y.test <- factor(rep(0:1, each=100))
  RES2[i,] <- MissClassRate(x.tran, x.test, y.test, y.tran)
}
boxplot(RES2, boxwex=0.5, col=2:8,names=c("LDA", "QDA", "KNN-1",
                                          "KNN-5", "KNN-20", "Logit", "NB"), main="Scenario 2",
        ylab="Test Error Rates")

apply(RES2, 2, mean)





RES3 <- matrix(NA, K, 7)
for (i in 1:K) {
  x.A <- cbind(rt(150, df=5, ncp=0), rt(150, df=5, ncp=0))
  x.B <- cbind(rt(150, df=5, ncp=0.5), rt(150, df=5, ncp=0.5))
  x.tran <- rbind(x.A[1:50, ], x.B[1:50, ])
  x.test <- rbind(x.A[-c(1:50), ], x.B[-c(1:50), ])
  y.tran <- factor(rep(0:1, each=50))
  y.test <- factor(rep(0:1, each=100))
  RES3[i,] <- MissClassRate(x.tran, x.test, y.test, y.tran)
}
boxplot(RES3, boxwex=0.5, col=2:8,names=c("LDA", "QDA", "KNN-1",
                                          "KNN-5", "KNN-20", "Logit", "NB"), main="Scenario 3",
        ylab="Test Error Rates")

apply(RES3, 2, mean)






RES4 <- matrix(NA, K, 7)
for (i in 1:K) {
  x.A <- rmnorm(150, rep(0, 2), matrix(c(1,0.5,0.5,1),2))
  x.B <- rmnorm(150, rep(1, 2), matrix(c(1,-0.5,-0.5,1),2))
  x.tran <- rbind(x.A[1:50, ], x.B[1:50, ])
  x.test <- rbind(x.A[-c(1:50), ], x.B[-c(1:50), ])
  y.tran <- factor(rep(0:1, each=50))
  y.test <- factor(rep(0:1, each=100))
  RES4[i,] <- MissClassRate(x.tran, x.test, y.test, y.tran)
}
boxplot(RES4, boxwex=0.5, col=2:8,names=c("LDA", "QDA", "KNN-1",
                                          "KNN-5", "KNN-20", "Logit", "NB"), main="Scenario 4",
        ylab="Test Error Rates")

apply(RES4, 2, mean)





RES5 <- matrix(NA, K, 7)
for (i in 1:K) {
  x.A <- rmnorm(150, rep(0, 2), diag(2))
  x.B <- rmnorm(150, rep(1, 2), diag(2))
  x.tran <- rbind(x.A[1:50, ], x.B[1:50, ])
  x.test <- rbind(x.A[-c(1:50), ], x.B[-c(1:50), ])
  tr.int <- x.tran[,1]*x.tran[,2];te.int <- x.test[,1]*x.test[,2]
  xb.tr <- cbind(x.tran,tr.int)%*%c(-0.5,0.5,1)
  xb.te <- cbind(x.test,te.int)%*%c(-0.5,0.5,1)
  y.tran <- rep(0, 100); y.test <- rep(0, 200)
  y.tran[xb.tr > 0] <- 1; y.tran <- factor(y.tran)
  y.test[xb.te > 0] <- 1; y.test <- factor(y.test)
  RES5[i,] <- MissClassRate(x.tran, x.test, y.test, y.tran)
}
boxplot(RES5, boxwex=0.5, col=2:8,names=c("LDA", "QDA", "KNN-1",
                                          "KNN-5", "KNN-20", "Logit", "NB"), main="Scenario 5",
        ylab="Test Error Rates")

apply(RES5, 2, mean)








RES6 <- matrix(NA, K, 7)
for (i in 1:K) {
  x.A <- rmnorm(150, rep(0, 2), diag(2))
  x.B <- rmnorm(150, rep(1, 2), diag(2))
  x.tran <- rbind(x.A[1:50, ], x.B[1:50, ])
  x.test <- rbind(x.A[-c(1:50), ], x.B[-c(1:50), ])
  tr.int <- exp(x.tran[,1])/log(abs(x.tran[,2]))
  te.int <- exp(x.test[,1])/log(abs(x.test[,2]))
  xb.tr <- cbind(x.tran,tr.int)%*%c(-0.5,0.5,1)
  xb.te <- cbind(x.test,te.int)%*%c(-0.5,0.5,1)
  y.tran <- rep(0, 100); y.test <- rep(0, 200)
  y.tran[xb.tr > 0] <- 1; y.tran <- factor(y.tran)
  y.test[xb.te > 0] <- 1; y.test <- factor(y.test)
  RES6[i,] <- MissClassRate(x.tran, x.test, y.test, y.tran)
}
boxplot(RES6, boxwex=0.5, col=2:8,names=c("LDA", "QDA", "KNN-1",
                                          "KNN-5", "KNN-20", "Logit", "NB"), main="Scenario 6",
        ylab="Test Error Rates")

apply(RES6, 2, mean)







par(mfrow=c(2,3))
boxplot(RES1, boxwex=0.5, col=2:8, ylim=c(0,0.6),
        names=c("LDA","QDA","KNN-1","KNN-5","KNN-20","Logit","NB"),
        main="Scenario 1", ylab="Test Error Rates")
boxplot(RES2, boxwex=0.5, col=2:8, ylim=c(0,0.6),
        names=c("LDA","QDA","KNN-1","KNN-5","KNN-20","Logit","NB"),
        main="Scenario 2", ylab="Test Error Rates")
boxplot(RES3, boxwex=0.5, col=2:8, ylim=c(0,0.6),
        names=c("LDA","QDA","KNN-1","KNN-5","KNN-20","Logit","NB"),
        main="Scenario 3", ylab="Test Error Rates")
boxplot(RES4, boxwex=0.5, col=2:8, ylim=c(0,0.6),
        names=c("LDA","QDA","KNN-1","KNN-5","KNN-20","Logit","NB"),
        main="Scenario 4", ylab="Test Error Rates")
boxplot(RES5, boxwex=0.5, col=2:8, ylim=c(0,0.6),
        names=c("LDA","QDA","KNN-1","KNN-5","KNN-20","Logit","NB"),
        main="Scenario 5", ylab="Test Error Rates")
boxplot(RES6, boxwex=0.5, col=2:8, ylim=c(0,0.6),
        names=c("LDA","QDA","KNN-1","KNN-5","KNN-20","Logit","NB"),
        main="Scenario 6", ylab="Test Error Rates")

