## Simple example (simulate data set)
set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1, ] <- x[y==1, ] + 1
plot(x, col=(3-y), pch=19, xlab="X1", ylab="X2")

## Support vector classifier with cost=10
library(e1071)
dat <- data.frame(x, y=as.factor(y))
dat
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE) #lr-kernel
plot(svmfit, dat)
summary(svmfit) # Number of Support Vectors:  7
names(svmfit) 

svmfit$index # suppor vector
svmfit$coefs
## coefficient beta_0 (negative)
beta0 <- svmfit$rho







## coefficient beta_1 and beta_2
beta <- drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0
beta

plot(x, col=(3-y), pch=19, xlab="X1", ylab="X2")
points(x[svmfit$index, ], pch=5, cex=2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2) # margins's line
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2) # margins's line

## Different value of tuning parameter (cost=0.1)
svmfit <- svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
svmfit$index
beta0
beta

beta <- drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 <- svmfit$rho
plot(x, col=(3-y), pch=19, xlab="X1", ylab="X2")
points(x[svmfit$index, ], pch=5, cex=2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)






## A function to create a grid of values or a lattice of values
make.grid <- function(x, n = 75) {
  ran <- apply(x, 2, range)
  x1 <- seq(from = ran[1,1], to = ran[2,1], length = n)
  x2 <- seq(from = ran[1,2], to = ran[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid <- make.grid(x)
xgrid[1:76,]

## Classification of potential test set
ygrid <- predict(svmfit, xgrid)
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index, ], pch=5, cex=2)







## Cross validation to find the optimal tuning parameter (cost)
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

## Generate test set
set.seed(4321)
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1, ] <- xtest[ytest==1, ] + 1
testdat <- data.frame(xtest, y=as.factor(ytest))

## Compute misclassification rate for the optimal model
ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)
mean(ypred!=testdat$y)






## Misclassification rate for other value of cost (cost=0.01)
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred <- predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)
mean(ypred!=testdat$y)

## Misclassification rate for other value of cost (cost=1)
svmfit <- svm(y~., data=dat, kernel="linear", cost=1, scale=FALSE)
ypred <- predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)
mean(ypred!=testdat$y)

## Misclassification rate for other value of cost (cost=100)
svmfit <- svm(y~., data=dat, kernel="linear", cost=100, scale=FALSE)
ypred <- predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)
mean(ypred!=testdat$y)






library(mnormt)
library(e1071)
## Misclassification Rate of SVC
SVC.MCR <- function(x.tran, x.test, y.tran, y.test,
                    cost=c(0.01,0.1,1,10,100)) {
  dat <- data.frame(x.tran, y=as.factor(y.tran))
  testdat <- data.frame(x.test, y=as.factor(y.test))
  MCR <- rep(0, length(cost)+1)
  for (i in 1:length(cost)) {
    svmfit <- svm(y~., data=dat, kernel="linear", cost=cost[i])
    MCR[i] <- mean(predict(svmfit, testdat)!=testdat$y)
  }
  tune.out <- tune(svm, y~., data=dat, kernel="linear",
                   ranges=list(cost=cost))
  pred <- predict(tune.out$best.model, testdat)
  MCR[length(cost)+1] <- mean(pred!=testdat$y)
  MCR
}




set.seed(123)
K <- 100
RES <- matrix(NA, K, 6)
for (i in 1:K) {
  x.A <- rmnorm(100, rep(0, 2), matrix(c(1,-0.5,-0.5,1),2))
  x.B <- rmnorm(100, rep(1, 2), matrix(c(1,-0.5,-0.5,1),2))
  x.tran <- rbind(x.A[1:50, ], x.B[1:50, ])
  x.test <- rbind(x.A[-c(1:50), ], x.B[-c(1:50), ])
  y.tran <- factor(rep(0:1, each=50))
  y.test <- factor(rep(0:1, each=50))
  RES[i,] <- SVC.MCR(x.tran, x.test, y.tran, y.test)
}
apply(RES, 2, mean)
boxplot(RES, boxwex=0.5, col=2:7, names=c("0.01", "0.1", "1", "10",
                                          "100", "CV"), main="", ylab="Classification Error Rates")





## Simulate non-linear data set
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(-1, 150), rep(1, 50))
dat <- data.frame(x, y=as.factor(y))
plot(x, col=y+3, pch=19)

fit <- svm(y~.,data=dat, kernel="radial", gamma=0.5, cost=0.1)
plot(fit, dat)
summary(fit)

fit <- svm(y~.,data=dat, kernel="radial", gamma=0.5, cost=5)
plot(fit, dat)
summary(fit)





fit <- svm(y~.,data=dat, kernel="radial", gamma=0.5, cost=1)

px1 <- seq(round(min(x[,1]),1), round(max(x[,1]),1), 0.1)
px2 <- seq(round(min(x[,2]),1), round(max(x[,2]),1), 0.1)
xgrid <- expand.grid(X1=px1, X2=px2)
ygrid <- as.numeric(predict(fit, xgrid))
ygrid[ygrid==1] <- -1
ygrid[ygrid==2] <- 1

plot(xgrid, col=ygrid+3, pch = 20, cex = .2)
points(x, col = y+3, pch = 19)

pred <- predict(fit, xgrid, decision.values=TRUE)
func <- attributes(pred)$decision
contour(px1, px2, matrix(func, length(px1), length(px2)),
        level=0, col="purple", lwd=2, lty=2, add=TRUE)






## Separate training and test sets
set.seed(1234)
tran <- sample(200, 100)
test <- setdiff(1:200, tran)

## Fit SVM with a radial kernel (gamma=1, cost=0.01)
svmfit <- svm(y~.,data=dat[tran, ],kernel="radial",gamma=1,cost=0.01)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])

## Fit SVM with a radial kernel (gamma=1, cost=1)
svmfit <- svm(y~.,data=dat[tran, ],kernel="radial",gamma=1,cost=1)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])

## Fit SVM with a radial kernel (gamma=1, cost=1000)
svmfit <- svm(y~.,data=dat[tran, ],kernel="radial",gamma=1,cost=1000)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])






## Fit SVM with a radial kernel (gamma=0.5, cost=1)
svmfit <- svm(y~.,data=dat[tran, ],kernel="radial",gamma=0.5,cost=1)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])

## Fit SVM with a radial kernel (gamma=1, cost=1)
svmfit <- svm(y~.,data=dat[tran, ],kernel="radial",gamma=1,cost=1)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])

## Fit SVM with a radial kernel (gamma=3, cost=1000)
svmfit <- svm(y~.,data=dat[tran, ],kernel="radial",gamma=2,cost=1)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])

## Fit SVM with a radial kernel (gamma=5, cost=1000)
svmfit <- svm(y~.,data=dat[tran, ],kernel="radial",gamma=5,cost=1)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])






## Find the optimal tuning parameters
set.seed(1)
tune.out <- tune(svm, y~., data=dat[tran, ], kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
tune.out$best.parameters

pred <- predict(tune.out$best.model, dat[test,])
table(pred=pred, true=dat[test, "y"])
mean(pred!=dat[test, "y"])

## Polynomial kernel with a degree of 2
svmfit <- svm(y~., data=dat[tran, ], kernel="polynomial", degree=2,
              cost=0.1)
summary(svmfit)
pred <- predict(svmfit, dat[test, ])
mean(pred!=dat[test, "y"])






## Find the optimal tuning parameters
set.seed(12)
tune.out <- tune(svm, y~., data=dat[tran, ], kernel="polynomial",
                 ranges=list(cost=c(0.1,1,10,100), degree=c(1,2,3,4)))
summary(tune.out)
tune.out$best.parameters

pred <- predict(tune.out$best.model, dat[test,])
table(pred=pred, true=dat[test, "y"])
mean(pred!=dat[test, "y"])

tune.out <- tune(svm, y~., data=dat[tran, ], kernel="sigmoid",
                 ranges=list(cost=c(0.1,1,10,100), gamma=c(0.1,1,2,3)))
summary(tune.out)
tune.out$best.parameters

pred <- predict(tune.out$best.model, dat[test,])
table(pred=pred, true=dat[test, "y"]) #이경우에는 별로 좋지 않다
mean(pred!=dat[test, "y"])






## A function to plot an ROC curve given pred and truth
library(ROCR)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)}

## Compute fitted values
svmfit.opt <- svm(y ~., data=dat[tran, ],kernel="radial", gamma=1,
                  cost=0.1, decision.values=T)
fitted <- attributes(predict(svmfit.opt, dat[tran, ],
                             decision.values=TRUE))$decision.values

ry <- y
ry[y==-1] <- 1
ry[y==1] <- -1







## ROC plot
par(mfrow=c(1,2))
rocplot(fitted, ry[tran], main="Training Data", col=4, lwd=2) #단순모델

## More flexible fit (gamma=50)
svmfit.flex <- svm(y~., data=dat[tran, ], kernel ="radial", gamma=1,
                   cost=1000, decision.values=T)
fitted2 <- attributes(predict(svmfit.flex, dat[tran, ],
                              decision.values=TRUE))$decision.values
rocplot(fitted2, ry[tran], add=T, col="red", lwd=2) #복잡모델

## ROC curves for test set
fitted3 <- attributes(predict(svmfit.opt, dat[test, ],
                              decision.values=T))$decision.values
rocplot(fitted3, ry[test], main="Test Data", col=4, lwd=2)
fitted4 <- attributes(predict(svmfit.flex, dat[test, ],
                              decision.values=T))$decision.values
rocplot(fitted4, ry[test], add=T, col="red", lwd=2)







Heart <- read.csv("http://faculty.marshall.usc.edu/gareth-james/
ISL/Heart.csv", h=TRUE)
str(Heart)
summary(Heart)
Heart <- Heart[, colnames(Heart)!="X"]
Heart[,"Sex"] <- factor(Heart[,"Sex"], 0:1, c("female", "male"))
Heart[,"Fbs"] <- factor(Heart[,"Fbs"], 0:1, c("false", "true"))
Heart[,"ExAng"] <- factor(Heart[,"ExAng"], 0:1, c("no", "yes"))
Heart[,"ChestPain"] <- as.factor(Heart[,"ChestPain"])
Heart[,"Thal"] <- as.factor(Heart[,"Thal"])
Heart[,"AHD"] <- as.factor(Heart[,"AHD"])
w <- which(is.na(Heart), arr.ind=TRUE)
Heart <- Heart[-w[,1],]
dim(Heart)
summary(Heart)







## Separate training and test sets
set.seed(123)
train <- sample(1:nrow(Heart), nrow(Heart)/2)
test <- setdiff(1:nrow(Heart), train)

## Classification tree
library(tree)
tree.tran <- tree(AHD ~., Heart, subset = train)
tree.pred <- predict(tree.tran, Heart[test,], type="class")
mean(tree.pred!=Heart$AHD[test])

## Cross-validated tree
set.seed(1234)
cv.heart <- cv.tree(tree.tran, FUN=prune.misclass, K=5)
w <- cv.heart$size[which.min(cv.heart$dev)]
prune.heart <- prune.misclass(tree.tran, best=w)
heart.pred <- predict(prune.heart, Heart[test,], type="class")
mean(heart.pred!=Heart$AHD[test])






set.seed(1111)
## Bagging and random forest (m=3, 4, and 5)
library(randomForest)
bag.heart <- randomForest(x=Heart[train,-14], y=Heart[train,14],
                          xtest=Heart[test,-14], ytest=Heart[test,14], mtry=13)
bag.heart$test$err.rate[500,1]
rf.heart <- randomForest(x=Heart[train,-14], y=Heart[train,14],
                         xtest=Heart[test,-14], ytest=Heart[test,14], mtry=3)
rf.heart$test$err.rate[500,1]
rf.heart <- randomForest(x=Heart[train,-14], y=Heart[train,14],
                         xtest=Heart[test,-14], ytest=Heart[test,14], mtry=4)
rf.heart$test$err.rate[500,1]
rf.heart <- randomForest(x=Heart[train,-14], y=Heart[train,14],
                         xtest=Heart[test,-14], ytest=Heart[test,14], mtry=5)
rf.heart$test$err.rate[500,1]






library(e1071)
## SVM with a linear kernel
tune.out <- tune(svm, AHD~., data=Heart[train, ], kernel="linear",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
heart.pred <- predict(tune.out$best.model, Heart[test,])
table(heart.pred, Heart$AHD[test])
mean(heart.pred!=Heart$AHD[test])

## SVM with a radial kernel
tune.out <- tune(svm, AHD~., data=Heart[train, ], kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100), gamma=c(0.5,1,2,3)))
heart.pred <- predict(tune.out$best.model, Heart[test,])
table(heart.pred, Heart$AHD[test])
mean(heart.pred!=Heart$AHD[test])

## SVM with a polynomial kernel
tune.out <- tune(svm, AHD~.,data=Heart[train, ],kernel="polynomial",
                 ranges=list(cost=c(0.1,1,10,100), degree=c(1,2,3)))
heart.pred <- predict(tune.out$best.model, Heart[test,])
table(heart.pred, Heart$AHD[test])
mean(heart.pred!=Heart$AHD[test])

## SVM with a sigmoid kernel
tune.out <- tune(svm, AHD~.,data=Heart[train, ],kernel="sigmoid",
                 ranges=list(cost=c(0.1,1,10,100), gamma=c(0.5,1,2,3)))
heart.pred <- predict(tune.out$best.model, Heart[test,])
table(heart.pred, Heart$AHD[test])
mean(heart.pred!=Heart$AHD[test])
#일반적으로 svm이 랜덤포레스트보다 더 좋았다. 






## Generate 2-classes data
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))

## Add one more class
set.seed(1)
x <- rbind(x, matrix (rnorm(50*2), ncol=2))
y <- c(y, rep (0,50))
x[y==0, 2] <- x[y==0, 2] + 2
dat <- data.frame(x=x, y=as.factor (y))
plot(x, col=(y+1), pch=19)

## Fit SVM based on one-versus-one classification
svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)


