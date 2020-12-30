library(ISLR)
library(tree)

data(Hitters)
str(Hitters)

## Missing data
summary(Hitters$Salary)
miss <- is.na(Hitters$Salary) #결측치를 가진 데이터를 학습에서 제외

## Fit a regression tree
g <- tree(log(Salary) ~ Years + Hits, subset=!miss, Hitters)
g #트리 사이즈(# leafs)는 8개 별모양갯수
summary(g)

## Draw a tree
plot(g)
text(g, pretty=0)





## Prune a tree
g2 <- prune.tree(g, best=3) #트리 사이즈(# leafs)는 3개로 설정
plot(g2)
text(g2, pretty=0)

## Another R package for tree
library(rpart)
library(rpart.plot)

## Fit a regression tree
g3 <- rpart(log(Salary) ~ Years + Hits, subset=!miss, Hitters)
g3 #트리 사이즈(# leafs)는 7개 별모양갯수 여기서는 앞의 모델과 알고리즘이 달라서 다른값의 트리가 나온다
summary(g3)

## Draw a fancy tree
prp(g3, branch.lwd=4, branch.col="darkgreen", extra=101)






par(mar=c(4,4,3,4))
plot(Hitters$Years, Hitters$Hits, pch=16, col="orange", xlab="Years",
     ylab="Hits", xaxt="n", yaxt="n")
axis(1, at=c(1,4.5,24), labels=c("1","4.5","24"), tick=FALSE, pos=1)
axis(4, at=c(1,117.5,238), labels=c("1","117.5","238"), tick=FALSE,
     las=2)
abline(v=4.5, lwd=2, col="darkgreen")
segments(4.5,117.5,25,117.5, lwd=2, lty=1, col="darkgreen")
text(2,117.5,font=2,cex=1.4,expression(R[1]))
text(13,50,font=2,cex=1.4,expression(R[2]))
text(13,180.5,font=2,cex=1.4,expression(R[3]))

## Compute the mean of log Salary for each region
attach(Hitters)
mean(log(Salary[Years < 4.5]), na.rm=TRUE)
mean(log(Salary[Years >= 4.5 & Hits < 117.5]), na.rm=TRUE)
mean(log(Salary[Years >= 4.5 & Hits >= 117.5]), na.rm=TRUE)






library(ISLR);library(tree);attach(Hitters)

## Fit a regression tree with 263 observations
miss <- is.na(Salary)
miss
sum(miss)
summary(Hitters$Salary)

g <- tree(log(Salary) ~ Years + Hits + RBI + PutOuts + Walks + Runs +
            Assists + HmRun + Errors + AtBat, subset=!miss, Hitters)
plot(g)
text(g, pretty=0)
summary(g)

## Perform 6 fold CV
set.seed(1234)
cv.g <- cv.tree(g, K=6)
cv.g
cv.g$size # number of terminal node
cv.g$dev # 트리의 잔차 dev만 확인 k는 확인안해도 됨
plot(cv.g$size, cv.g$dev, type="b")

## Find the optimal tree size that minimizes MSE
w <- which.min(cv.g$dev) #dev가 가장낮을때의 트리
w #5가 가장좋은 트리사이즈
g2 <- prune.tree(g, best=cv.g$size[w])
g2
plot(g2)
text(g2, pretty=0)





## Make a newdata
newdata <- data.frame(Salary, Years, Hits, RBI, PutOuts, Walks,
                      Runs, Assists, HmRun, Errors, AtBat)
newdata <- newdata[!miss,]
dim(newdata)
dim(Hitters)
## Separate samples into 132 training sets and 131 test sets
set.seed(1111)
train <- sample(1:nrow(newdata), ceiling(nrow(newdata)/2))
length(train)
263-132 #test set size

## Fit a tree with training set and compute test MSE
## in the original sclae
tree.train <- tree(log(Salary) ~ ., subset=train, newdata)
yhat <- exp(predict(tree.train, newdata[-train, ])) # 로그변환된 값을 다시 바꿔줌
tree.test <- newdata[-train, "Salary"]
tree.test
plot(yhat, tree.test)
abline(0,1)
mean((yhat-tree.test)^2) #error가 많니 높네
summary(tree.train)




## Perform 6 fold CV for training sets
set.seed(1234)
cv.g <- cv.tree(tree.train, K=6)
plot(cv.g$size, cv.g$dev, type="b")
cv.g
## Prune a tree with training set and compute test MSE
## in the original sclae
w <- which.min(cv.g$dev)
prune.tree <- prune.tree(tree.train, best=cv.g$size[w])
yhat2 <- exp(predict(prune.tree, newdata[-train, ]))
plot(yhat2, tree.test)
abline(0,1)
mean((yhat2-tree.test)^2) #여전히 크지만 전보다는 낮아졌다
## Compute test MSE of least square estimates
g0 <- lm(log(Salary)~., newdata, subset=train)
yhat3 <- exp(predict(g0, newdata[-train,]))
mean((newdata$Salary[-train]-yhat3)^2) #트리모델보다 더 높은값이 나왔다

p <- seq(0,1,0.01)
plot(p, -p*log(p),type="l")




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

dim(Heart)
summary(Heart)






## Remove missing data
sum(is.na(Heart))
w <- which(is.na(Heart), arr.ind=TRUE)
w
Heart[w[,1],c(12,13)]
Heart <- Heart[-w[,1],] # 결측치열 제거거

## Updated Heart dataset
dim(Heart)
sum(is.na(Heart))
summary(Heart)
plot(Heart)

## Logistic Regression
g <- glm(AHD ~., family="binomial", Heart)
summary(g)






## Fit a classification tree
library(tree)
tree.heart <- tree(AHD ~., Heart)
summary(tree.heart) #factor 값이어야만 분류트리가 잘 적합됨
tree.heart
plot(tree.heart)
text(tree.heart)
#a, b, c, d...는 factor 변수에서 순서를 적용한것
plot(tree.heart)
text(tree.heart, pretty=0) #
#a,b,c,d로 라벨을 바꾸지 않고 실제 펙터를 보여줌

# 그림의 왼쪽부분에 각각 모두 no로 분류되었는데 왜그런가??
#각각 no를 구분하는 확률이 다르므로 구분되었다
# 32) Chol < 226.5 8   0.000 No ( 1.00000 0.00000 ) *
# 33) Chol > 226.5 9  11.460 No ( 0.66667 0.33333 ) *
# 17) MaxHR > 152.5 63   0.000 No ( 1.00000 0.00000 ) *
# 9) Age > 57.5 35  41.880 No ( 0.71429 0.28571 )  
# 18) Fbs: false 29  37.360 No ( 0.65517 0.34483 ) *
# 19) Fbs: true 6   0.000 No ( 1.00000 0.00000 ) *
# 오른쪽 아래의 node, [RestECG < 1] 를 보면, 이상한점을 알 수 있다. node의 양 갈래의 예측이 모두 Yes인 것이다. 둘다 Yes 라면 왜 굳이 나눈걸까? 이는 node의 purity(순수한 정도) 때문이다. [RestECG < 1]의 오른쪽 leaf에는, 9개의 자료가 모두 Yes이고, 왼쪽 leaf에는 7/11이 Yes였다. 따라서 실제 예측을 할때에도, [RestECG < 1]의 오른쪽으로 배정된 test data는 더욱 확실하게 Yes라고 할 수 있고 왼쪽으로 배정된 test data는 낮은 확실성을 갖고 예측을 할 수 있게 되는 것이다. node[RestECG < 1]는 classification error에는 차이가 없지만, GIni index나 entropy의 기준에서는 차이가 있다.(!) 이러한 이유로 후자를 사용하는 것이다.

#추가로, 질적변수의 경우도 역시나 node로 활용될 수 있는데, 위의 예시의 경우 [ChestPaint : a]의 node가 그 예이다. ChestPaint가 a클래스이면 오른쪽 leaf, 아니면 왼쪽 leaf로 가는 식이다.

## predict the probability of each class or class type
predict(tree.heart, Heart) # 확률이 높은 곳으로 고름 둘다 0.50000000이면 랜덤으로 고름
#또는 임계값을 조정해주어도 됨
predict(tree.heart, Heart, type="class")

## Compute classification error rate of training observations
pred <- predict(tree.heart, Heart, type="class")
table(pred, Heart$AHD)
mean(pred!=Heart$AHD)
#아마 트레인 테스트를 구분하지않아 0.0976431로 낮은수치가 나왔을수도 있다.





## Separate training and test sets
set.seed(123)
train <- sample(1:nrow(Heart), nrow(Heart)/2)
test <- setdiff(1:nrow(Heart), train)

length(train)
length(test)

heart.test <- Heart[test, ]
heart.tran <- tree(AHD ~., Heart, subset = train)
summary(heart.tran) #Number of terminal nodes:  12이었다.
heart.pred <- predict(heart.tran, heart.test, type="class")

## Compute classification error rate
table(heart.pred, Heart$AHD[test])
mean(heart.pred!=Heart$AHD[test])
# [1] 0.2751678 오분류율 1/4보다 높다

## Run 5-fold cross validataion
set.seed(1234)
cv.heart <- cv.tree(heart.tran, FUN=prune.misclass, K=5)
cv.heart

par(mfrow=c(1, 2))
plot(cv.heart$size, cv.heart$dev, type="b")
plot(cv.heart$k, cv.heart$dev, type="b")






## Find the optimal tree size
w <- cv.heart$size[which.min(cv.heart$dev)]
w

## Prune the tree with the optimal size
prune.heart <- prune.misclass(heart.tran, best=w)
summary(prune.heart) # Number of terminal nodes:  5 였다


par(mfrow=c(1, 2))
plot(heart.tran)
text(heart.tran)
plot(prune.heart)
text(prune.heart, pretty=0)

## Compute classification error of the subtree
heart.pred <- predict(prune.heart, heart.test, type="class")
table(heart.pred, Heart$AHD[test])
mean(heart.pred!=Heart$AHD[test])
# 여전히 높은 오분류율이지만 앞의 값보다는 낮아졌다





set.seed(111)
K <- 100
RES <- matrix(0, K, 2)
for (i in 1:K) {
  train <- sample(1:nrow(Heart), nrow(Heart)/2)
  test <- setdiff(1:nrow(Heart), train)
  heart.test <- Heart[test, ]
  heart.tran <- tree(AHD ~., Heart, subset = train)
  heart.pred <- predict(heart.tran, heart.test, type="class")
  RES[i,1] <- mean(heart.pred!=Heart$AHD[test])
  
  cv.heart <- cv.tree(heart.tran, FUN=prune.misclass, K=5)
  w <- cv.heart$size[which.min(cv.heart$dev)]
  prune.heart <- prune.misclass(heart.tran, best=w)
  heart.pred.cv <- predict(prune.heart, heart.test, type="class")
  RES[i,2] <- mean(heart.pred.cv!=Heart$AHD[test])
}
RES
apply(RES, 2, mean) #비슷한값 여기서는 prune.misclass가 크게 중요하지 않았다.
boxplot(RES, col=c("orange", "lightblue"), boxwex=0.6,
        names=c("unpruned tree", "pruned tree"),
        ylab="Classification Error Rate")



x <- seq(1,10)
x
sample(x, replace = TRUE)


## Separate training and test sets
set.seed(123)
train <- sample(1:nrow(Heart), nrow(Heart)/2)
test <- setdiff(1:nrow(Heart), train)

## Classification error rate for single tree
heart.tran <- tree(AHD ~., subset = train, Heart)
heart.pred <- predict(heart.tran, Heart[test, ], type="class")
tree.err <- mean(Heart$AHD[test]!=heart.pred)
tree.err # one single tree

## Bagging
set.seed(12345)
B <- 500 # 500-times tree
n <- nrow(Heart)





Vote <- rep(0, length(test))
bag.err <- NULL

for (i in 1:B) {
  index <- sample(train, replace=TRUE) # 복원추출(boostrap sample)
  heart.tran <- tree(AHD ~., Heart[index,]) # new data set
  heart.pred <- predict(heart.tran, Heart[test, ], type="class")
  Vote[heart.pred=="Yes"] <- Vote[heart.pred=="Yes"] + 1 # yes 이면 1표추가
  preds <- rep("Yes", length(test))
  preds[Vote < i/2] <- "No" #전체가 50% 이하면 No
  bag.err[i] <- mean(Heart$AHD[test]!=preds) # 시작부터 현재까지의 오분류율
}

plot(bag.err, type="l", xlab="Number of Trees", col=1,
     ylab="Classification Error Rate")
abline(h=tree.err, lty=2, col=2)
legend("topright", c("Single tree", "Bagging"), col=c(2,1),
       lty=c(2,1))

sort(sample(seq(20),replace=TRUE))
# obestimator 선택이 안되는 집단을 test set으로 두고 반복 시행





## n: sample size, B: the number of bootstrap
## K: the number of simulation
n <- 100
B <- 500
K <- 10

m <- array(seq(n), c(n, B, K))
out <- matrix(0, n, K)

for (i in 1:K) {
  nm <- apply(m[,,i], 2, function(x) sample(x, replace=TRUE))
  x <- matrix(0, n, B)
  for (j in 1:B) x[nm[,j], j] <- 1
  out[,i] <- apply(x, 1, sum)/B
}

boxplot(out, boxwex=0.5, col="orange", ylim=c(0.5,0.7),
        xlab="Simulation replication",
        ylab="Proportion of bootstrapped observations")
# 부스트트랩에 뽑힌 train set의 비율





## Average of misclassification rate for single tree
## over 50 replications
set.seed(12345)
K <- 50
Err <- NULL

for (i in 1:K) {
  train <- sample(1:nrow(Heart), nrow(Heart)*2/3)
  test <- setdiff(1:nrow(Heart), train)
  heart.tran <- tree(AHD ~., subset = train, Heart)
  heart.pred <- predict(heart.tran, Heart[test, ], type="class")
  Err[i] <- mean(Heart$AHD[test]!=heart.pred)
}

Tree.Err <- mean(Err)
Tree.Err
summary(Err)





set.seed(1234)
Valid <- Vote <- Mis <- rep(0, nrow(Heart))
OOB.err <- NULL
for (i in 1:B) {
  index <- sample(1:nrow(Heart), replace=TRUE)
  test <- setdiff(1:nrow(Heart), unique(index))
  Valid[test] <- Valid[test] + 1
  heart.tran <- tree(AHD ~., Heart[index,])
  heart.pred <- predict(heart.tran, Heart[test,], type="class")
  Vote[test] <- Vote[test] + (heart.pred=="Yes")
  preds <- rep("Yes", length(test))
  preds[Vote[test]/Valid[test] < 0.5] <- "No"
  wh <- which(Heart$AHD[test]!=preds)
  Mis[test[wh]] <- -1; Mis[test[-wh]] <- 1 #valid 와 vote가 다르면 -1 같으면 1
  OOB.err[i] <- sum(Mis==-1)/sum(Mis!=0)
}
plot(OOB.err, type="l", xlab="Number of Trees", col=1,
     ylab="Classification Error Rate", ylim=c(0.1,0.4))
abline(h=Tree.Err, lty=2, col=2)
legend("topright", c("Single tree", "OOB"), col=c(2,1), lty=c(2,1))





## Separate training and test sets
set.seed(123)
train <- sample(1:nrow(Heart), nrow(Heart)/2)
test <- setdiff(1:nrow(Heart), train)

## Bagging : the number of predictors (m=13)
## Random forest : m = 6 or 4
B <- 500
n <- nrow(Heart)
Vote1 <- Vote2 <- Vote3 <- rep(0, length(test))
Err <- matrix(0, B, 3)

for (i in 1:B) {
  index <- sample(train, replace=TRUE)
  s6 <- c(sort(sample(1:13, 6)),14) 
  s4 <- c(sort(sample(1:13, 4)),14)
  tree1 <- tree(AHD ~., Heart[index,]) # 전체 value
  tree2 <- tree(AHD ~., Heart[index, s6]) # 6가지의 value
  tree3 <- tree(AHD ~., Heart[index, s4]) # 4가지의 value
  pred1 <- predict(tree1, Heart[test, ], type="class")
  pred2 <- predict(tree2, Heart[test, ], type="class")
  pred3 <- predict(tree3, Heart[test, ], type="class")
  Vote1[pred1=="Yes"] <- Vote1[pred1=="Yes"] + 1
  Vote2[pred2=="Yes"] <- Vote2[pred2=="Yes"] + 1
  Vote3[pred3=="Yes"] <- Vote3[pred3=="Yes"] + 1
  preds1 <- preds2 <- preds3 <- rep("Yes", length(test))
  preds1[Vote1 < i/2] <- "No"
  preds2[Vote2 < i/2] <- "No"
  preds3[Vote3 < i/2] <- "No"
  Err[i,1] <- mean(Heart$AHD[test]!=preds1)
  Err[i,2] <- mean(Heart$AHD[test]!=preds2)
  Err[i,3] <- mean(Heart$AHD[test]!=preds3)
}

matplot(Err, type="l", xlab="Number of Trees", lty=1, col=c(1,2,4),
        ylab="Classification Error Rate")
legend("topright", c("m = 13", "m = 6", "m = 4"), col=c(1,2,4),
       lty=1)  
  
  
  
  
  
## Install randomForest package
install.packages('randomForest')
library(randomForest)

## Separate training and test sets
set.seed(123)
train <- sample(1:nrow(Heart), nrow(Heart)/2)
test <- setdiff(1:nrow(Heart), train)

## Bagging (m=13)
bag.heart <- randomForest(x=Heart[train,-14], y=Heart[train,14],
                          xtest=Heart[test,-14], ytest=Heart[test,14],
                          mtry=13, importance=TRUE)
bag.heart
## Classification error rate of test set
bag.conf <- bag.heart$test$confusion[1:2,1:2]
1- sum(diag(bag.conf))/sum(bag.conf)

names(bag.heart$test)  
bag.heart$test$err.rate[1:10,]
1- sum(diag(tree.conf))/sum(tree.conf)

  
  
  
## Comparison with classification tree
library(tree)
tree.heart <- tree(AHD ~ ., Heart, subset=train)
yhat.tree <- predict(tree.heart, newdata=Heart[test,],type="class")
tree.conf <- table(yhat.tree, Heart$AHD[test])
1- sum(diag(tree.conf))/sum(tree.conf)

## Comparison with random forest with m = 6
rf1.heart <- randomForest(x=Heart[train,-14], y=Heart[train,14],
                          xtest=Heart[test,-14], ytest=Heart[test,14],
                          mtry=6, importance=TRUE)
rf1.conf <- rf1.heart$test$confusion[1:2,1:2]
1- sum(diag(rf1.conf))/sum(rf1.conf)

## Comparison with random forest with m = 4
rf2.heart <- randomForest(x=Heart[train,-14], y=Heart[train,14],
                          xtest=Heart[test,-14], ytest=Heart[test,14],
                          mtry=4, importance=TRUE)
rf2.conf <- rf2.heart$test$confusion[1:2,1:2]
1- sum(diag(rf2.conf))/sum(rf2.conf)
  




#feature importance
(ibag <- importance(bag.heart)) 
(irf1 <- importance(rf1.heart))
(irf2 <- importance(rf2.heart))

varImpPlot(bag.heart) 
varImpPlot(rf1.heart)
varImpPlot(rf2.heart)

par(mfrow=c(1,3))
barplot(sort(ibag[,3]),main="Bagging",horiz=TRUE,col=2)
barplot(sort(irf1[,3]),main="Random forest (m=6)",horiz=TRUE,col=2)
barplot(sort(irf2[,3]),main="Random forest (m=4)",horiz=TRUE,col=2)

barplot(sort(ibag[,4]),main="Bagging", horiz=TRUE,col=2)
barplot(sort(irf1[,4]),main="Random forest (m=6)",horiz=TRUE,col=2)
barplot(sort(irf2[,4]),main="Random forest (m=4)",horiz=TRUE,col=2)





## Install gbm package
install.packages('gbm')
library(gbm)

## Separate training and test sets
set.seed(123)
train <- sample(1:nrow(Heart), nrow(Heart)/2)
test <- setdiff(1:nrow(Heart), train)

## Create (0,1) response
Heart0 <- Heart
Heart0[,"AHD"] <- as.numeric(Heart$AHD)-1

## boosting (d=1)
boost.d1 <- gbm(AHD~., data=Heart0[train, ], distribution="bernoulli",
                n.trees=1000, interaction.depth=1)
summary(boost.d1)






yhat.d1 <- predict(boost.d1, newdata=Heart0[test, ],
                   type="response", n.trees=1000)
phat.d1 <- rep(0, length(yhat.d1))
phat.d1[yhat.d1 > 0.5] <- 1
mean(phat.d1!=Heart0[test, "AHD"])

## boosting (d=2)
boost.d2 <- gbm(AHD~., data=Heart0[train, ], distribution="bernoulli",
                n.trees=1000, interaction.depth=2)
yhat.d2 <- predict(boost.d2, newdata=Heart0[test, ],
                   type="response", n.trees=1000)
phat.d2 <- rep(0, length(yhat.d2))
phat.d2[yhat.d2 > 0.5] <- 1
mean(phat.d2!=Heart0[test, "AHD"])






## boosting (d=3)
boost.d3 <- gbm(AHD~., data=Heart0[train, ], distribution="bernoulli",
                n.trees=1000, interaction.depth=3)
yhat.d3 <- predict(boost.d3, newdata=Heart0[test, ],
                   type="response", n.trees=1000)
phat.d3 <- rep(0, length(yhat.d3))
phat.d3[yhat.d3 > 0.5] <- 1
mean(phat.d3!=Heart0[test, "AHD"])
## boosting (d=4)

boost.d4 <- gbm(AHD~., data=Heart0[train, ], distribution="bernoulli",
                n.trees=1000, interaction.depth=4)
yhat.d4 <- predict(boost.d4, newdata=Heart0[test, ],
                   type="response", n.trees=1000)
phat.d4 <- rep(0, length(yhat.d4))
phat.d4[yhat.d4 > 0.5] <- 1
mean(phat.d4!=Heart0[test, "AHD"])








set.seed(1111)
Err <- matrix(0, 3000, 4)

for (k in 1:4) {
  boost <- gbm(AHD~., data=Heart0[train, ], distribution="bernoulli",
               n.trees=3000, interaction.depth=k)
  for (i in 1:3000) {
    yhat <- predict(boost, newdata=Heart0[test, ],
                    type="response", n.trees=i)
    phat <- rep(0, length(yhat))
    phat[yhat > 0.5] <- 1
    Err[i,k] <- mean(phat!=Heart0[test, "AHD"])
  }
}

matplot(Err, type="l", xlab="Number of Trees", lty=2, col=1:4,
        ylab="Classification Error Rate")
legend("topright", c("d = 1", "d = 2", "d = 3", "d = 4"),
       col=1:4, lty=1)





