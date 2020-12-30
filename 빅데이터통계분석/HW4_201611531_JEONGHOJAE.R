#######################문제1#################################
#install.packages('ISLR')
library(ISLR)
data('College')
str(College)
data <- College

RNGkind(sample.kind = "Rounding")
set.seed(1111)
id <- rep(c("tr", "te"), c(600, 177))
index <- matrix(id, length(id), 100)
index <- apply(index, 2, sample)


fun.acc <- function(pred, test){
  acc <- NA
  tp <- sum(pred[test=="Yes"] == "Yes")
  tn <- sum(pred[test=="No"] == "No")
  fp <- sum(pred=="Yes") - tp
  fn <- sum(test=="Yes") - tp
  acc <- (tp+tn)/(tp+tn+fp+fn)
  
  return(acc)
}

fun.mcc <- function(pred, test){
  mcc <- NA
  tp <- sum(pred[test=="Yes"] == "Yes")
  tn <- sum(pred[test=="No"] == "No")
  fp <- sum(pred=="Yes") - tp
  fn <- sum(test=="Yes") - tp
  mcc <- ((tp*tn)-(fp*fn))/(sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
  
  return(mcc)
}

fun.f1 <- function(pred, test){
  f1 <- NA
  tp <- sum(pred[test=="Yes"] == "Yes")
  tn <- sum(pred[test=="No"] == "No")
  fp <- sum(pred=="Yes") - tp
  fn <- sum(test=="Yes") - tp
  f1 <- 2*tp/(2*tp+fp+fn)
  
  return(f1)
}


#######################문제2#################################
library(MASS)
library(e1071)

y <- data[,1]
y.num <- rep(0,length(y))
y.num[y=='Yes'] <- 1

alpha=0.5
K <- dim(index)[2]
lda.acc <- qda.acc <- nb.acc <- matrix(NA,K,1)

for(k in 1:K){
  tran <- which(index[,k]=="tr")
  test <- which(index[,k]=="te")
  
  testset <- data[test,]
  y <- data[,1]
  x <- data[,2:18]
  
  ##LDA
  lda.tran <- lda(Private ~., data=data, subset = tran)
  lda.pred <- predict(lda.tran,x)$class[-tran]
  lda.acc[k,1] <- fun.acc(lda.pred,testset)
  
  ##QDA
  qda.tran <- qda(Private ~., data=data, subset=tran)
  qda.pred <- predict(qda.tran,x)$class[-tran]
  qda.acc[k,1] <- fun.acc(qda.pred,testset)
  ##NB
  nb.tran <- naiveBayes(Private ~., data=data, subset=tran)
  nb.pred <- predict(nb.tran,x)[-tran]
  nb.acc[k,1] <- fun.acc(nb.pred,testset)
}

q2_lda.acc <- mean(lda.acc)
q2_qda.acc <- mean(qda.acc)
q2_nb.acc <- mean(nb.acc)




lda.mcc <- qda.mcc <- nb.mcc <- matrix(NA,K,1)

for(k in 1:K){
  tran <- which(index[,k]=="tr")
  test <- which(index[,k]=="te")
  
  testset <- data[test,]
  y <- data[,1]
  x <- data[,2:18]
  
  ##LDA
  lda.tran <- lda(Private ~., data=data, subset = tran)
  lda.pred <- predict(lda.tran,x)$class[-tran]
  lda.mcc[k,1] <- fun.mcc(lda.pred,testset)
  
  ##QDA
  qda.tran <- qda(Private ~., data=data, subset=tran)
  qda.pred <- predict(qda.tran,x)$class[-tran]
  qda.mcc[k,1] <- fun.mcc(qda.pred,testset)
  ##NB
  nb.tran <- naiveBayes(Private ~., data=data, subset=tran)
  nb.pred <- predict(nb.tran,x)[-tran]
  nb.mcc[k,1] <- fun.mcc(nb.pred,testset)
}

q2_lda.mcc <- mean(lda.mcc)
q2_qda.mcc <- mean(qda.mcc)
q2_nb.mcc<- mean(nb.mcc)



lda.f1 <- qda.f1 <- nb.f1 <- matrix(NA,K,1)

for(k in 1:K){
  tran <- which(index[,k]=="tr")
  test <- which(index[,k]=="te")
  
  testset <- data[test,]
  y <- data[,1]
  x <- data[,2:18]
  
  ##LDA
  lda.tran <- lda(Private ~., data=data, subset = tran)
  lda.pred <- predict(lda.tran,x)$class[-tran]
  lda.f1[k,1] <- fun.f1(lda.pred,testset)
  
  ##QDA
  qda.tran <- qda(Private ~., data=data, subset=tran)
  qda.pred <- predict(qda.tran,x)$class[-tran]
  qda.f1[k,1] <- fun.f1(qda.pred,testset)
  ##NB
  nb.tran <- naiveBayes(Private ~., data=data, subset=tran)
  nb.pred <- predict(nb.tran,x)[-tran]
  nb.f1[k,1] <- fun.f1(nb.pred,testset)
}

q2_lda.f1 <- mean(lda.f1)
q2_qda.f1 <- mean(qda.f1)
q2_nb.f1 <- mean(nb.f1)

q2_1 <- cbind(q2_lda.acc,q2_qda.acc,q2_nb.acc)
q2_2 <- cbind(q2_lda.mcc,q2_qda.mcc,q2_nb.mcc)
q2_3 <- cbind(q2_lda.f1,q2_qda.f1,q2_nb.f1)
q2 <- cbind(t(q2_1),t(q2_2),t(q2_3))
rownames(q2) <- c("LDA","QDA","NB")
colnames(q2) <- c("ACC","MCC","F1")
q2

#######################문제3#################################

library(tree)
tree.acc <- tree.mcc <- tree.f1 <- matrix(NA,K,1)

for(k in 1:K){
  tran <- which(index[,k]=="tr")
  test <- which(index[,k]=="te")
  
  testset <- data[test,]
  y <- data[,1]
  x <- data[,2:18]
  
  tree.tran <- tree(Private ~., data=data, subset = tran)
  tree.pred.pro <- predict(tree.tran,x)[-tran,2]
  tree.pred <- ifelse(tree.pred.pro>=0.5, "Yes" ,"No")
   
  tree.acc[k,1] <- fun.acc(tree.pred,testset)
  tree.mcc[k,1] <- fun.mcc(tree.pred,testset)
  tree.f1[k,1] <- fun.f1(tree.pred,testset)
}

q3_tree.acc <- mean(tree.acc)
q3_tree.mcc <- mean(tree.mcc)
q3_tree.f1 <- mean(tree.f1)


q3 <- cbind(q3_tree.acc,q3_tree.mcc,q3_tree.f1)
rownames(q3) <- c("Classification Tree")
colnames(q3) <- c("ACC","MCC","F1")
q3

#######################문제4#################################
RNGkind(sample.kind = "Rounding")
set.seed(4444)
cv.id <- rep(seq(10), 60)
cv.index <- matrix(cv.id, length(cv.id), 100)
cv.index <- apply(cv.index, 2, sample)

tree.acc.cv <- tree.mcc.cv <- tree.f1.cv <- matrix(NA,K,1)

for(k in 1:K){
  tran <- which(index[,k]=="tr")
  test <- which(index[,k]=="te")
  
  testset <- data[test,]
  y <- data[,1]
  x <- data[,2:18]

    tree.tran <- tree(Private ~., data=data, subset = tran)
  cv.g <- cv.tree(tree.tran, K=10, rand=cv.index[,k], FUN=prune.misclass)
  w <- max(cv.g$size[which(min(cv.g$dev)==cv.g$dev)])

  prune.tree <- prune.misclass(tree.tran, best=w)
  tree.pred.cv.pro <- predict(prune.tree,x)[-tran,2]
  tree.pred.cv <- ifelse(tree.pred.cv.pro>=0.5, "Yes" ,"No")

  tree.acc.cv[k,1] <- fun.acc(tree.pred.cv,testset)
  tree.mcc.cv[k,1] <- fun.mcc(tree.pred.cv,testset)
  tree.f1.cv[k,1] <- fun.f1(tree.pred.cv,testset)  
}

q4_tree.acc.cv <- mean(tree.acc.cv)
q4_tree.mcc.cv <- mean(tree.mcc.cv)
q4_tree.f1.cv <- mean(tree.f1.cv)


q4 <- cbind(q4_tree.acc.cv,q4_tree.mcc.cv,q4_tree.f1.cv)
rownames(q4) <- c("Cross Validated Tree")
colnames(q4) <- c("ACC","MCC","F1")
q4


#######################문제5#################################
RNGkind(sample.kind = "Rounding")
set.seed(5555)
bt.index <- array(0, c(600, 300, 100))
for (t in 1:100) {
  for (b in 1:300) {
    u <- unique(sample(1:600, replace=TRUE))
    bt.index[u, b, t] <- 1
  }
}

B <- dim(bt.index)[2]
bag.acc <- bag.mcc <- bag.f1 <- matrix(NA,K,1)

for(k in 1:100){
  tran <- which(index[,k]=="tr")
  test <- which(index[,k]=="te")
  Vote <- rep(0, length(test))
  
  trainset <- data[tran,]
  testset <- data[test,]
  y <- data[,1]
  x <- data[,2:18]
  
  for (i in 1:B) {
    num <- which(bt.index[,i,k]==T)
    
    tree_model <- tree(Private ~., data=trainset, subset=num)
    tree_pred.pro <- predict(tree_model, data)[test,2]
    tree_pred <- ifelse(tree_pred.pro>=0.5, "Yes" ,"No")
    
    Vote[tree_pred=="Yes"] <- Vote[tree_pred=="Yes"] + 1
  }
  
  preds <- rep("Yes", length(test))
  preds[Vote < 150] <- "No"
  
  bag.acc[k,1] <- fun.acc(preds,testset)
  bag.mcc[k,1] <- fun.mcc(preds,testset)
  bag.f1[k,1] <- fun.f1(preds,testset) 
}



q5_bag.acc <- mean(bag.acc)
q5_bag.mcc <- mean(bag.mcc)
q5_bag.f1 <- mean(bag.f1)

q5 <- cbind(q5_bag.acc,q5_bag.mcc,q5_bag.f1)
rownames(q5) <- c("Bagging Tree")
colnames(q5) <- c("ACC","MCC","F1")
q5







#######################문제6#################################
#install.packages('randomForest')
library(randomForest)

y <- data[,1]
y.num <- rep(0,length(y))
y.num[y=='Yes'] <- 1

alpha=0.5
K <- dim(index)[2]


RNGkind(sample.kind = "Rounding")
set.seed(4444)
cv.id <- rep(seq(10), 60)
cv.index <- matrix(cv.id, length(cv.id), 100)
cv.index <- apply(cv.index, 2, sample)

rf.acc <- rf.mcc <- rf.f1 <- matrix(NA,K,1)
rf.mis <- rep(NA,10)
rf.mismean <- rep(NA,6)

for(k in 1:K){
  tran <- which(index[,k]=="tr")
  test <- which(index[,k]=="te")
  trainset <- data[tran,]
  testset <- data[-tran,]
  
  for (j in 1:6){
    for (i in 1:10){
      tran.cv <- which(cv.index[,k]!=i)
      tran_y <- trainset[,1]
      tran_x <- trainset[,2:18]
      
      rf.tran <- randomForest(Private ~., data=trainset, subset = tran.cv, mtry=j, ntree = 1000)
      rf.pred.pro <- predict(rf.tran,tran_x,type = "prob")[-tran.cv,2]
      rf.pred <- ifelse(rf.pred.pro>=0.5, "Yes" ,"No")
      rf.mis[i] <- 1 - fun.acc(rf.pred,trainset[-tran.cv,])
      }
    rf.mismean[j] <- mean(rf.mis)
    }
  m <- min(which(min(rf.mismean)==rf.mismean))
  
  rf.tran <- randomForest(Private ~., data=data, subset = tran, mtry=m, ntree = 1000)
  rf.pred.pro <- predict(rf.tran,x,type = "prob")[-tran,2]
  rf.pred <- ifelse(rf.pred.pro>=0.5, "Yes" ,"No")
    
  rf.acc[k,1] <- fun.acc(rf.pred,testset)
  rf.mcc[k,1] <- fun.mcc(rf.pred,testset)
  rf.f1[k,1] <- fun.f1(rf.pred,testset) 
}

q6_rf.acc <- mean(rf.acc)
q6_rf.mcc <- mean(rf.mcc)
q6_rf.f1 <- mean(rf.f1)

q6 <- cbind(q6_rf.acc,q6_rf.mcc,q6_rf.f1)
rownames(q6) <- c("Random Forest")
colnames(q6) <- c("ACC","MCC","F1")
q6

#######################문제7#################################
q7 <- rbind(q2,q3,q4,q5,q6)
q7

q2;q3;q4;q5;q6;q7
