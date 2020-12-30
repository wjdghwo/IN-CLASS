#######################문제1#################################
#install.packages('ISLR')
library(ISLR)
data('College')
str(College)
data <- College

RNGkind(sample.kind = "Rounding")
set.seed(1234)
tran <- sample(dim(College)[1], floor(dim(College)[1]*0.7))
test <- College$Private[-tran]


g1 <- glm(Private~., family="binomial", data=College, subset=tran)
logit <- predict(g1, College, type="response")[-tran]
logit[logit >= 0.5] <- "Yes"
logit[logit < 0.5] <- "No"
pred1 <- logit
table(pred1, test)
mean(pred1!=test)

library(MASS)
g2 <- lda(Private ~., data=College, subset=tran)
pred2 <- predict(g2, College)$class[-tran]
table(pred2, test)
mean(pred2!=test)

g3 <- qda(Private~., data=College, subset=tran)
pred3 <- predict(g3, College)$class[-tran]
table(pred3, test)
mean(pred3!=test)


library(e1071)
g4 <- naiveBayes(Private ~ ., data=College, subset=tran)
pred4 <- predict(g4, College)[-tran]
table(pred4, test)
mean(pred4!=test)


#######################문제2#################################
g1 <- glm(Private~., family="binomial", data=College, subset=tran)
pred1 <- predict(g1, College, type="response")[-tran]

g2 <- lda(Private ~., data=College, subset=tran)
pred2 <- predict(g2, College)$posterior[-tran,2]

g3 <- qda(Private~., data=College, subset=tran)
pred3 <- predict(g3, College)$posterior[-tran,2]

g4 <- naiveBayes(Private ~ ., data=College, subset=tran)
pred4 <- predict(g4, College, type="raw")[-tran,2]


#install.packages("ROCR")
library(ROCR)
## Compute ROC curve
label <- factor(College$Private[-tran],levels=c("Yes","No"),labels=c("TRUE","FALSE"))
preds <- prediction(pred1, label) # ROC curve 계산
perf <- performance(preds, "tpr", "fpr") #ROC curve 함수
AUC1 <- performance(preds, "auc")@y.values # AUC값 구하기
plot(perf, lwd=2, col="red", lty=1)

par(new=TRUE)
preds <- prediction(pred2, label) # ROC curve 계산
perf <- performance(preds, "tpr", "fpr") #ROC curve 함수
AUC2 <- performance(preds, "auc")@y.values # AUC값 구하기
plot(perf, lwd=2, col="orange", lty=2)

par(new=TRUE)
preds <- prediction(pred3, label) # ROC curve 계산
perf <- performance(preds, "tpr", "fpr") #ROC curve 함수
AUC3 <- performance(preds, "auc")@y.values # AUC값 구하기
plot(perf, lwd=2, col="darkgreen", lty=3)

par(new=TRUE)
preds <- prediction(pred4, label) # ROC curve 계산
perf <- performance(preds, "tpr", "fpr") #ROC curve 함수
AUC4 <- performance(preds, "auc")@y.values # AUC값 구하기
plot(perf, lwd=2, col="darkblue", lty=4)

abline(a=0, b=1, lty=2)
labels <- c(paste("LR :", AUC1), paste("LDA :", AUC2), paste("QDA :", AUC3), paste("NB :", AUC4))
cols <- c("red", "orange", "darkgreen", "darkblue")

legend("bottomright", legend = labels, lwd=2, col = cols, lty = 1:4)

cbind(AUC1,AUC2,AUC3,AUC4)
#######################문제3#################################
RNGkind(sample.kind = "Rounding")
set.seed(12345)
N.lab <- sample(rep(seq(10), length=sum(College$Private[tran]=="No")))
Y.lab <- sample(rep(seq(10), length=sum(College$Private[tran]=="Yes")))
gr <- rep(0, length(tran))
gr[College$Private[tran]=="No"] <- N.lab
gr[College$Private[tran]=="Yes"] <- Y.lab

thre <- seq(0,1,0.01)
pred1 <- pred2 <-pred3 <-pred4 <-NULL
mis <- NULL
K <- 10
mis <- matrix(NA,length(thre),K)
misclass <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g1 <- glm(Private~., family="binomial", data=datafold, subset=tranfold)
    logit <- predict(g1, datafold, type="response")[-tranfold]
    logit[logit >= thre[i]] <- "Yes"
    logit[logit < thre[i]] <- "No"
    pred1 <- logit
    mis[i,k] <- mean(pred1!=testfold)
  }
  misclass[i] <- mean(mis[i,])
}
min(misclass)
which(misclass==min(misclass))
thre[which(misclass==min(misclass))]
a <- thre[which(misclass==min(misclass))]


g1 <- glm(Private~., family="binomial", data=College, subset=tran)
logit <- predict(g1, College, type="response")[-tran]
logit[logit >= a] <- "Yes"
logit[logit < a] <- "No"
pred1 <- logit
table(pred1, test)
mean(pred1!=test)


mis <- matrix(NA,length(thre),K)
misclass <- NULL
for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g2 <- lda(Private ~., data=datafold, subset=tranfold)
    pred2 <- predict(g2, datafold)$posterior[-tranfold,2]
    pred2[pred2 >= thre[i]] <- "Yes"
    pred2[pred2 < thre[i]] <- "No"
    mis[i,k] <- mean(pred2!=testfold)
  }
  misclass[i] <- mean(mis[i,])
}
min(misclass)
which(misclass==min(misclass))
thre[which(misclass==min(misclass))]
a <- mean(thre[which(misclass==min(misclass))])
a

g2 <- lda(Private ~., data=College, subset=tran)
pred2 <- predict(g2, College)$posterior[-tran,2]
pred2[pred2 >= a] <- "Yes"
pred2[pred2 < a] <- "No"
table(pred2, test)
mean(pred2!=test)

mis <- matrix(NA,length(thre),K)
misclass <- NULL
for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g3 <- qda(Private ~., data=datafold, subset=tranfold)
    pred3 <- predict(g3, datafold)$posterior[-tranfold,2]
    pred3[pred3 >= thre[i]] <- "Yes"
    pred3[pred3 < thre[i]] <- "No"
    mis[i,k] <- mean(pred3!=testfold)
  }
  misclass[i] <- mean(mis[i,])
}
min(misclass)
which(misclass==min(misclass))
thre[which(misclass==min(misclass))]
a <- mean(thre[which(misclass==min(misclass))])
a

g3 <- qda(Private ~., data=College, subset=tran)
pred3 <- predict(g3, College)$posterior[-tran,2]
pred3 <- ifelse(pred3>=a, "Yes", "No")
table(pred3, test)
mean(pred3!=test)


mis <- matrix(NA,length(thre),K)
misclass <- NULL
for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g4 <- naiveBayes(Private ~ ., data=datafold, subset=tranfold)
    pred4 <- predict(g4, datafold, type="raw")[-tranfold,2]
    pred4[pred4 >= thre[i]] <- "Yes"
    pred4[pred4 < thre[i]] <- "No"
    mis[i,k] <- mean(pred4!=testfold)
  }
  misclass[i] <- mean(mis[i,])
}
min(misclass)
which(misclass==min(misclass))
thre[which(misclass==min(misclass))]
a <- mean(thre[which(misclass==min(misclass))])
a

g4 <- naiveBayes(Private ~ ., data=College, subset=tran)
pred4 <- predict(g4, College, type="raw")[-tran,2]
pred4 <- ifelse(pred4>=a, "Yes", "No")
table(pred4, test)
mean(pred4!=test)

#######################문제4#################################
thre <- seq(0,1,0.01)
pred1 <- pred2 <-pred3 <-pred4 <-NULL
K <- 10
RES <- matrix(NA, length(thre), 4)
MCC <- matrix(NA,length(thre),K)
MCC_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g1 <- glm(Private~., family="binomial", data=datafold, subset=tranfold)
    logit <- predict(g1, datafold, type="response")[-tranfold]
    logit <- ifelse(logit>=thre[i], "Yes", "No")
    pred1 <- logit
    RES[i,1] <- sum(pred1[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred1[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred1=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN

    MCC[i,k] <- ((RES[i,1]*RES[i,2])-(RES[i,3]*RES[i,4]))/sqrt(
      (RES[i,1]+RES[i,3])*(RES[i,1]+RES[i,4])*(RES[i,2]+RES[i,3])*(RES[i,2]+RES[i,4]))
  }
  MCC_[i] <- mean(MCC[i,])
  MCC_[is.na(MCC_)] <- 0
}

max(MCC_)
which(MCC_==max(MCC_))
thre[which(MCC_==max(MCC_))]
a <- mean(thre[which(MCC_==max(MCC_))])
a

g1 <- glm(Private~., family="binomial", data=College, subset=tran)
logit <- predict(g1, College, type="response")[-tran]
logit <- ifelse(logit>=a, "Yes", "No")
pred1 <- logit
RES <- NULL
RES[1] <- sum(pred1[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred1[test=="No"] == "No")#TN
RES[3] <- sum(pred1=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

((RES[1]*RES[2])-(RES[3]*RES[4]))/sqrt(
  (RES[1]+RES[3])*(RES[1]+RES[4])*(RES[2]+RES[3])*(RES[2]+RES[4]))

RES <- matrix(NA, length(thre), 4)
MCC <- matrix(NA,length(thre),K)
MCC_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g2 <- lda(Private ~., data=datafold, subset=tranfold)
    pred2 <- predict(g2, datafold)$posterior[-tranfold,2]
    pred2 <- ifelse(pred2>=thre[i], "Yes", "No")
    RES[i,1] <- sum(pred2[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred2[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred2=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN
    
    MCC[i,k] <- ((RES[i,1]*RES[i,2])-(RES[i,3]*RES[i,4]))/sqrt(
      (RES[i,1]+RES[i,3])*(RES[i,1]+RES[i,4])*(RES[i,2]+RES[i,3])*(RES[i,2]+RES[i,4]))
  }
  MCC_[i] <- mean(MCC[i,])
  MCC_[is.na(MCC_)] <- 0
}

max(MCC_)
which(MCC_==max(MCC_))
thre[which(MCC_==max(MCC_))]
a <- mean(thre[which(MCC_==max(MCC_))])
a

g2 <- lda(Private ~., data=College, subset=tran)
pred2 <- predict(g2, College)$posterior[-tran,2]
pred2 <- ifelse(pred2>=a, "Yes", "No")
RES <- NULL
RES[1] <- sum(pred2[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred2[test=="No"] == "No")#TN
RES[3] <- sum(pred2=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

((RES[1]*RES[2])-(RES[3]*RES[4]))/sqrt(
  (RES[1]+RES[3])*(RES[1]+RES[4])*(RES[2]+RES[3])*(RES[2]+RES[4]))


RES <- matrix(NA, length(thre), 4)
MCC <- matrix(NA,length(thre),K)
MCC_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g3 <- qda(Private ~., data=datafold, subset=tranfold)
    pred3 <- predict(g3, datafold)$posterior[-tranfold,2]
    pred3 <- ifelse(pred3>=thre[i], "Yes", "No")
    RES[i,1] <- sum(pred3[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred3[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred3=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN
    
    MCC[i,k] <- ((RES[i,1]*RES[i,2])-(RES[i,3]*RES[i,4]))/sqrt(
      (RES[i,1]+RES[i,3])*(RES[i,1]+RES[i,4])*(RES[i,2]+RES[i,3])*(RES[i,2]+RES[i,4]))
  }
  MCC_[i] <- mean(MCC[i,])
  MCC_[is.na(MCC_)] <- 0
}

max(MCC_)
which(MCC_==max(MCC_))
thre[which(MCC_==max(MCC_))]
a <- mean(thre[which(MCC_==max(MCC_))])
a

g3 <- qda(Private ~., data=College, subset=tran)
pred3 <- predict(g3, College)$posterior[-tran,2]
pred3 <- ifelse(pred3>=a, "Yes", "No")
RES <- NULL
RES[1] <- sum(pred3[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred3[test=="No"] == "No")#TN
RES[3] <- sum(pred3=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

((RES[1]*RES[2])-(RES[3]*RES[4]))/sqrt(
  (RES[1]+RES[3])*(RES[1]+RES[4])*(RES[2]+RES[3])*(RES[2]+RES[4]))


RES <- matrix(NA, length(thre), 4)
MCC <- matrix(NA,length(thre),K)
MCC_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g4 <- naiveBayes(Private ~ ., data=datafold, subset=tranfold)
    pred4 <- predict(g4, datafold, type="raw")[-tranfold,2]
    pred4 <- ifelse(pred4>=thre[i], "Yes", "No")
    RES[i,1] <- sum(pred4[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred4[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred4=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN
    
    MCC[i,k] <- ((RES[i,1]*RES[i,2])-(RES[i,3]*RES[i,4]))/sqrt(
      (RES[i,1]+RES[i,3])*(RES[i,1]+RES[i,4])*(RES[i,2]+RES[i,3])*(RES[i,2]+RES[i,4]))
  }
  MCC_[i] <- mean(MCC[i,])
  MCC_[is.na(MCC_)] <- 0
}

max(MCC_)
which(MCC_==max(MCC_))
thre[which(MCC_==max(MCC_))]
a <- mean(thre[which(MCC_==max(MCC_))])
a


g4 <- naiveBayes(Private ~ ., data=College, subset=tran)
pred4 <- predict(g4, College, type="raw")[-tran,2]
pred4 <- ifelse(pred4>=a, "Yes", "No")
RES <- NULL
RES[1] <- sum(pred4[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred4[test=="No"] == "No")#TN
RES[3] <- sum(pred4=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

((RES[1]*RES[2])-(RES[3]*RES[4]))/sqrt(
  (RES[1]+RES[3])*(RES[1]+RES[4])*(RES[2]+RES[3])*(RES[2]+RES[4]))

#######################문제5#################################
thre <- seq(0,1,0.01)
pred1 <- pred2 <-pred3 <-pred4 <-NULL
K <- 10
RES <- matrix(NA, length(thre), 4)
F1 <- matrix(NA,length(thre),K)
F1_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g1 <- glm(Private~., family="binomial", data=datafold, subset=tranfold)
    logit <- predict(g1, datafold, type="response")[-tranfold]
    logit <- ifelse(logit>=thre[i], "Yes", "No")
    pred1 <- logit
    RES[i,1] <- sum(pred1[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred1[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred1=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN
    
    F1[i,k] <- (2*RES[i,1])/(2*RES[i,1]+RES[i,3]+RES[i,4])
  }
  F1_[i] <- mean(F1[i,])
  F1_[is.na(F1_)] <- 0
}

max(F1_)
which(F1_==max(F1_))
thre[which(F1_==max(F1_))]
a <- mean(thre[which(F1_==max(F1_))])
a

g1 <- glm(Private~., family="binomial", data=College, subset=tran)
logit <- predict(g1, College, type="response")[-tran]
logit <- ifelse(logit>=a, "Yes", "No")
pred1 <- logit
RES <- NULL
RES[1] <- sum(pred1[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred1[test=="No"] == "No")#TN
RES[3] <- sum(pred1=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

(2*RES[1])/(2*RES[1]+RES[3]+RES[4])

RES <- matrix(NA, length(thre), 4)
F1 <- matrix(NA,length(thre),K)
F1_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g2 <- lda(Private ~., data=datafold, subset=tranfold)
    pred2 <- predict(g2, datafold)$posterior[-tranfold,2]
    pred2 <- ifelse(pred2>=thre[i], "Yes", "No")
    RES[i,1] <- sum(pred2[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred2[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred2=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN
    
    F1[i,k] <- (2*RES[i,1])/(2*RES[i,1]+RES[i,3]+RES[i,4])
  }
  F1_[i] <- mean(F1[i,])
  F1_[is.na(F1_)] <- 0
}

max(F1_)
which(F1_==max(F1_))
thre[which(F1_==max(F1_))]
a <- mean(thre[which(F1_==max(F1_))])
a

g2 <- lda(Private ~., data=College, subset=tran)
pred2 <- predict(g2, College)$posterior[-tran,2]
pred2 <- ifelse(pred2>=a, "Yes", "No")
RES <- NULL
RES[1] <- sum(pred2[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred2[test=="No"] == "No")#TN
RES[3] <- sum(pred2=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

(2*RES[1])/(2*RES[1]+RES[3]+RES[4])


RES <- matrix(NA, length(thre), 4)
F1 <- matrix(NA,length(thre),K)
F1_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g3 <- qda(Private ~., data=datafold, subset=tranfold)
    pred3 <- predict(g3, datafold)$posterior[-tranfold,2]
    pred3 <- ifelse(pred3>=thre[i], "Yes", "No")
    RES[i,1] <- sum(pred3[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred3[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred3=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN
    
    F1[i,k] <- (2*RES[i,1])/(2*RES[i,1]+RES[i,3]+RES[i,4])
  }
  F1_[i] <- mean(F1[i,])
  F1_[is.na(F1_)] <- 0
}

max(F1_)
which(F1_==max(F1_))
thre[which(F1_==max(F1_))]
a <- mean(thre[which(F1_==max(F1_))])
a

g3 <- qda(Private ~., data=College, subset=tran)
pred3 <- predict(g3, College)$posterior[-tran,2]
pred3 <- ifelse(pred3>=a, "Yes", "No")
RES <- NULL
RES[1] <- sum(pred3[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred3[test=="No"] == "No")#TN
RES[3] <- sum(pred3=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

(2*RES[1])/(2*RES[1]+RES[3]+RES[4])



RES <- matrix(NA, length(thre), 4)
MCC <- matrix(NA,length(thre),K)
MCC_ <- NULL

for (i in 1:length(thre)) {
  for (k in 1:K) {
    tranfold <- which(gr!=k)
    datafold <- College[tran,]
    testfold <- datafold$Private[-tranfold]
    
    g4 <- naiveBayes(Private ~ ., data=datafold, subset=tranfold)
    pred4 <- predict(g4, datafold, type="raw")[-tranfold,2]
    pred4 <- ifelse(pred4>=thre[i], "Yes", "No")
    RES[i,1] <- sum(pred4[testfold=="Yes"] == "Yes")#TP
    RES[i,2] <- sum(pred4[testfold=="No"] == "No")#TN
    RES[i,3] <- sum(pred4=="Yes") - RES[i,1]#FP
    RES[i,4] <- sum(testfold=="Yes") - RES[i,1]#FN
    
    F1[i,k] <- (2*RES[i,1])/(2*RES[i,1]+RES[i,3]+RES[i,4])
  }
  F1_[i] <- mean(F1[i,])
  F1_[is.na(F1_)] <- 0
}

max(F1_)
which(F1_==max(F1_))
thre[which(F1_==max(F1_))]
a <- mean(thre[which(F1_==max(F1_))])
a


g4 <- naiveBayes(Private ~ ., data=College, subset=tran)
pred4 <- predict(g4, College, type="raw")[-tran,2]
pred4 <- ifelse(pred4>=a, "Yes", "No")
RES <- NULL
RES[1] <- sum(pred4[test=="Yes"] == "Yes")#TP
RES[2] <- sum(pred4[test=="No"] == "No")#TN
RES[3] <- sum(pred4=="Yes") - RES[1]#FP
RES[4] <- sum(test=="Yes") - RES[1]#FN

(2*RES[1])/(2*RES[1]+RES[3]+RES[4])
