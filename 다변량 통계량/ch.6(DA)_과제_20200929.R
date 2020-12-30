#install.packages("HDclassif")
#install.packages("MASS")
#install.packages("MVN")
#install.packages("biotools")

library(HDclassif)
library(MASS)
library(MVN)
library(biotools)

setwd("G:/학교/2020 2학기 정호재/다변량통계학2/실습/20200929/Rdata")

admission<-read.table("admission.txt", header=T)
attach(admission)
head(admission)
dim(admission)
pairs(admission[1:2], pch=21, bg=c("red", "green", "blue")[unclass(admission$group)])
str(admission)
unique(admission[,3])
group1 = admission[which(admission$group == 1),1:2]
group2 = admission[which(admission$group == 2),1:2]
group3 = admission[which(admission$group == 3),1:2]

colMeans(group1)
colMeans(group2)
colMeans(group3)
# 각각의 변수에서 첫번째 그룹과 두, 세번째 그룹간의 차이가 극명하게 나타나고 두번째와 세번째그룹은 차이가 극명하지 않다.

result_group1 = mvn(group1)
result_group2 = mvn(group2)
result_group3 = mvn(group3)
list(result_group1, result_group2, result_group3)

# 첫번째 두번째는 다변량 정규성 만족 / 세번째는 다변량 정규성 불만족(왜도는 만족)
# 두 그룹이 다변량 정규성을 따르므로 전체적으로 다변량 정규성을 따른다고 가정하고 검정 시작

#공분산 행렬
S1=cov(group1)
S2=cov(group2)
S3=cov(group3)

dim(group1)
dim(group2)
dim(group3)

#합동 공분산 행렬
Sp=(30*S1+27*S2+25*S3)/(85-3) #(31-1)*S1+(28-1)*S2+(26-1)*S3)/(85-3)

list(S1, S2, S3)
Sp

library(biotools)
boxM(admission[, -3], admission[, 3])
# 공분산행렬의 동질성을 따르지 않는다. 귀무가설 기각
result_group = mvn(admission[1:2])
result_group
# 다변량 정규성 만족

# 각 그룹간의 공분산행렬들이 동일한 경우에는 선형판별함수를 사용
# 공분산 행렬의 동질성 검정 ㄴㄴqda
n1=dim(group1)[1]
n2=dim(group2)[1]
n3=dim(group3)[1]

QDA=qda(group~., data=admission, prior=c(n1,n2,n3)/(n1+n2+n3))
qcluster=predict(QDA, admission)$class
table(admission$group, qcluster)
(1-mean(admission$group==qcluster))*100


LDA=lda(group~., data=admission, prior=c(n1,n2,n3)/(n1+n2+n3))
lcluster=predict(LDA, admission)$class
table(admission$group, lcluster)
(1-mean(admission$group==lcluster))*100

# qda>lda가 더 좋은 방식

QDA=qda(group~., data=admission, prior=c(n1,n2,n3)/(n1+n2+n3), CV=TRUE)
confusion_admission=table(admission$group, QDA$class)
confusion_admission
# 총 4개 오분류


# Expected actual error rate : EAER
EAER=(1-sum(diag(prop.table(confusion_admission))))*100
list(confusion_admission, EAER)


pairs(admission[1:2], pch=21, bg=c("red", "green", "blue")[unclass(admission$group)])


# 오분류 4% 낮음
# 왠지 그래프 그려쟈이
