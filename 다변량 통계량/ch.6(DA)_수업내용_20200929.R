#오늘 수업 : 여러 군집의 판별분석#
###세 종류 붓꽃 자료의 QDA###

# 정규성 검정
# 공분산 행렬의 동질성 검정 ㅇlda ㄴㄴqda
install.packages("MASS")
install.packages("MVN")

library("MVN")
library(biotools)

#[Step 1] Iris flower data : setosa, versicolor, virginica
iris
head(iris)
attach(iris)
dim(iris) #iris data size
pairs(iris[1:4], pch=21, bg=c("red", "green", "blue")[unclass(iris$Species)])
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
# 변수에 따라 잘 나눠지므로 판별분석하면 잘 나눠지지 않을까??
# setosa = iris[which(iris$Species =="setosa"),1:4]
versicolor = iris[51:100, 1:4] # Iris data only for versicolor and four variables
virginica = iris[101:150, 1:4] # Iris data only for virginica and four variables


#세 군집의 평균벡터
colMeans(setosa)
colMeans(versicolor)
colMeans(virginica)
 
# 첫번째 변수에서 차이가 있고 군집별로 차이가 있음을 알수있다.

#[Step 2] MVN tests based on the Skewness and Kurtosis Ststistics
#그룹별로 첨도와 왜도를 통해 다변량 정규성이 만족하는지 확인
result_setosa = mvn(setosa)
result_versicolor = mvn(versicolor)
result_virginica = mvn(virginica)
list(result_setosa, result_versicolor, result_virginica)

#공분산 행렬
S1=cov(setosa)
S2=cov(versicolor)
S3=cov(virginica)
#합동공분산 행렬
n1<- dim(setosa)[1]
n2<- dim(versicolor)[1]
n3<- dim(virginica)[1]

boxM(iris[,-5], iris[,5])

Sp=((n1-1)*S1+(n2-1)*S2+(n3-1)*S3)/(n1+n2+n3-3) #((50-1)*S1+(50-1)*S2+(50-1)*S3)/(150-3)
list(S1, S2, S3, Sp)

# 사전확률 주어진 데이터 비율로 적용해야함 총 150개 에 대해서 150 50 51 49 -> 50/150 51/150 49/150으로 설정
# 과제를 영어로 적으면 추가점수
# 젤중요한건 텀프랑 기말

#[Step 3] Box's M-Test for Equlaity of Covariance Matrices
#공분산 행렬의 동질성 확인 
boxM(iris[, -5], iris[, 5])


# [Step 4] Quadratic DA with 3 groups applying 
# resubstitution prediction and equal prior probabilities.

QDA=qda(Species~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
        data=iris, prior=c(1,1,1)/3)


QDA=qda(Species~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
        data=iris, prior=c(n1,n2,n3)/(n1+n2+n3))

#prior= 옵션은 군집의 사전확률을 지정한다. 이 옵션이 생략될 경우 훈련용 자료의 군집 비율이 사용
qcluster=predict(QDA, iris)$class
table(Species, qcluster)

# 3개가 오분류됨
# Total percent correct
mean(Species==qcluster)

detach(iris)


###세 종류 와인 자료의 QDA###
install.packages("HDclassif")
library(HDclassif)
library("MVN")
library(biotools)

#[Step 1] Wine data : clusters 1, 2, 3
data(wine)
attach(wine)
wine 
# class는 지역별로 차이
dim(wine) # wine data size 
# 변수가 많아서 눈으로 알아보기 어려움
pairs(wine[2:14], pch=21, bg=c("red", "green", "blue")[unclass(wine$class)])
c1 = wine[1:59, 2:14] #59개
c2 = wine[60:130,2:14] #71개
c3 = wine[131:178,2:14] #48개
colMeans(c1)
colMeans(c2)
colMeans(c3)
# 평균이 차이가 나는 변수들 존재 

n1=dim(c1)[1]
n2=dim(c2)[1]
n3=dim(c3)[1]

#[Step 2] MVN tests based on the Skewness and Kurtosis Ststistics
result_c1 = mvn(c1)
result_c2 = mvn(c2)
result_c3 = mvn(c3)
list(result_c1,result_c2,result_c3) 

# 첫번째 세번째는 다변량 정규성 만족 / 두번째는 다변량 정규성 불만족
# 두 그룹이 다변량 정규성을 따르므로 전체적으로 다변량 정규성을 따른다고 가정하고 검정 시작

#공분산 행렬
S1=cov(c1);S2=cov(c2);S3=cov(c3)
#합동 공분산 행렬
Sp=(58*S1+70*S2+47*S3)/(178-3) #((59-1)*S1+(71-1)*S2+(48-1)*S3)/(178-3)

list(S1, S2, S3, Sp)

#[Step 3] Box's M-Test for Equlaity of Covariance Matrices
boxM(wine[, -1], wine[, 1])
# 공분산행렬의 동질성을 따르지않는다. 귀무가설 기각

#[Step 4] Quadratic DA with 3 groups applying 
# resubstitution prediction and equal prior probabilities.
QDA=qda(class~., data=wine, prior=c(1,1,1)/3)
QDA=qda(class~., data=wine, prior=c(n1,n2,n3)/(n1+n2+n3))


qcluster=predict(QDA, wine)$class
table(class, qcluster)
# 1개 오분류

# Total percent correct
mean(class==qcluster)
# 매우 높은 예측률

detach(wine)

#예초기 자료와 붓꽃 자료의 DA에서 CVM(교차타당성방법)에 의한 기대오분류율#
# CVM based on the holdout(jacknife)prediction : Evaluating Discriminant Function
library(MASS)
# Riding mower Data on two variables(x1.Income x2.Lotsize) 
setwd("G:/학교/2020 2학기 정호재/다변량통계학2/실습/20200929/Rdata")

ridingmower<-read.table("ridingmower.txt", header=T)
attach(ridingmower)
ridingmower
# 예초기 
# 다변량 정규성 공분산행렬 동질성 만족-> lda

# Linear DA
LDA=lda(pop~x1.Income + x2.Lotsize, data=ridingmower, CV=TRUE) # CV =교차타당성 결과 제공 
LDA_rsm=lda(pop~x1.Income + x2.Lotsize, data=ridingmower) # CV =교차타당성 결과 제공 안함
# CV=TRUE = leave one out cv를 실행 (1번 데이터를 제외한 나머지 훈련, 쭉쭉 데이터 개수만큼 반복 - 훈련및 테스트 진행)
# data가 크면 10-fold 사용

LDA  # 이미 훈련과 테스트를 마친 결과임 
# class 및 사후확률 존재
LDA_rsm

?lda

iris
LDA$class

# Confusion Table 
confusion_ridingmower=table(ridingmower$pop, LDA$class)
confusion_ridingmower


prop.table(confusion_ridingmower)
# 각 테이블에서의 확률

# Expected actual error rate : EAER 
EAER=(1-sum(diag(prop.table(confusion_ridingmower))))*100
EAER
# 오분류 퍼센트

detach(ridingmower)

# Iris flower data : setosa, versicolor, virginica
data(iris)
attach(iris)

# Quadratic DA with 3 groups applying 
# holdout(jacknife)prediction and equal prior probabilities.
QDA=qda(Species~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
        data=iris, prior=c(1,1,1)/3, CV=TRUE)

?qda

# Confusion Table 
confusion_iris=table(iris$Species, QDA$class)
# 총 4개 오분류


# Expected actual error rate : EAER
EAER=(1-sum(diag(prop.table(confusion_iris))))*100
list(confusion_iris, EAER)

detach(iris)

# 과제 cv했을때 안했을 때 비교하는 문제 있으니 지난시간 했던 실습 참고
