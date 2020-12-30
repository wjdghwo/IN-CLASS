#수업 주요 내용 
#1. 다변량정규분포 가정에 대한 검정
#2. 공분산행렬의 동질성검정 
#3. LDA 와 QDA 

# DA 수행단계

# step1 : 각 군집이 다변량 정규성을 따르는지 검정 -> o : 공분산 행렬의 동질성 검정 실시(다변량 정규성 무조건 따라야함)
#                                                 -> x : 피셔 선형판별분석 (공분산 행렬이 동질성이 만족했을때)
# step2 : 공분산 행렬의 동질성 검정               -> o : LDA
#                                                 -> x : QDA

# step3 : 오분류율 계산 



## 1.붓꽃 자료에 대한 예제 ##
#[보기 6.3.4] 붓꽃 자료의 다변량정규성 검정#
#[R-코드 6.3.4]- (setosaversicolor-mardiaTest- QDA.R)#
# install.packages("MASS")
# install.packages("MVN")
# install.packages("haven") # MVN 오류시
library(MASS)
# library(haven) #MVN 오류시
library(MVN)
setwd("E:/학교/2020 2학기 정호재/다변량통계학2/20200917/Rdata(all)")
setosa_versi<-read.table("setosaversicolor.txt", header=T)
attach(setosa_versi)
head(setosa_versi) # 변수는 총 3가지/종류가 타겟변수
str(setosa_versi)
unique(setosa_versi[,3]) #타겟값 보기/unique 중복없이 유일한 값만 추출

# MVN tests based on the Skewness and Kurtosis Statistics 
setosa=setosa_versi[1:50, 1:2]
versicolor=setosa_versi[51:100, 1:2] # 각각 종류별로 데이터 분리
setosa
versicolor
result_setosa = mvn(setosa)
result_versicolor = mvn(versicolor) # 다변량 정규성 검정
result_setosa
result_versicolor

# pca 분류규칙에 필요한것
# 1. 분포
# 2. 오분류 비용 (그룹 a,b가있을때 원래 a인데 b로 선택될때의 오류비용):  -그룹간 동일하다고 가정
# 3. 사전확률 (기존에 있었던 연구결과로 인해서 적용하는 확률)-그룹간 동일하다고 가정
# 실제로 알기가 어렵기 때문에 오분류 비용과 사전확률은 그룹간 동일하다고 가정
# pca 분류규칙에서 함수의 우도로 문류를 하기 때문에 다변량 정규성이 만족해야함

# term project때 다변량 정규성을 안따른는 경우 왜도가 다변량 정규성을때는 첨도가 아니더라도 왜도만 만족해도 다변량 정규성이 따른다고 가정가능(Term project시) 

#[보기 6.3.1] 두 종류 붓꽃 자료의 공분산행렬의 동질성#
# Box’s test for the hypothesis test of the equality of at least
#two covariance matrices in Mardia et al.(1979, p. 140).
library(MASS)
setosa_versi<-read.table("setosaversicolor.txt", header=T)
attach(setosa_versi)
group=as.factor(종류) #섹터 별로 저장
group 

# Scatter Plot

plot(setosa_versi[, 1:2], pch=unclass(종류),col = c('blue','red')[group])
# 산점도 결과 분류가 잘될꺼같음 한선으 조금 제거해야할수도
# LDA 그룹간 분류를 해주는 함수식을 찾는것 (1차)
# 2차는 QDA


# Density function of Variables
?ldahist
par(mfrow=c(1,2))
ldahist(data= 꽃받침길이, g=종류, type="density") # 확률밀도함수를 그려주는 함수
ldahist(data= 꽃받침폭, g=종류, type="density") 
#꽃받침 길이는 분산의 차이가 있어보이지만 꽃받침폭은 분산에 차이가 없어보인다.

#공분산행렬 동질성 검정
cov.Mtest=function(x,ina,a=0.05){ 
  ## x is the *data set*
  ## ina is a *numeric vector* indicating the groups of the data set #그룹데이터를 numeric vector형태로 변환해서 사용
  ## a is the significance level, set to 0.05 by default
  x=as.matrix(x)
  p=ncol(x) ## dimension of the data set
  n=nrow(x) ## total sample size
  k=max(ina) ## number of groups
  nu=rep(0,k) ## the sample size of each group will be stored here later
  pame=rep(0,k) ## the determinant of each covariance will be stored here
  ## the next "for" function calculates the covariance matrix of each group
  nu=as.vector(table(ina))
  mat=mat1=array(dim=c(p,p,k))
  for (i in 1:k) {
    mat[,,i]=cov(x[ina==i,])
    pame[i]=det(mat[,,i]) ## the detemirnant of each covariance matrix
    mat1[,,i]=(nu[i]-1)*cov(x[ina==i,]) }
  ## the next 2 lines calculate the pooled covariance matrix
  Sp=apply(mat1,1:2,sum)
  Sp=Sp/(n-k)
  for (i in 1:k)
    pamela=det(Sp) ## determinant of the pooled covariance matrix
  test1=sum((nu-1)*log(pamela/pame))
  gama1=(2*(p^2)+3*p-1)/(6*(p+1)*(k-1))
  gama2=(sum(1/(nu-1))-1/(n-k))
  gama=1-gama1*gama2
  
  test=gama*test1 ## this is the M (test statistic)
  df=0.5*p*(p+1)*(k-1) ## degrees of freedom of the chi-square distribution
  pvalue=1-pchisq(test,df) ## p-value of the test statistic
  crit=qchisq(1-a,df) ## critical value of the chi-square distribution
  list(M.test=test,degrees=df,critical=crit,p.value=pvalue) }

ina=as.numeric(as.factor(setosa_versi[, 3])) # 책에서는 as.factor가 빠져있음 / 문자열로 저장이 되어있으면 as.factor형태로 팩터를 넣어주어야함

head(setosa_versi)
x=setosa_versi[, 1:2]
cov.Mtest(x, ina)
#p.value가 0.05보다 작으므로 귀무가설기각 분산이 동질하지 않음



#공분산행렬 동질성 검정(내장함수) 
library(biotools)
boxM(x, as.factor(setosa_versi[, 3])) # 데이터 및 그룹변수를 펙터형태로 넣음
?boxM
#p.value가 0.05보다 작으므로 귀무가설기각 분산이 동질하지 않음

# 위의 함수와 반올림의 차이만 있을뿐 값이 같게 나옴
# 과제나 텀프때 boxM사용가능

#[보기 6.3.4] 다변량 정규성만족,공분산행렬 동질성 성립 하지 않으므로  QDA 실시 #
# Quadratic DA with 2 groups applying 
# resubstitution prediction and equal prior probabilities.
QDA=qda(종류~꽃받침길이+꽃받침폭, data=setosa_versi, prior=c(1,1)/2) # prior사전확률, 디폴트 값이 동일하기에 모르는 경우적지않아도됨
QDA # 위에서 학습한 qda모델

qcluster=predict(QDA, setosa_versi)$class # predict(모델, 예측할 데이터)/ #qda로 예측한 결과 그룹 저장
#predict(QDA, setosa_versi)-posterior: 그 그룹일 확률/class:나눠진 그룹

qct=table(종류, qcluster) #실제 데이터의 타겟변수와 예측한 데이터를 table형태로 비교
qct
#하나의 데이터가 setosa인데 versicolor로 오분류됨

# Total percent correct
mean(종류==qcluster)
#99%로 높은 예측률 보임
# 훈련할 데이터에 전체데이터를 넣었기에 즉 test데이터가 없어서 높은 예측률을 보인걸수도 있음

detach(setosa_versi) #데이터를 R 검색 경로에서 제거한다.




## 2.예초기 자료에 대한 예제##
#[보기 6.3.3] 예초기 자료의 다변량정규성 검정#
#[R-코드 6.3.3] - (ridingmower-Mardiatest-LDA.R))#
# MVD test and Linear DA for two groups(owner, nonowner)
# of riding mower on two variables(x1.Income x2.Lotsize) 
library(MASS)
library(MVN)

ridingmower<-read.table("ridingmower.txt", header=T)
attach(ridingmower)
group=as.factor(ridingmower[,1])
group

head(ridingmower) # 타겟값 예초기를 가지는지 아닌지

# Scatter Plot
plot(ridingmower[, 2:3], pch=unclass(pop),col = c('blue','red')[group] )
# 완벽하게 분류되기 힘들어보임

# Density function of Variables
par(mfrow=c(1,2))
ldahist(data= x1.Income, g=pop, width=30, type="density")
ldahist(data= x2.Lotsize, g=pop, width=10, type="density")
#lotsize는 등분산성 정규성 만족할꺼 처럼보임
#incomdms 잘 모르겠음 검정결과로 확인


# MVN tests based on the Skewness and Kurtosis Statistics
owner=ridingmower[1:12, 2:3] #예초기 소유
nonowner=ridingmower[13:24, 2:3] #예초기 미소유
owner
nonowner
result_owner = mvn(owner)
result_nonowner = mvn(nonowner)
result_owner #다변량 정규성 만족
result_nonowner #다변량 정규성 만족



#[보기 6.3.2] 예초기 소유 자료의 공분산행렬의 동질성#
#[R-코드 6.3.2]- (ridingmower-Boxtest.R)#
# Box’s test for the hypothesis test of the equality of at least
#two covariance matrices in Mardia et al.(1979, p. 140).

cov.Mtest=function(x,ina,a=0.05){
  ## x is the data set
  ## ina is a numeric vector indicating the groups of the data set
  ## a is the significance level, set to 0.05 by default
  x=as.matrix(x)
  p=ncol(x) ## dimension of the data set
  n=nrow(x) ## total sample size
  k=max(ina) ## number of groups
  nu=rep(0,k) ## the sample size of each group will be stored here later
  pame=rep(0,k) ## the determinant of each covariance will be stored here
  ## the next "for" function calculates the covariance matrix of each group
  nu=as.vector(table(ina))
  mat=mat1=array(dim=c(p,p,k))
  for (i in 1:k) {
    mat[,,i]=cov(x[ina==i,])
    pame[i]=det(mat[,,i]) ## the detemirnant of each covariance matrix
    mat1[,,i]=(nu[i]-1)*cov(x[ina==i,]) }
  ## the next 2 lines calculate the pooled covariance matrix
  Sp=apply(mat1,1:2,sum)
  Sp=Sp/(n-k)
  for (i in 1:k)
    pamela=det(Sp) ## determinant of the pooled covariance matrix
  test1=sum((nu-1)*log(pamela/pame))
  gama1=(2*(p^2)+3*p-1)/(6*(p+1)*(k-1))
  gama2=(sum(1/(nu-1))-1/(n-k))
  gama=1-gama1*gama2
  test=gama*test1 ## this is the M (test statistic)
  df=0.5*p*(p+1)*(k-1) ## degrees of freedom of the chi-square distribution
  pvalue=1-pchisq(test,df) ## p-value of the test statistic
  crit=qchisq(1-a,df) ## critical value of the chi-square distribution
  list(M.test=test,degrees=df,critical=crit,p.value=pvalue) }

ina=as.numeric(ridingmower[, 1])
x=ridingmower[, 2:3]
cov.Mtest(x, ina)
# 공분산행렬의 동질성 만족

library(biotools)
boxM(x, as.factor(ridingmower[, 1]))


# [보기 6.3.3] 다변량 정규성 만족, 공분산행렬 동질성 성립하므로 LDA 실시 #
# Linear DA
LDA=lda(pop~x1.Income + x2.Lotsize, data=ridingmower)
LDA
lcluster=predict(LDA, ridingmower)$class
lct=table(pop, lcluster)
lct # 종 3개 잘못분류
# Total percent correct

mean(pop==lcluster)
# 87.5%의 분류율

######################
# 오늘 - 두군집에 대한 분류
# 다음 실습 - 다군집에 대한 분류
# 과제는 다음 시간에 레포트 형식으로 제출




