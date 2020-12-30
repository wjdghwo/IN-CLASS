#install.packages("MASS")
#install.packages("MVN")
#install.packages("haven")
library(MASS)
library(MVN)
setwd("D:/학교/2020 2학기 정호재/다변량통계학2/20200917/Rdata(all)")
salmon<-read.table("salmon.txt", header=T)
#attach(salmon)
head(salmon) # 변수는 총 3가지/종류가 타겟변수
str(salmon)

unique(salmon[,1]) #타겟값 보기/unique 중복없이 유일한 값만 추출

# MVN tests based on the Skewness and Kurtosis Statistics 
salmon_1=salmon[1:50, 3:4]
salmon_2=salmon[51:100, 3:4] # 각각 종류별로 데이터 분리
salmon_1
salmon_2


result_salmon_1 = mvn(salmon_1)
result_salmon_2 = mvn(salmon_2) # 다변량 정규성 검정
result_salmon_1
result_salmon_2

#################################################################
#two covariance matrices in Mardia et al.(1979, p. 140).
library(MASS)
salmon<-read.table("salmon.txt", header=T)
#attach(salmon)
group=as.factor(class) #섹터 별로 저장
group 

# Scatter Plot
par(mfrow=c(1,1))
plot(salmon[, 3:4], pch=unclass(class),col = c('blue','red')[group])
# 산점도 결과 분류가 잘될꺼같음 한선으 조금 제거해야할수도
# LDA 그룹간 분류를 해주는 함수식을 찾는것 (1차)
# 2차는 QDA


# Density function of Variables
par(mfrow=c(1,2))
ldahist(data= x1, g=class, type="density") # 확률밀도함수를 그려주는 함수
ldahist(data= x2, g=class, type="density") 
#꽃받침 길이는 분산의 차이가 있어보이지만 꽃받침폭은 분산에 차이가 없어보인다.

#공분산행렬 동질성 검정
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


ina=as.numeric(as.factor(salmon[, 1])) # 책에서는 as.factor가 빠져있음 / 문자열로 저장이 되어있으면 as.factor형태로 팩터를 넣어주어야함

x=salmon[, 3:4]
cov.Mtest(x, ina)
#p.value가 0.05보다 작으므로 귀무가설기각 분산이 동질하지 않음


#공분산행렬 동질성 검정(내장함수) 
library(biotools)
boxM(x, as.factor(salmon[, 1])) # 데이터 및 그룹변수를 펙터형태로 넣음






############################################################
#다변량 정규성만족,공분산행렬 동질성 성립 하지 않으므로  QDA 실시 #
QDA=qda(class~x1+x2, data=salmon, prior=c(1,1)/2) # prior사전확률, 디폴트 값이 동일하기에 모르는 경우적지않아도됨
QDA # 위에서 학습한 qda모델

qcluster=predict(QDA, salmon)$class # predict(모델, 예측할 데이터)/ #qda로 예측한 결과 그룹 저장
#predict(QDA, setosa_versi)-posterior: 그 그룹일 확률/class:나눠진 그룹

qct=table(class, qcluster) #실제 데이터의 타겟변수와 예측한 데이터를 table형태로 비교
qct
#하나의 데이터가 setosa인데 versicolor로 오분류됨

# Total percent correct
mean(class==qcluster)
#99%로 높은 예측률 보임
# 훈련할 데이터에 전체데이터를 넣었기에 즉 test데이터가 없어서 높은 예측률을 보인걸수도 있음



####################################################

salmon<-read.table("salmon.txt", header=T)
#attach(salmon)
head(salmon) # 변수는 총 3가지/종류가 타겟변수
str(salmon)

unique(salmon[,2]) #타겟값 보기/unique 중복없이 유일한 값만 추출

# MVN tests based on the Skewness and Kurtosis Statistics 
salmon_1=salmon[salmon$sex==1,3:4]
salmon_2=salmon[salmon$sex==2,3:4] # 각각 종류별로 데이터 분리
salmon_1
salmon_2


result_salmon_1 = mvn(salmon_1)
result_salmon_2 = mvn(salmon_2) # 다변량 정규성 검정
result_salmon_1
result_salmon_2


#two covariance matrices in Mardia et al.(1979, p. 140).
library(MASS)
salmon<-read.table("salmon.txt", header=T)
attach(salmon)
group=as.factor(sex) #섹터 별로 저장
group 

# Scatter Plot

plot(salmon[, 3:4], pch=unclass(sex),col = c('blue','red')[group])
# 산점도 결과 분류가 잘될꺼같음 한선으 조금 제거해야할수도
# LDA 그룹간 분류를 해주는 함수식을 찾는것 (1차)
# 2차는 QDA


# Density function of Variables
par(mfrow=c(1,2))
ldahist(data= x1, g=sex, type="density") # 확률밀도함수를 그려주는 함수
ldahist(data= x2, g=sex, type="density") 
#꽃받침 길이는 분산의 차이가 있어보이지만 꽃받침폭은 분산에 차이가 없어보인다.

#공분산행렬 동질성 검정
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


ina=as.numeric(as.factor(salmon[, 2])) # 책에서는 as.factor가 빠져있음 / 문자열로 저장이 되어있으면 as.factor형태로 팩터를 넣어주어야함

x=salmon[, 3:4]
cov.Mtest(x, ina)
#p.value가 0.05보다 작으므로 귀무가설기각 분산이 동질하지 않음


#공분산행렬 동질성 검정(내장함수) 
library(biotools)
boxM(x, as.factor(salmon[, 1])) # 데이터 및 그룹변수를 펙터형태로 넣음






############################################################
#다변량 정규성만족,공분산행렬 동질성 성립 하지 않으므로  QDA 실시 #
LDA=lda(sex~x1+x2, data=salmon, prior=c(1,1)/2) # prior사전확률, 디폴트 값이 동일하기에 모르는 경우적지않아도됨
LDA # 위에서 학습한 qda모델

qcluster=predict(LDA, salmon)$class # predict(모델, 예측할 데이터)/ #qda로 예측한 결과 그룹 저장
#predict(QDA, setosa_versi)-posterior: 그 그룹일 확률/class:나눠진 그룹

qct=table(sex, qcluster) #실제 데이터의 타겟변수와 예측한 데이터를 table형태로 비교
qct
#하나의 데이터가 setosa인데 versicolor로 오분류됨

# Total percent correct
mean(sex==qcluster)
#49%로 낮은 예측률 보임

