'''
* 인자분석(요인분석) : 주성분분석(PCA)처럼 차원축소 기법에 속한다. 

* PCA vs FA

1. 구성
PCA : 변수들의 선형 결합
FA : 잠재적 공통인자의 선형 결합
* 변수 = 인자 적재(factor loadings)*잠재적 공통인자들 + 특정인자들 (x_j = lambda_j1*f_1 + lambda_j2*f_2 + ... )
* 인자 적재(factor loading)를 찾는 것이 주된 목표 중 하나
* 상관이 높은 변수들은 특정 인자에 공통적으로 유사한 인자적재값을 가진다.
* 특정인자: 공통인자에 의해 설명되지 못하는 부분

2. 생성되는 변수의 수
PCA : 
  - 변수명: 주성분(princiapl conponent)
- 서로 독립이다.
- 최대 변수의 수 만큼 생긴다. (제1주성분, 제2주성분, ... 제p주성분) , 
- 적합도(goodness-of-fit)에 의해 약 70% 이상을 차지하는 보통 2개의 주성분을 선택한다. 

FA : 
  - 변수명: 인자(factor)
- 서로 독립이다.
- 추정하고자 하는 인자모형에서 인자의 수(보통 2개내지 3개)를 정하고 모형의 적합성을 검정한다.


3. 생성되는 변수의 해석 및 의미
PCA : 
  - 제1주성분이 가장 중요, 그 다음 2주성분, ...
- 주성분의 해석은 주성분과 변수의 상관관계를 근사적으로 보여주는 주성분계수(가중치)를 보고 관련된 변수에 의해서 이루어진다. 

FA : 
  - 새로 생긴 변수인 인자들은 기본적으로 대등한 관계를 가진다. ( 어느것이 더 중요한 변수다, 이런 것들이 없다 ), 
- 인자의 이름은 인자적재값에 의해서 연관된 변수들에 의해서 적절하게 부여한다. 


* (임의로 만든 예시)
데이터 : 수학, 과학, 영어, 중국어, 프랑스어 5개의 변수로 이루어진 데이터. (값은 0~100점)  

PCA : 제1주성분 : 0.8*수학 + 0.72*과학 + 0.7*영어 + 0.6*중국어 + 0.6*프랑스어
-> 제1주성분의 의미 : 전체적인 성적 (대부분 계수가 높으므로)

FA : 인자적재(factor loadings (Loading matrix))를 잘 확인해야 한다.  
(인자적재:Loading matrix 값)
인자1	인자2
수학		0.9	0.3
과학		0.8	0.4
영어		0.3	0.7
중국어		0.2	0.8
프랑스어	0.2	0.9

-> 인자1 : 수학과 과학이 0.9와 0.8로 높은 값을 보이고 있다. [ 이과적 능력 ] 에 관한 변수라고 생각 할 수 있다. 
-> 인자2 : 영어, 중국어, 프랑스어가 높은 값을 보이고 있다. [ 언어적 능력 ] 에 관한 변수라고 생각 할 수 있다.


* 인자 분석의 목적
1. 입력 변수들의 특성 파악
2. 서로 독립인 새로운 변수 생성
3. 차원 축소
4. 다중공선성 문제 해결


* 인자 분석의 방법
1. PCFA(principal component factor analysis) : 
  -   PCA(주성분 분석) 알고리즘 이용 
- 대수적 알고리즘: 공분산행렬(상관행렬)을 스펙트럼 분해(spectral decomposition)
2. MLFA(maximum likelihood factor analysis) : 
  - 다변량 정규분포 가정이 반드시 필요하다.***중요
- 최대우도를 이용하여 모형의 인자 적재와 평균벡터를 추정한다.
- 대수적 알고리즘: 공분산행렬(상관행렬)을 공통인자 분해(commom factor decomposition) 

# probability vs likelihood (차이점)
# https://www.youtube.com/watch?v=pYxNSUDSFH4


* 인자회전

* Biplot
'''




## factor analysis

### PCFA Steps for KLPGA

#[Step 1] Data Matrix X
setwd("D:/2020 1학기 정호재/다변량통계학(1)/200602 다변량 실습 5/data")
Data1.3.2<-read.table("klpga.txt", header=T)
X=Data1.3.2
rownames<-rownames(X)
p=ncol(X) 
head(X)

#[Step 2] Covariance Matrix S(or Correlation Matix R)
R=round(cor(X),3) #단위가 다르므로 cor사용
R

#[Step 3] Spectral Decomposition (# of factor)
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=round(eigen.R$vectors, 2) # Eigenvectors
V

#[Step 4] Number of factors : m (# of factor)
gof=eigen.R$values/p*100 # Goodness-of fit
round(gof, 3) # contribution rate
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Factor Number", ylab="Eigenvalue")
#팔꿈치가 2에서 발생

#[Step 5]Factor LoadinPCAgs and Communality
V2=V[,1:2]
L=V2%*%diag(sqrt(eigen.R$values[1:2])) # Loading matrix : 인자적재 행렬
rownames(L) = colnames(X)
colnames(L) = c("요인1","요인2")
round(L, 3) #인자적재행렬 -요인1:평균퍼팅수만 절대값 낮음(텅균타수와 평균퍼팅수는 낮을수로 성적이 좋음)-성적/요인2:평균퍼팅수에서 절대값 높음-평균퍼팅수와 관련된 변수
round(diag(L%*%t(L)), 3) # Communality: 공통성 -> 설명력과 유사한 느낌
#두 요인이 변수를 설명해주는 설명력(1에 가까울수록 설명력 높음)

#[Step 6]Specific Variance : 특정분산(Psi) ( 1- Communality )
Psi=diag(R-L%*%t(L))
round(Psi, 3) #1-공통성(Communality): 0에 가까울수록 설명력 높음

#{Step 7] Residual Matrix ( 전체 = 공통성 + 특정분산 + 잔차 )
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 3) #잔차행렬:비대각원소들의 절대값이 낮을수록 두 요인을 잘 설명함

# PCFA using the principal()
library(psych)
pcfa<-principal(R, nfactors=2, rotate="none") #nfactors:요인수/rotate:인자회전
pcfa
#첫번째값:인자적재행렬/두번째값:인자별 설명력/세번째값:잔차의 제곱합(RMSR)

round(pcfa$values, 2)
gof=pcfa$values/p*100 # Goodness-of fit
round(gof, 3)



### MLFA Steps for KLPGA (no rotation) #다변량 정규성을 만족해야함

# ML Estimation using the factanal( )
library(psych)
mlfa<-factanal(covmat=R, factors = 2, rotation="none" ) #covmat:상관행렬 or cov행렬
mlfa 
#첫번째값:psi(상금율을 설명을 잘 못함)
#두번째값:인자적재값(pcfa평균퍼팅수가 낮고 그린적중률이 높았으나 mlfa에서는 반대)Factor1:경기성적 그린적중률과 관련된 변수
#세번째값:설명력88%

# Residual Matrix
L=mlfa$loading[,1:2] # factor loading 
Psi=mlfa$uniquenesses # specific variance
Rm = R-(L%*%t(L) + diag(Psi)) 
round(Rm, 3) #잔차행렬

# Factor Loadings Plot(before rotation)
lim<-range(pretty(L))
plot(L[,1], L[,2],main="Plot of Factor Loadings : none ",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim) #점
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)#변수명
abline(v=0, h=0) #x,y축
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1) #벡터(그래프로 요인이 어떤 특성을 가지는지 알고 싶으면 벡터의 크기를 보면 됨)
#pca와 해석 비슷하게 함 하지만 해석하기 애매한 부분도 있음

### 비교 해석 방식은 157p

###############################################################################
### MLFA Steps for KLPGA (rotation)
### rotation 장점
# 장점1 : 인자모형의 기본 성질 유지됨
# 공분산행렬의 추정치, 잔차행렬, 특정분산, 공통성 등이 변하지 않음
# 장점2 : 인자적재행렬 구조의 단순화 
# 각 변수(행)에 대해 어떤 공통인자의 인자적재는 큰 값을 갖고, 나머지 다른 공통인자의 인자적재는 매우 작거나 0에 가깝게 되는 형태
# 공통인자(열)를 기준해서 볼 때 가능한 많은 변수에 대한 인자적재가 0에 가깝게 되는 형태

# ML Estimation using the factanal( )
mlfa<-factanal(covmat=R, factors = 2,  rotation="varimax") # rotation="none"/varimax:직교회전 
mlfa
# rotation을 주지 않았을때보다 낮은 값들은 더 낮게 높은 값들은 더 높게 변함

# Residual Matrix
L=mlfa$loading[, 1:2]
L
Psi=mlfa$uniquenesses
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 3) #잔차행렬<-전체적으로 낮은값 (잘 설명하고 있다)

# Factor Loadings Plot(after rotation)
lim<-range(pretty(L))
plot(L[,1], L[,2],main="Plot of Factor Loadings : Varimax ",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)
#이전보다 해석하기 편해짐


######################################################################################################
### air pollution data
Data2.8.2<-read.table("airpollution.txt", header=T)
X=Data2.8.2
rownames<-rownames(X)
p=ncol(X) 
n=nrow(X)
head(X)
dim(X)
Z<-scale(X, scale=T) #Solar값만 유난히 높아서 scale해줌

# Covariance Matrix S(or Correlation Matix R)
R=round(cor(X),3)
R
round(cov(Z),3) #scale하면 cov사용

##### PCFA using the principal( ) #######
library(psych)
pcfa<-principal(Z, nfactors=3, rotate="varimax")
pcfa
#첫번째인자-자동차 배기가스와 관련/두번째인자-태양및 오존에 관련한 인자/세번째인자-풍속에 관련한 인다

round(pcfa$values, 3)
gof=pcfa$values/p*100 # Goodness-of fit(적합도)
round(gof, 3)

# Residual Matrix
L=pcfa$loading[, 1:3]
round(L, 3)
Psi=pcfa$uniquenesses
Psi
#앞의 데이터보다 설명력이 낮아 대체적으로 높은값을 가짐/Solar값(가장 높음)을 특별히 잘 설명을 하지 못함
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 3)

# Plot of PC Factor Loadings (171p)
par(mfrow=c(2,2))
lim<-range(pretty(L))
#요인 1과 2
plot(L[,1], L[,2],main="(a) PC Factor Loadings : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)

#요인 1과 3
plot(L[,1], L[,3],main="(b) PC Factor Loadings : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,1], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 3], col=2, code=2, length=0.1)

#요인 2과 3
plot(L[,2], L[,3],main="(c) PC Factor Loadings : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,2], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,2], L[, 3], col=2, code=2, length=0.1)


# Factor Scores : Regression Method #관측치와 관련한 그래프를 그려보자
fpc=pcfa$scores
round(fpc, 3)

# Plot of Factor Scores : PFA (173p)
par(mfrow=c(2,2))
par(pty="s")
lim<-range(pretty(fpc))
plot(fpc[,1], fpc[,2],main=" (a) Factor Scores : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,2], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
#7,8이 CO NO값 다른 값보다 높음

plot(fpc[,1], fpc[,3],main=" (b) Factor Scores : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,3], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(fpc[,2], fpc[,3],main="(c) Factor Scores : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,2], fpc[,3], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)


##### MLFA using the factanal( ) #### 요인분석으로도 관측치 데이터를 그릴수있음
library(psych)
mlfa<-factanal(Z, factors = 3, rotation="varimax", score="regression")
mlfa

# Residual Matrix
Lm=mlfa$loading[, 1:3]
round(L, 3)
Psi=mlfa$uniquenesses
Rm = R-(Lm%*%t(Lm) + diag(Psi))
round(Rm, 3)

# ML Factor Loadings Plot
par(mfrow=c(2,2))
lim<-range(pretty(L))
plot(Lm[,1], Lm[,2],main="(a) ML Factor Loadings : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(Lm[,1], Lm[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,1], Lm[, 2], col=2, code=2, length=0.1)

plot(Lm[,1], Lm[,3],main="(b) ML Factor Loadings : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(Lm[,1], Lm[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,1], Lm[, 3], col=2, code=2, length=0.1)

plot(Lm[,2], Lm[,3],main="(c) ML Factor Loadings : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(Lm[,2], Lm[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,2], Lm[, 3], col=2, code=2, length=0.1)


# Factor Scores : Regression Method
fml=mlfa$scores
round(fml, 3)

# Plot of Factor Scores : MLFA
par(mfrow=c(2,2))
par(pty="s")
lim<-range(pretty(fml))
plot(fml[,1], fml[,2],main=" (a) Factor Scores : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(fml[,1], fml[,2], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(fml[,1], fml[,3],main=" (b) Factor Scores : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(fml[,1], fml[,3], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(fml[,2], fml[,3],main="(c) Factor Scores : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(fml[,2], fml[,3], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)


# Plot of Factor Scores : Pairs(MLFA, PCFA) (174p) #MLFA와 PCFA의 차이
par(pty="s")
par(mfrow=c(2,2))
plot(fml[,1], fpc[,1],main="(a) Factor Scores : ml f1 and pc f1",  xlab="ml f1", ylab="pc f1",
     xlim=lim, ylim=lim)
text(fml[,1], fpc[,1], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
#두요인 비슷

plot(fml[,2], fpc[,2],main="(b) Factor Scores : ml f2 and pc f2",  xlab="ml f2", ylab="pc f2",
     xlim=lim, ylim=lim)
text(fml[,2], fpc[,2], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
#두요인 비슷

plot(fml[,3], fpc[,3],main="(c) Factor Scores : ml f3 and pc f3",  xlab="ml f3", ylab="pc f3",
     xlim=lim, ylim=lim)
text(fml[,3], fpc[,3], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
#두요인 비슷안함

#######################################################################################################
# Biplot based on the Singular Value Decomposition(180p~) #Biplot으로 데이터 확인
svd.Z <- svd(Z) 
U <- svd.Z$u    
V <- svd.Z$v 
D <- diag(svd.Z$d)
F <- (sqrt(n-1)*U)[,1:2]  # Factor Scores Matrix : F
L <- (sqrt(1/(n-1))*V%*%D)[,1:2] # Factor Loadings Matrix : Lambda
C<- rbind(F, L)
rownames(F)<-rownames(X)
rownames(L)<-colnames(X)

# Godness-of-fit
eig <- (svd.Z$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[1:2]) #편의를 위하여 두요인만 선택
per
gof


# Biplot: Joint Plot of Factor Loadings and Scores #회전 안했을 때
par(mfrow=c(1,2))
par(pty="s")
lim1 <- range(pretty(L))
lim2 <- range(pretty(F))
biplot(F,L, xlab="f1",ylab="f2", main=" (a) Unrotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

# Varimax Rotated Biplot: Joint Plot of Rotated Factor Loadings and Scores
#회전 했을 때

varimax<-varimax(L)
Lt = varimax$loadings 
T=varimax$rotmat
T
Ft= F%*%T
biplot(Ft,Lt, xlab="f1",ylab="f2", main="(b) Varimax Rotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)
#f1:자동차 배기가스와 관련된 인자, f2:태양방사능과 오존과 관련한 인자
#21번 자동차 배기가스 높고 태양광 낮음
#30번 전체적으로 낮음
#2사분면 대기오염 덜함 4사분면 대기오염 심한 데이터


