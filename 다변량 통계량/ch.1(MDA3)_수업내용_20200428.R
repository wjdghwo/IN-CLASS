## Chap 1 : Multivariate Data Analysis

# Bivarate Normal distribution
# Chisq plot
# Correlation Coefficient Test for Normailty
# MVN tests based on the Skewness and Kurtosis Ststistics

## set working dic
setwd("D:/2020 1학기 정호재/다변량통계학(1)/200402 다변량 실습1/Rdata")

## [r-code 1.6.1 BVNpdf.R] ##
# 이변량정규분포의 pdf와 등고선그림
BVNpdf <- function(mu1,mu2,sig1,sig2,rho) {
  par(mfrow=c(1,2))
  s12 = sig1*sig2*rho
  s11 = sig1^2
  s22 = sig2^2
  Sig <- matrix(c(s11,s12,s12,s22),ncol=2,nrow=2,byrow=T)
  Sinv <- solve(Sig)
  x1 <- seq(mu1 - 3.5*sig1,mu1+3.5*sig1,len=50)
  fx1 <- seq(-3.5,3.5,len=50)
  x2 <- seq(mu2 - 3.5*sig2,mu2+3.5*sig2,len=50)
  fx2 <- seq(-3.5,3.5,len=50)
  f <- function(x1,x2) {    
    cons <- ((2*pi)*det(Sig)^.5)^{-1}
    cons*exp(-(.5*(1 - rho^2)^{-1})*(x1^2+x2^2-2*rho*x1*x2))
  }
  f <- outer(fx1,fx2,f)
  persp(x1,x2,f,theta = 30, expand=.50)
  title(main="Bivariate Normal pdf")
  contour(x1,x2,f,lty="solid",drawlabels=F)
  title(main="Contour Plot of BVN pdf")
}

win.graph()
BVNpdf(0,0,1,1,0) #상관관계 없음
win.graph()
BVNpdf(0,0,1,1,0.8) #양의 상관관계
win.graph()
BVNpdf(0,0,1,1,-0.8) #음의 상관관계


## [r-code 1.8.1 iris-chisqplot.R] ##
# 다변량 정규성검정(카이스퀘어 사용)
data(iris)     
head(iris) 

#그룹별로 나눠서 검정
setosa = iris[1:50, 1:4]  # Iris data only for setosa
versicolor = iris[51:100, 1:4] # Iris data only for versicolor
virginica = iris[101:150, 1:4] # Iris data only for virginica 

# Chi-squre Plot for Checking MVN
x=virginica #setosa versicolor virginica각각 변수를 넣어서 확인
n=dim(x)[1]
p=dim(x)[2]
S=cov(x)
xbar=colMeans(x)
m=mahalanobis(x, xbar, S)
m=sort(m)
id=seq(1, n)
pt=(id-0.5)/n
q=qchisq(pt, p)
plot(q, m, pch="*", xlab="Quantile", ylab="Ordered Squared Distance")
abline(0, 1)
# 그래프에서 점들이 선에 조금 벗어나긴 해도 전체적으로 선형성을 띈다 따라서 다변량 정규성을 띌 것이다.
# 하지만 주관적이기 때문에 상관계수를 이용하여 판단을 한다.

# Correlation Coefficient Test for Normailty
rq=cor(cbind(q, m))[1,2]
rq
# 값이 0.9863187(setosa)으로 1에 가까우므로 다변량 정규성을 띈다고 판단(표를 이용-수업에서는 사용x)
# 이상치를 제거해주면 다변량 정규성에 띄게 할 수있음

## [r-code 1.8.2 iris-MVNtest.R] ##
# 왜도와 첨도를 이용하여 다변량 정규성 검정 
#install.packages("MVN")
library("MVN") # for mardia test
iris

# MVN tests based on the Skewness and Kurtosis Ststistics
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
versicolor = iris[51:100, 1:4] # Iris data only for versicolor and four variables
virginica = iris[101:150, 1:4] # Iris data only for virginica and four variables

par(mfrow=c(1,3))
result_setosa = mvn(setosa, mvnTest = "mardia", multivariatePlot =  "qq")
result_versicolor = mvn(versicolor, mvnTest = "mardia", multivariatePlot =  "qq")
result_virginica = mvn(virginica, mvnTest = "mardia", multivariatePlot =  "qq")
#mvnTest = "mardia"-디폴트 값, multivariatePlot =  "qq" - chisquare그래프

#다변량 정규성 검정 결과
result_setosa 
result_versicolor
result_virginica
# 첫 번째 값 : Mardia Skewness, Mardia Kurtosis의 result가 yes이므로 다변량정규성 띔
# 두 번째 값 변수에대한 일변량 정규성검정-모든변수가 정규성을 띄지 않아도 다변량정규성을 띌수있음
# 기본가정이 다변량정규성을 따라야하는것이 많음


# term project에서 쓸 데이터가 있는 사이트
# https://www.kaggle.com/
# https://kosis.kr/ #국가 통계포털
# https://www.data.go.kr/ #공공데이터 포털
# https://kbig.kr/portal/kbig #빅데이터 센터
