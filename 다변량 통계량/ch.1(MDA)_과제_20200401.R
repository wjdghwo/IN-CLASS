################1-1
setwd("D:/2020 1학기 정호재/다변량통계학(1)/200402 다변량 실습1/Rdata")
library(rgl)
library(MVN)
data1.3.2<-read.table("klpga.txt", header=T)
str(data1.3.2)
dim(data1.3.2)
colSums(is.na(data1.3.2))
summary(data1.3.2)

################1-2
X<-data1.3.2
class(X)
X<-as.matrix(X)# 자료행렬
n<-nrow(X)# 행 개수
xbar<-t(X)%*%matrix(1,n,1)/n # 평균벡터
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J# 중심화행렬
Y<-H%*%X# 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)# 공분산행렬 
D<-diag(1/sqrt(diag(S)))# 표준편차행렬의 역
Z<-H%*%X%*%D# 표준화자료행렬
colnames(Z)<-colnames(X)
R<-t(Z)%*%Z/(n-1)# 상관행렬
colnames(xbar)<-c("Mean")
xbar; # 평균벡터
S; # 공분산행렬 
R; # 상관행렬
colMeans(X) # 평균벡터
cov(X) # 공분산행렬 
cor(X) # 상관행렬

################1-3
detS <- det(S)
detR <- det(R)
trS <- sum(diag(S))
trR <- sum(diag(R))
detS; # 데이터의 일반화 분산
detR; # 상관행렬의 일반화 분산
trS; # 데이터의 총 분산
trR; # 상관행렬의 총 분산

################1-5
plot(data1.3.2)
cor(X)

################2-1
setwd("D:/2020 1학기 정호재/다변량통계학(1)/200421 다변량 실습2/Rdata")
data1.3.4<-read.table("irisflower.txt", header=T) 
str(data1.3.4)
dim(data1.3.4)
colSums(is.na(data1.3.4))
summary(data1.3.4)
X <- data1.3.4[,-1]
head(X)
plot(X[,1:2],pch=unclass(X[,5]), col=1:3)

################2-2
setosa=X[which(X$group=="setosa"),]
versicolor=X[which(X$group=="versicolor"),]
virginica=X[which(X$group=="virginica"),]
par(mfrow=c(1,3))
plot(setosa[,1:2],col=1)
plot(versicolor[,1:2],col=2)
plot(virginica[,1:2],col=3) 

################2-3
setosa=X[which(X$group=="setosa"),]
versicolor=X[which(X$group=="versicolor"),]
virginica=X[which(X$group=="virginica"),]
setosa_bar<-colMeans(setosa[,-3])
setosa_S<-cov(setosa[,-3]) 
setosa_R<-cor(setosa[,-3])
versicolor_bar<-colMeans(versicolor[,-3]) 
versicolor_S<-cov(versicolor[,-3]) 
versicolor_R<-cor(versicolor[,-3]) 
virginica_bar<-colMeans(virginica[,-3]) 
virginica_S<-cov(virginica[,-3])
virginica_R<-cor(virginica[,-3])
cbind(setosa_bar, versicolor_bar, virginica_bar) # 평균벡터
cbind(setosa_S, versicolor_S, virginica_S)
cbind(setosa_R, versicolor_R, virginica_R) # 상관행렬

#############3
x<-X
n<-dim(x)[1]
p<-dim(x)[2]
S<-cov(x)
xbar<-colMeans(x)
m<-mahalanobis(x, xbar, S)
m<-sort(m)
id<-seq(1, n)
pt<-(id-0.5)/n
q<-qchisq(pt, p)
plot(q, m, pch="*", xlab="Quantile", ylab="Ordered Squared Distance")
abline(0, 1)
rq<-cor(cbind(q, m))[1,2]
rq
result<-mvn(X, multivariatePlot =  "qq")
result
############4
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
BVNpdf(setosa_bar[1],setosa_bar[2],setosa_S[1,1],setosa_S[2,2],setosa_R[1,2])
BVNpdf(versicolor_bar[1],versicolor_bar[2],versicolor_S[1,1],
       versicolor_S[2,2],versicolor_R[1,2])
BVNpdf(virginica_bar[1],virginica_bar[2],virginica_S[1,1],
       virginica_S[2,2],virginica_R[1,2])
