# Practice for Multivariate Statistics
# 02.04.2020

# ctrl + l : consol 창 내용 지우기
# ctrl + r : 라인별 스크립트 실행
# ?함수  ex) ?apply
# 함수의 코드 구성을 알고 싶을때 : F5



### R 기초 사용법 ###
#scala
x = 2
x
x <- 4
x

#vector
y = c(1,2,3)
y

seq(1,10)
seq(1,10,0.5)

rep(1,10)
rep(1:3,10)

#matrix
matrix(1,nrow=3,ncol=3)
matrix(1,3,3)
matrix(c(1,2,3,1,2,3,1,2,3),3,3)
matrix(c(1,2,3,1,2,3,1,2,3),3,3, byrow = T)

A = matrix(rep(c(1,2,3),3),3,3, byrow=T)
A
colnames(A) = c("일","이","삼")
A
rownames(A) = c(1,2,3)
A
### 파일 불러오기 ###
#txt
read.table("D:/iris(table).txt")
read.table("D:/iris(table).txt",header=TRUE)

#csv
read.csv("D:/iris(csv).csv",header=T)

#xlsx
install.packages("xlsx")
library(xlsx)
read.xlsx("D:/ex(xlsx).xlsx",header=T, sheetIndex="Sheet1")

## xlsx package 설치가 안될 때 ##
install.packages("rJava")
source("https://install-github.me/talgalili/installr")
installr::install.java()
library(rJava)

#작업경로 설정
setwd("C:/Users/user/Desktop/Rdata")
X = read.csv("iris(csv).csv",header=T)
head(X)

### p.3 ###
Data1.1.1 = read.table("3subjects.txt", header=T)
Data1.1.1
Data1.1.1[,-1]	# 1열 제외
X = Data1.1.1

# indexing : 데이터 일부를 선택/ 선별하는 작업

X[1,1] 		# X의 1행 1열
X[,3]	 	# 3열만 
X[5,]	 	# 5행만
X[1:5,]		#1~5행까지
X[,c(1,3)]  	# 1,3열만
X[1:5,c(-2,-3)]	# 1~5행 표시하는데 2,3열의 변수제외


# Descriptive Statistics
summary(X)


# Covariance Matrix
cov(X)

# Correlation Matirx
cor(X)

# Multiple Scattor Plot
plot(X)

# Boxplot of 3 Subjects
boxplot(X, xlab= "3 Subjects", ylab="Exam Mark")

# Standard Deviation
sd(X[,'Mechanics'])     # sd(X[,1])
sd(X[,'Algebra'])
sd(X[,'Statistics'])

# Stem-and-leaf plot
stem(X[,1])
stem(X[,2])
stem(X[,3])


### p.7 [r-code 1.1.2] ### 
install.packages("rgl")
library(rgl)
# ‘ rgl ’ packges는 3차원 그래프로 결과를 보여줌
# 'rgl' 패키지 다운이 안 될 시 최신 버전 r을 설치

# Observations in Variables Space 
lim<-c(0, 100) 
plot3d(X[,1], X[,2], X[,3],xlim=lim, ylim=lim, zlim=lim,
       xlab="Mechanics", ylab="Algebra", zlab="Statistics") 
text3d(X[,1], X[,2], X[,3],rownames(X)) 

# Variables in Observations Space 
plot3d(X[1,], X[2,], X[3,], xlim=lim, ylim=lim, zlim=lim, 
       xlab="Student1", ylab="Student2", zlab="Student3") 
text3d(X[1,], X[2,], X[3,], colnames(X))


### p.13 [r-code 1.3.1] ###
Data1.3.1<-read.table("5subjects.txt", header=T) 
X<-Data1.3.1[, -1] 
head(X,10)
# Multiple Scatter Plot 
plot(X) 
# Box Plot 
boxplot(X)
### p.16 [r-code 1.3.2] ###
Data1.3.2<-read.table("klpga.txt", header=T) 
X<-Data1.3.2
head(X)

# Descriptive Statistics 
summary(X)

# Covariance Matrix 
cov(X) 
options(digits=3)		# 3자릿수로 표시
cov(X)

# Correlation Matrix 
cor(X) 

# Multiple Scatter Plot 
plot(X) 

# Boxplot of 3 Subjects 
boxplot(X)

### p.19 [r-code 1.3.3] ###
Data1.3.3<-read.table("protein1.txt", header=T) 
X<-Data1.3.3

# Multiple Scatter Plot 
plot(X) 
# Barplot of 25 Countries 
X<-t(X)
par(las=2) 		# label style 1,2,3
par(mar=c(4,4,1,2))	# 여백 mar=(c(아래,왼쪽,위,오른쪽))
barplot(X, legend=rownames(X), horiz=TRUE) 

# Star Plot 
X<-scale(Data1.3.3) 
stars(X, key.loc=c(0, 2), full=FALSE)

### p.21 [r-code 1.3.4] ### 
Data1.3.4<-read.table("irisflower.txt", header=T) 
X<-Data1.3.4[, -1] 
head(X)
data(iris)
iris

# Box Plot 
par(mfrow=c(2, 2)) 
boxplot(꽃받침길이~group, data=X, xlab="붓꽃 종류", ylab="X1: 꽃받침 길이") 
boxplot(꽃받침폭~group, data=X, xlab="붓꽃 종류", ylab="X2: 꽃받침 폭") 
boxplot(꽃잎길이~group, data=X, xlab="붓꽃 종류", ylab="X3: 꽃잎 길이") 
boxplot(꽃잎폭~group, data=X, xlab="붓꽃 종류", ylab="X4: 꽃잎 폭") 

# Multiple Scatter Plot 
plot(X[,1:4], pch=unclass(X[,5]),col=1:3)

### p.36 [r-code 1.4.1]  ###
Data1.1.1<-read.table("3subjects.txt", header=T)
X<-Data1.1.1[,-1]
X
class(X)
X<-as.matrix(X)			# 자료행렬
n<-nrow(X)			# 행 개수
xbar<-t(X)%*%matrix(1,n,1)/n 	# 평균벡터
I<-diag(n)			
J<-matrix(1,n,n)
H<-I-1/n*J			# 중심화행렬
Y<-H%*%X			# 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)		# 공분산행렬 
D<-diag(1/sqrt(diag(S)))		# 표준편차행렬의 역
Z<-H%*%X%*%D		# 표준화자료행렬
colnames(Z)<-colnames(X)
R<-t(Z)%*%Z/(n-1)		# 상관행렬
R_S<-D%*%S%*%D		# 상관행렬과 공분산행렬의 대수적 관계
detS <- det(S)			# 일반화 분산
detR <- det(R)
trS <- sum(diag(S))		# 총 분산
trR <- sum(diag(R))
# 결과출력
X; Y; Z; S; R; detS; trS; detR; trR