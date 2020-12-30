# 첫번째 과제 

setwd("D:/2020 1학기 정호재/다변량통계학(1)/200402 다변량 실습1/Rdata")

data <- read.table("irisflower.txt",header = T)
group <- data[,6]
dat=data[,2:3]
plot(dat,pch=unclass(group),col=unclass(group))
legend("topright",c("setosa","versicolor","verginica"),pch=c(1,2,3),col = c(1,2,3))


### 5subjects data

# Steps for PCA

#[Step 1] Data Matrix X
install.packages("MVT")
library("MVT")
data(examScor)
X=examScor
head(X)
dim(X) #행 88개(관측치)열 5개(변수)

#[Step 2] Covariance Matrix S(or Correlation Matix R)(데이터의 단위가 일정할 때 사용, 단위가 다를때는 corr함수사용)
S=round(cov(X),3)
S

#[Step 3] Spectrla Decompositoin 
eigen.S=eigen(S)
round(eigen.S$values, 3) # Eigenvalus
V=round(eigen.S$vectors, 3) # Eigenvectors
V
#주성분의 적합도 계산

#[Step 4] Choice of Eigenvalues and Eigenvectors
gof=eigen.S$values/sum(eigen.S$values)*100 # Goodness-of fit 적합도
round(gof, 2) #제 (1,2,3,4,5) 주성분의 설명력(%)
#1주성분:61.91%, 2주성분:18.21% -> 1주성분, 2주성분의 설명력=80.12% 70이상(경험적)이면 주성분을 이용해도 데이터가 설명이 가능함
plot(eigen.S$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")
#1주성분:700 2주성분:200 3주성분,4주성분:100 설명력 차이가 작을때 그전의 주성분을 선택함

#[Step 5] PCs : liner combination of original variables 
V2=V[,1:2]
V2
#주성분 벡터에서 중심화 행렬 표준화행렬을 곱하는데 단위가 같기 때문에 표준화행렬을 구하진 않았다.

#[Step 6] PCS, PCs Scores and New Data Matrix P
Y=scale(X, scale=F) # Centred Data Matrix(중심화행렬)
P=Y%*%V2            # PCs Scores(주성분점수-데이터로 이용)
P

#[Step 7] Plot of PCs Scores
plot(P[,1], P[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2]+2, labels=rownames(P), cex=0.8, col="blue")
abline(v=0, h=0)
#x축 1주성분, y축 2주성분 주성분간은 독립/ 선형관계 존재안함 
#관측치의 특성파악, 군집을 나눌수 있음 (0을 기준으로 왼쪽 오른쪽)
#관측치수가 높을수록 점수가 낮음->상위권 왼쪽, 하위권 오른쪽에 분포 되어있음

# PC Biplots for 5 Subjects Exam
library("MVT")
data(examScor)
X=examScor
n <- nrow(X) 
rownames(X)
colnames(X)
joinnames=c(rownames(X),colnames(X))

Y <- scale(X,scale=F)

# Biplot based on the Singular Value Decomposition(spectral이 아닌 singular value로 계산)
#변수들이 어떻게 영향을 끼지치는지/관측치과 변수와의 관계를 보여줌
svd.Y <- svd(Y) 
U <- svd.Y$u    
V <- svd.Y$v 
D <- diag(svd.Y$d)
G <- (sqrt(n-1)*U)[,1:2]
H <- (sqrt(1/(n-1))*V%*%D)[,1:2] 
C<- rbind(G, H)
rownames(G)<-rownames(X)
rownames(H)<-colnames(X)
rownames(C)<-joinnames

# Godness-of-fit(적합도)
eig <- (svd.Y$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[1:2])(제 2주성분까지 선택했을때 설명력이 80%정도 됨)

# Biplots
par(mfrow=c(2,2))
par(pty="s")
#변수에 대한 그래프
#화살표사이의 각 cos값이 1에 가까울수록 연관성이 높고 0에 가까울수록 독립임(-1이면 음의관계)
#stat와 mech 연관x, stat와 analy 연관o
#2nd pc에 0을 기준으로 위 closed book 아래 open book으로 나뉨
lim1 <- range(pretty(H))
plot(H[,1],H[,2],xlab="1st PC",ylab="2nd PC", main="(a) 5 Subjects",
     xlim=lim1,ylim=lim1,pch=15,col=2, type="n")
abline(v=0,h=0)
text(H[,1], H[,2],colnames(X),cex=0.8,col=1,pos=3)
arrows(0,0,H[,1],H[,2],col=2,code=2, length=0.1)

lim2 <- range(pretty(G))
plot(G[,1],G[,2],xlab="1st PC",ylab="2nd PC", main="(b) 88 Students",
     xlim=lim2,ylim=lim2,pch=16, type="n")
abline(v=0,h=0)
text(G[,1],G[,2],rownames(X),cex=0.8,pos=3)

lim3 <- range(pretty(C))
plot(C[,1],C[,2],xlab="1st PC",ylab="2nd PC",  main="(c) 5 Subjects and 88 Students",
     xlim=lim3,ylim=lim3,pch=16,  type="n")
abline(v=0,h=0)
text(C[,1],C[,2],joinnames,cex=0.8,pos=3)
arrows(0,0,C[89:93,1],C[89:93,2],col=2,code=2, length=0.1)

#화살표와 점을 합쳐서 scale해서 그린 그림-biplot
#상위권쪽의 방향으로 화살표 방향이 위치됨(꼭 화살표방향으로 점수가 높아지는것은 아님)
biplot(G,H, xlab="1st PC",ylab="2nd PC", main="(d) biplot function",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

### klpga data(횟수와 비율로 변수들간 단위가 다름->corr matrix사용)

# PCA Steps for KLPGA

#[Step 1] Data Matrix X
setwd("D:/2020 1학기 정호재/다변량통계학(1)/200402 다변량 실습1/Rdata")
Data1.3.2<-read.table("klpga.txt", header=T)
X=Data1.3.2
rownames<-rownames(X) 
head(X)

#[Step 2] Covariance Matrix S(or Correlation Matix R)
R=round(cor(X),3)
R

#[Step 3] Spectral Decomposition 
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=round(eigen.R$vectors, 2) # Eigenvectors

#[Step 4] Choice of Eigenvalues and Eigenvectors
gof=eigen.R$values/sum(eigen.R$values)*100 # Goodness-of fit
round(gof, 2) #제 2주성분까지 선택했을때 90.47%의 설명력으로 높은 설명력을 보임
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V2=V[,1:2]
V2

#[Step 6] PCS, PCs Scores and New Data Matrix P(변수간에 단위가 다르므로 표준화행렬 사용)
Z=scale(X, scale=T) # Standardized Data Matrix
Z
P=Z%*%V2            # PCs Scores(주성분점수)
round(P, 3)

#[Step 7] Plot of PCs Scores(주성분점수 그림)
plot(P[,1], P[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)
#오른쪽으로 갈수록 번호가 낮고(상위권 선수) 왼쪽으로 갈수록 번호가 높음(하위권 선수)
#관측치의 번호가 낮을수록 상금률높음 평균타수낮음 나머지 높음(우수한 선수), 관측치의 번호가 높을수록 대비됨(하위권 선수)
#제 1주성분의 의미:성적이라고 봐도 무관

### KLPGA PCbiplot(변수와 관측치간의 관계를 알아봄)
Data1.3.2<-read.table("klpga.txt", header=T)
X=Data1.3.2
n <- nrow(X) 
rownames(X)
colnames(X)

Y <- scale(X,scale=T)

# Biplot based on the Singular Value Decomposition
svd.Y <- svd(Y) 
U <- svd.Y$u    
V <- svd.Y$v 
D <- diag(svd.Y$d)
G <- (sqrt(n-1)*U)[,1:2]
H <- (sqrt(1/(n-1))*V%*%D)[,1:2]
rownames(G)<-rownames(X)
rownames(H)<-colnames(X) 

# Godness-of-fit
eig <- (svd.Y$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[1:2])
round(per, 2)
round(gof, 2)

# PC Biplot
lim<-range(pretty(G))
biplot(G,H, xlab="1st PC(71.83%)",ylab="2nd PC(18.64%)", main="Biplot for KLPGA Data ",
       xlim=lim,ylim=lim,cex=0.8,pch=16)
abline(v=0,h=0)
#파세이브율, 평균퍼팅수, 파브레이크율 각이작으므로 높은 연관성을 보임 평균타수와는 정반대(음의 상관)
#그린적줄율, 평균퍼팅수 간 각도 90도->관계없음
#상위권:파세이브율, 평균퍼팅수, 파브레이크율 높고 평균타수 낮음
#제1주성분의 의미는 성적이라고 할수있다.

### skull data
# PCA Steps based on the SD for Skull Data

#[Step 1] Data Matrix X(standized data)
Data1.3.2<-read.table("skull.txt", header=T)
Z=as.matrix(Data1.3.2)
rownames<-rownames(Z) 
colnames<-colnames(Z)
n=nrow(Z)

#[Step 2] Covariance Matrix S(or Correlation Matix R)
R=(t(Z)%*%Z/(n-1))
R# =cov(Z) #데이터가 이미 표준화되어있어서 단위가 같으므로 cov함수 사용

#[Step 3] Spectral Decomposition 
eigen.R=eigen(R)
round(eigen.R$values, 2) # Eigenvalues
V=eigen.R$vectors # Eigenvectors
round(V, 2)

#[Step 4] Choice of Eigenvalues and Eigenvectors
gof=eigen.R$values/sum(eigen.R$values)*100 # Goodness-of fit
round(gof, 2) #주성분 3까지의 합 76.8%
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V3=V[,1:3] #주성분 3개를 가짐
round(t(V3), 2)

#[Step 6] PCS, PCs Scores and New Data Matrix P
Z # Standardized Data Matrix
P=Z%*%V3            # PCs Scores
round(P, 3)

#[Step 7] Plot of PCs Scores
#주성분이 3개이므로 주성분그림도 3개가 되어야함
#주성분 1과 주성분 2의 plot
par(mfrow=c(2,2))
plot(P[,1], P[, 2], main="(a) Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
#왼쪽:두개골의 크기가 큰 군집(남성) 오른쪽:두개골의 크기가 작은군집(여성)
#제 1주성분 : 두개골의 크기

#주성분 1과 주성분 3의 plot
plot(P[,1], P[, 3], main="(b) Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P[,1], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

#주성분 2과 주성분 3의 plot
plot(P[,2], P[, 3], main="(c) Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P[,2], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

Data1.3.2<-read.table("klpga.txt", header=T)
X<-Data1.3.2

# PCA based on the SD using princomp( ) #Spectral을 사용한 함수
pca.R<-princomp(X, cor=T)
summary(pca.R, loadings=T) # explanation, coefficient
round(pca.R$scores, 3)  # PC score
screeplot(pca.R, type="lines")

# Principle component biplot (SD)
biplot(pca.R, scale=0, xlab="1st PC",ylab="2nd PC",
       main="PC Biplot for KLPGA Data ")   
abline(v=0, h=0)


# PCA on the SVD using prcomp( ) #Singular value를 사용한 함수
pcasvd.Z<-prcomp(X, scale=T) 
summary(pcasvd.Z)  # explanation
round(pcasvd.Z$rotation, 3) # PC coefficient
pcasvd.Z$scale
screeplot(pcasvd.Z, type="lines")

# Principle component biplot (SVD)
biplot(pcasvd.Z, scale=0,  xlab="1st PC",ylab="2nd PC",
       main="PC Biplot for KLPGA Data ")
abline(v=0, h=0)

#공부할때는 함수보다는 알고리즘을 사용해서 공부해보기