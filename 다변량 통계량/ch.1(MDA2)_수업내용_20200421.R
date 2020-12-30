## Chap 1 : Multivariate Data Analysis

## set working dic
setwd("D:/2020 1학기 정호재/다변량통계학(1)/204021 다변량 실습2/Rdata")

## [r-code 1.4.2 irisflower- covcorr.R] ##

iris<-read.table("irisflower.txt",header=T)
X <- iris[,-1]
head(X)
plot(X[,1:4],pch=unclass(X[,5]), col=1:3)

setosa=X[which(X$group=="setosa"),]
versicolor=X[which(X$group=="versicolor"),]
virginica=X[which(X$group=="virginica"),]


win.graph()#새 플롯창 띄어줌
plot(setosa[,1:4],col=1)
win.graph()
plot(versicolor[,1:4],col=2)
win.graph()
plot(virginica[,1:4],col=3)

# 눈으로 보면 부정확할수도 있어서 수치로 확인하자


XX<-iris[,2:5]
head(XX)

SandR<-array(NA,c(4,4,6)) #빈공간 만듬 4x4행렬 6개 만듬
rownames(SandR)<-colnames(XX)
colnames(SandR)<-colnames(XX)
variation<-matrix(NA,3,4)
rownames(variation)<-c("setosa","versicolor","virginica")
colnames(variation)<-c("detS","trS", "detR","trR")

for(i in 1:3) {
  X<-XX[(50*(i-1)+1):(50*i),]
  
  S<-cov(X) #공분산
  R<-cor(X) #상관행렬
  
  detS<-det(S) #일반화 분산
  detR<-det(R) #일반화 상관행렬
  trS<-sum(diag(S)) #총분산
  trR<-sum(diag(R)) #총 상관행렬의 합
  
  SandR[,,i*2-1]<-S
  SandR[,,i*2]<-R
  
  variation[i,1]<-detS
  variation[i,2]<-trS
  variation[i,3]<-detR
  variation[i,4]<-trR
}

SandR<-round(SandR,3)
variation<-round(variation,6)
setosa<-list(SandR[,,1],SandR[,,2],variation[1,]) #비교를 위해 각각을 묶음
versicolor<-list(SandR[,,3],SandR[,,4],variation[2,])
virginica<-list(SandR[,,5],SandR[,,6],variation[3,])

setosa
versicolor
virginica
#1 공분산 2 상관행렬 
# 3 총분산-trS / detR-일반화 상관행렬이 클수록 비대각행렬이 값이 작아짐(상관계수값이 높다)
# 그룹별로 특성이 다 다르다

## [r-code 1.5.1 3subjects-distances.R] ##
Data1.1.1<-read.table("3subjects.txt", header=T)
head(Data1.1.1)
dim(Data1.1.1)
X<-as.matrix(Data1.1.1)

n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n # 평균벡터
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                  # 중심화행렬-원래의 데이터에서 평균을 뺀것
Y<-H%*%X                 # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)          # 공분산행렬 
D<-diag(1/sqrt(diag(S)))     # 표준편차행렬의 역수 (1/s_ii)
Z<-Y%*%D                # 표준화자료행렬
colnames(Z)<-colnames(X)

# https://vsdevelop.tistory.com/2
# 유클리드거리 sqrt ((x1-x2)^2 + (y1-y2)^2) 
# 두 점사이의 일직선 거리
dist(X, method="euclidean")
de <- as.matrix(dist(X, method="euclidean"))
de <- as.dist(de)
round(de, 3)

#install.packages("rgl")
library(rgl)
plot3d(X)
text3d(X[,1],X[,2],X[,3],rownames(X))



# 표준화 유클리드거리 
Z
ds <- as.matrix(dist(Z, method="euclidean"))
ds <- as.dist(ds)
round(ds, 3)
plot3d(Z)
text3d(Z[,1],Z[,2],Z[,3],rownames(X))

# 시티블럭거리(= manhattan distance = L1 distance) x2-x1 + y2-y1
# 두 점사이의 거리간 블럭이 있을때 모든 거리는 같다.
#?dist # 모를때 사용
dc <- as.matrix(dist(X, method="manhattan"))
dc <- as.dist(dc)
round(dc, 3)

# 전체적인 결과 양상은 비슷함

# iris - distance
data(iris)
head(iris) #4개의 수치형 변수와 1개의 변수형 변수
setosa = iris[which(iris$Species=='setosa'),-5] # 계산을 위햐여 species는 제거
versicolor = iris[which(iris$Species=='versicolor'),-5]
virginica = iris[which(iris$Species=='virginica'),-5]
head(setosa)

setosa_centroid=apply(setosa, 2, mean) #평균
versicolor_centroid=apply(versicolor, 2, mean)
virginica_centroid=apply(virginica, 2, mean)

# 1~50 :setosa 51~100 versicolor 101~150 virginica

X = rbind(setosa_centroid,iris[80,-5])
Y = rbind(versicolor_centroid,iris[80,-5])
Z = rbind(virginica_centroid,iris[80,-5])

# setosa versicolor virginica와 iris 80 점과의 거리계산

dist(X,method = "euclidean")
dist(Y,method = "euclidean")
dist(Z,method = "euclidean")

# 80번은 versicolor에세 가장 가까움
# 14번은 setosa에서 가장 가까움
# 점이 어떤 그룹인지 유추가능하다.





#############################구한 거리 이용방법
#골프자료

# 단일연결법
single=hclust(ds,method="single")
plot(single, labels=선수)

# 와드연결법
ward=hclust(ds,method="ward.D2")
plot(ward, labels=선수)