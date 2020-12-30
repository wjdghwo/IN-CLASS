#### 20200625
#### CA
## Hir -> Non-Hir -> # of cluster

#1 계층적 군집분석
* 데이터 간의 유사성을 계산해 가장 가까운 객체들부터 차례로 군집화하는 방법
* dendrogram을 사용해 군집 형성 과정 파악
* 최상위 군집은 모든 데이터를 포함하는 군집, 최 하위 군집은 단 하나의 데이터만 포함하는 군집 
* 비계층적 군집분석과는 다르게 군집의 수를 따로 지정해주지 않음

#2 비계층적 군집분석
* 초기군집수 K개의 군집으로 시작하여 각 군집의 중심과 개체 간의 거리를 계산하여 가장 가까운 중심을 가진 군집에 할당
* k-means(K-평균법), K-중위수법, K-대표개체법 등등 
* 군집의 수를 지정해주어야 함 
* 이상치에 민감

#3 군집의 수 정하기

ccc(cubic clustering criterion) , Dindex, 등등 약 20개의 기준값을 비교하여 군집의 수 선택



setwd("C:/Users/user/Desktop/Rdata")

############# Hierarchical CA #############
#### economicview data (p.251)
Data1.3.5<-read.table("economicview.txt", header=T)
X<-Data1.3.5[,-1]
gov<-Data1.3.5[,1]
rownames(X)<-gov
X

# 유클리드거리
de<-dist(X, method="euclidean")
round(de, 3)
ward=hclust(de, method="ward.D")
# ward=hclust(de, method="ward.D")
plot(ward, labels=gov, main="(a) Ward Linkage : Euclidean Distance")

de


# make Z(for standized euclid dist)
n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n # 평균벡터
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                  # 중심화행렬
Y<-H%*%as.matrix(X)        # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)          # 공분산행렬 
D<-diag(1/sqrt(diag(S)))     # 표준편차행렬의 역
Z<-Y%*%D                # 표준화자료행렬
colnames(Z)<-colnames(X)


# 표준화 유클리드거리
ds <- dist(Z, method="euclidean")
round(ds, 3)

#wards=hclust(ds, method="ward")
wards=hclust(ds, method="ward.D")
plot(wards, labels=gov,  main="(b) Ward Linkage : Standardized Euclidean Distance")

# 마할라노비스거리
library(biotools)
dm<-D2.dist(X, S)
round(sqrt(dm), 3)
# wardm=hclust(ds, method="ward")
wardm=hclust(ds, method="ward.D2")
plot(wardm, labels=gov, main="(c) Ward Linkage : Mahalanobis Distance")

#### KLPGA Data (255p)
# AMCA : AM Linkages
Data1.3.2<-read.table("klpga.txt", header=T)

X<- Data1.3.2

n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n # 평균벡터
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                  # 중심화행렬
Y<-H%*%as.matrix(X)         # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)          # 공분산행렬 
D<-diag(1/sqrt(diag(S)))     # 표준편차행렬의 역
Z<-Y%*%D                # 표준화자료행렬
colnames(Z)<-colnames(X)
rownames(Z)<-rownames(X)

# 표준화 유클리드거리
ds <- dist(Z, method="euclidean")
round(ds, 3)
#단일연결법

sinle=hclust(ds, method="single")
par(mfrow=c(1,1))
plot(sinle, hang=-1, main="(a) Sinle Linkage")
#완전연결법
complete=hclust(ds, method="complete")
plot(complete,hang=-1, main="(b) Complete Linkage")
#평균연결법
average=hclust(ds, method="average")
plot(average, hang=-1, main="(c) Average Linkage")
#와드연결법
ward=hclust(ds, method="ward.D2")
plot(ward, hang=-1, main="(d) Ward Linkage")


#### utility data
# AMCA : Ward Linkage for US Public Utilities
Data5.3.1<-read.table("utility.txt", header=T)
X<-Data5.3.1[,-1]
rownames(X)<-Data5.3.1[,1]
X

n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n # 평균벡터
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                  # 중심화행렬
Y<-H%*%as.matrix(X)         # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1)          # 공분산행렬 
D<-diag(1/sqrt(diag(S)))     # 표준편차행렬의 역
Z<-Y%*%D                # 표준화자료행렬
colnames(Z)<-colnames(X)

# 표준화 유클리드거리
ds <- dist(Z, method="euclidean")
round(ds, 3)

#와드연결법
ward=hclust(ds, method="ward.D2")
plot(ward, labels=rownames(X), hang=-1,  main=" Ward Linkage : Standardized Euclidean Distance")


############# Non-Hierarchical CA #############
### utility data
# K-Means CA for Public Utilities
Data5.3.1<-read.table("utility.txt", header=T)
X<-Data5.3.1[,-1]
Z<-scale(X)
company=Data5.3.1[,1]

# K-means Method
kmeans <- kmeans(Z, 4) # 4 cluster solution
cluster=data.frame(company,cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C1;C2;C3;C4

# Get cluster means 
aggregate(X, by=list(kmeans$cluster),FUN=mean)

### economicview data
# K-Means CA for Economic Views
Data1.3.5<-read.table("economicview.txt", header=T)
X<-Data1.3.5[,-1]
Z<-scale(X)
기관=Data1.3.5[,1]

# 표준화 유클리드거리
ds <- dist(Z, method="euclidean")
round(ds, 3)

# K-means Method
kmeans <- kmeans(Z, 4) # 4 cluster solution
cluster=data.frame(기관,cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C1;C2;C3;C4

# Get cluster means 
aggregate(X, by=list(kmeans$cluster),FUN=mean)

####### # of cluster #########
### utility data

# Index for the Number of Clusters in K-Means CA: Public Utilities
# install.packages("NbClust")
library(NbClust)
Data5.3.1<-read.table("utility.txt", header=T)
X<-Data5.3.1[,-1]
Z<-scale(X)
company=Data5.3.1[,1]

#CCC Index
ccc<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8,
             method = "kmeans", index = "ccc")
ccc

?NbClust

plot(2:8, type="b", ccc$All.index, xlab="Number of Clusters",
     ylab="CCC")

#Dindex Index
dindex<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8,
                method = "kmeans", index = "dindex")
dindex

#All Indices
allindex<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8, 
                  method = "kmeans", index = "all" )
allindex
