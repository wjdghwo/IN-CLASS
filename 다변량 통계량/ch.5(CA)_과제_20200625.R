#data
setwd("D:/2020 1학기 정호재/다변량통계학(1)/200625 다변량 실습 6/data")
Data5.8.1<-read.table("kellogg.txt", header=T)
X<-Data5.8.1[,-1]
gov<-Data5.8.1[,1]
rownames(X)<-gov
X

#5.7.1
dummy_var <- transform(X,
                       new_X1 = ifelse(X1 >mean(X1),1,0), 
                       new_X2 = ifelse(X2 >mean(X2),1,0),
                       new_X3 = ifelse(X3 >mean(X3),1,0), 
                       new_X4 = ifelse(X4 >mean(X4),1,0),
                       new_X5 = ifelse(X5 >mean(X5),1,0),
                       new_X6 = ifelse(X6 >mean(X6),1,0),
                       new_X7 = ifelse(X7 >mean(X7),1,0),
                       new_X8 = ifelse(X8 >mean(X8),1,0),
                       new_X9 = ifelse(X9 >mean(X9),1,0),
                       new_X10= ifelse(X10 ==mean(X10),1,0))
X<-dummy_var[,-c(1:10)]
X<-as.matrix(X)
X

#5.7.2
n<-nrow(X)
p<-ncol(X)
drs<-dist(X, method="euclidean")
drs
crs<-1-drs^2/p
crs

install.packages("proxy")
library(proxy)
summary(pr_DB)
crs<-1-dist(X, method = "simple matching")
drs<-sqrt(p*(1-crs))
drs
crs
round(sqrt(p*(1-crs)),2)==round(drs,2)

#5.7.3
Data5.8.1<-read.table("kellogg.txt", header=T)
X<-Data5.8.1[,-1]
gov<-Data5.8.1[,1]
rownames(X)<-gov
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

# 표준화 유클리드거리(변수간 단위가 다르므로 사용)
ds <- dist(Z, method="euclidean")
round(ds, 3)
#단일연결법
single=hclust(ds, method="single")
plot(single, hang=-1, main="(a) Sinle Linkage")
rect.hclust(single,k=6)
#완전연결법
complete=hclust(ds, method="complete")
plot(complete,hang=-1, main="(b) Complete Linkage")
rect.hclust(complete,k=6)
#평균연결법
average=hclust(ds, method="average")
plot(average, hang=-1, main="(c) Average Linkage")
rect.hclust(complete,k=6)
#와드연결법
ward=hclust(ds, method="ward.D2")
plot(ward, hang=-1, main="(d) Ward Linkage")
rect.hclust(complete,k=6)

#5.7.4
install.packages("NbClust") #군집의 개수 정해줌
library(NbClust)
Creals=Data5.8.1[,1]

#Dindex Index
dindex<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8,
                method = "kmeans", index = "dindex")
dindex

kmeans <- kmeans(Z, 6) # 6 cluster solution
kmeans
cluster=data.frame(Creals,cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C5=cluster[(cluster[,2]==5),]
C6=cluster[(cluster[,2]==6),]
C1;C2;C3;C4;C5;C6

# Get cluster means 
aggregate(X, by=list(kmeans$cluster),FUN=mean)

library(cluster)
kmedoids <- pam(Z, 6, metric="euclidean") # 6 cluster solution
cluster=data.frame(Creals,cluster=kmedoids$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C5=cluster[(cluster[,2]==5),]
C6=cluster[(cluster[,2]==6),]
C1;C2;C3;C4;C5;C6

# Get cluster means 
aggregate(X, by=list(kmedoids$cluster),FUN=mean)

#5.7.5
#5.7.6