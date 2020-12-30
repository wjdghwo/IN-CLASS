############################################
# Summary Of Data
install.packages('readxl')
library(readxl)
h_export<-read_excel("C:/Users/Administrator/Desktop/현대기아 국가별 판매량.xlsx",
                     sheet="rdata",
                     range="A1:C49",
                     col_names=TRUE,
                     col_types="guess",
                     na = "NA")
h_export


install.packages('ggplot2')
library(ggplot2)
ggplot(data=h_export, aes(x=year, y=data, colour=Region,
                          group=Region)) +
  geom_line() +
  geom_point(size=3) + ggtitle("현대기아 국가별 판매량")
h_usa_year<-read_excel("C:/Users/Administrator/Desktop/현기차 미국 제품별 판매현황.xlsx",
                       sheet="rdata1",
                       range="A1:D187",
                       col_names=TRUE,
                       col_types="guess",
                       na = "NA")
h_usa_year


ggplot(data=h_usa_year, aes(x=year, y=data,
                            colour=model, group=model)) +
  geom_line() +
  geom_point(size=3) + ggtitle("현대기아 차종별 판매량")
h_usa<-read_excel("C:/Users/Administrator/Desktop/현기차 미국 제품별 판매현황.xlsx",
                  sheet="rdata2",
                  range="A1:I32",
                  col_names=TRUE,
                  col_types="guess",
                  na = "NA")
h_usa<-data.frame(h_usa)
h_usa


rownames(h_usa)<-h_usa[,1]
h_usa<-h_usa[,-1]
X<-h_usa
class(X)
X<-as.matrix(h_usa)


##############################################
boxplot(X)
n<-nrow(X)
p<-ncol(X)
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J # 중심화행렬-원래의데이터에서 평균을 뺀것
Y<-H%*%X # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1) # 공분산행렬
D<-diag(1/sqrt(diag(S))) # 표준편차행렬의 역수 (1/s_ii)
Z<-Y%*%D # 표준화자료행렬


rownames(Y)<-rownames(X)
rownames(Z)<-rownames(X)
colnames(Y)<-colnames(X)
colnames(Z)<-colnames(X)


xbar<-colMeans(X) # 평균벡터
S<-cov(X)# 공분산행렬
R<-cor(X)# 상관행렬
head(Y)
head(Z)
Y;Z;S;R


detS <- det(S)
detR <- det(R)
trS <- sum(diag(S))
trR <- sum(diag(R))
detS; # 데이터의 일반화 분산
detR; # 상관행렬의 일반화 분산
trS; # 데이터의 총 분산
trR; # 상관행렬의 총 분산


############################################
# Multivarate Normality Test
Zbar=colMeans(Z)
m<-mahalanobis(Z, Zbar, R)
m<-sort(m)
id<-seq(1, n)
pt<-(id-0.5)/n
q<-qchisq(pt, p)
plot(q, m, pch="*", xlab="Quantile", ylab="OrderedSquared Distance")
abline(0, 1)
rq<-cor(cbind(q, m))[1,2]
rq

X<-h_usa
class(X)
X<-as.matrix(h_usa)
X

library(MVN)
up=X[which(X[,8]==1),]
do=X[which(X[,8]==-1),]
up=up[,1:7]
do=do[,1:7]
result_up = mvn(up)
result_do = mvn(do)
result_up
result_do
mvn(X)


###########################################
# DCA
X=up
n<-nrow(X)
p<-ncol(X)
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J # 중심화행렬-원래의 데이터에서 평균을 뺀것
Y<-H%*%X # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1) # 공분산행렬
D<-diag(1/sqrt(diag(S))) # 표준편차행렬의 역수 (1/s_ii)
Z<-Y%*%D # 표준화자료행렬


rownames(Y)<-rownames(X)
rownames(Z)<-rownames(X)
colnames(Y)<-colnames(X)
colnames(Z)<-colnames(X)


xbar<-colMeans(X) # 평균벡터
S<-cov(X)# 공분산행렬
R<-cor(X)# 상관행렬
head(Y)
head(Z)
Y;Z;S;R


detS <- det(S)
detR <- det(R)
trS <- sum(diag(S))
trR <- sum(diag(R))
detS; # 데이터의 일반화 분산
detR; # 상관행렬의 일반화 분산
trS; # 데이터의 총 분산
trR; # 상관행렬의 총 분산


Zbar=colMeans(Z)
m<-mahalanobis(Z, Zbar, R)
m<-sort(m)
id<-seq(1, n)
pt<-(id-0.5)/n
q<-qchisq(pt, p)
plot(q, m, pch="*", xlab="Quantile", ylab="Ordered Squared Distance")
abline(0, 1)
rq<-cor(cbind(q, m))[1,2]
rq


####################################
X=do
n<-nrow(X)
p<-ncol(X)
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J # 중심화행렬-원래의 데이터에서 평균을 뺀것
Y<-H%*%X # 중심화 자료행렬
S<-t(Y)%*%Y/(n-1) # 공분산행렬
D<-diag(1/sqrt(diag(S))) # 표준편차행렬의 역수 (1/s_ii)
Z<-Y%*%D # 표준화자료행렬


rownames(Y)<-rownames(X)
rownames(Z)<-rownames(X)
colnames(Y)<-colnames(X)
colnames(Z)<-colnames(X)


xbar<-colMeans(X) # 평균벡터
S<-cov(X)# 공분산행렬
R<-cor(X)# 상관행렬
head(Y)
head(Z)
Y;Z;S;R


detS <- det(S)
detR <- det(R)
trS <- sum(diag(S))
trR <- sum(diag(R))
detS; # 데이터의 일반화 분산
detR; # 상관행렬의 일반화 분산
trS; # 데이터의 총 분산
trR; # 상관행렬의 총 분산


Zbar=colMeans(Z)
m<-mahalanobis(Z, Zbar, R)
m<-sort(m)
id<-seq(1, n)
pt<-(id-0.5)/n
q<-qchisq(pt, p)
plot(q, m, pch="*", xlab="Quantile", ylab="Ordered Squared Distance")
abline(0, 1)
rq<-cor(cbind(q, m))[1,2]
rq


cov.Mtest=function(x,ina,a=0.05){
  ## x is the *data set*
  ## ina is a *numeric vector* indicating the group of the data set 
  ##그룹데이터를 numeric vector형태로 변환해서 사용
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
  ## the next 2 lines calculate the pooled covariance
  matrix
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
  pvalue=1-pchisq(test,df) ## p-value of the test
  statistic
  crit=qchisq(1-a,df) ## critical value of the chi-square
  distribution
  list(M.test=test,degrees=df,critical=crit,p.value=pvalue) }


X<-h_usa
class(X)
X<-as.matrix(h_usa)
X


ina=as.numeric(as.factor(X[, 8]))
x=X[, 1:7]
cov.Mtest(x, ina)
# p.value가 0.05보다 작으므로 귀무가설 기각 분산이 이질


# 다변량 정규성 만족, 공분산행렬 동질성 성립하지 않으므로 QDA 실시
QDA=qda(유형~., data=X, prior=c(18,13)/31)
QDA


X=data.frame(X)
qcluster=predict(QDA, X)$class
qct=table(X$유형, qcluster)
qct


(1-mean(X$유형==qcluster))*100
QDA=qda(유형~., data=X, prior=c(18,13)/31, CV=TRUE)
QDA


confusion=table(X$유형, QDA$class)
confusion


# Expected actual error rate : EAER
EAER=(1-sum(diag(prop.table(confusion))))*100
EAER


####################################
# MDS
#비계량형 MDS: 데이터가 순서척도 인 변수를 가지는 경우 사용
X<-scale(as.matrix(X))
D <-as.matrix(dist(X, method="euclidean", diag=T))
car=colnames(D)


# Metric MDS
con<-cmdscale(D, k=2, eig=T)
con
con$eig
which(con$eig<0)
# 음수의 개수가 전체의 1/3보다 크므로 음수인 eigenvalue가 많다. 비계량형 mds를 적용


# Nonmetric MDS
library(MASS)
con<-isoMDS(as.matrix(D), k=2)
con


x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))


plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x+0.5,y,car, cex=0.8, pos=2)
abline(v=0, h=0)


# Shepard Diagram
dist_sh <- Shepard(D[lower.tri(D)], con$points)
dist_sh
cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
cdist_sh
plot(cdist_sh[,1], cdist_sh[,3], pch = ".", xlab = "Dissimilarity", ylab = "Distance",
     xlim = range(cdist_sh[,1]), ylim = range(cdist_sh[,1]))
lines(cdist_sh[,1], cdist_sh[,3], type = "S")


# Image Diagram
plot(cdist_sh[,2], cdist_sh[,3], pch = ".", xlab = "FitDissimilarity", ylab = "Distance",
     xlim = range(cdist_sh[,2]), ylim = range(cdist_sh[,2]))
lines(cdist_sh[,2], cdist_sh[,3], type = "p")


####################################
# CRA
saleup=length(which(X[,3]>10))
saledown=length(which(X[,3]<10))
saleup
saledown


cedan=length(which(X[,8]==1))
suv=length(which(X[,8]==-1))
cedan
suv


a=ifelse(X[,3]>=10,1,0)
O=table(X[,8],a)
row.names(O)=c("suv","cedan")
colnames(O)=c("saledown","saleup")
O
chisq.test(O)


F <- O/sum(O)
r <- apply(F,1,sum)
c <- apply(F,2,sum)
r;c;


Dr<- diag(1/sqrt(r))
Dc<- diag(1/sqrt(c))
Dr;Dc


cF<- F-r%*%t(c)
cF


Y <- Dr%*%(cF)%*%Dc
svd.Y <- svd(Y)
U <- svd.Y$u
V <- svd.Y$v
D <- diag(svd.Y$d)
A <- (Dr%*%U%*%D)[,1:2]
B <- (Dc%*%V%*%D)[,1:2]
rownames(A) <- c("suv","cedan")
rownames(B) <- c("saledown","saleup")
A;B


eig <- (svd.Y$d)^2
per <- eig/sum(eig)*100
gof <- sum(per[1:2])
rbind(round(eig, 3),round(per, 3))


par(pty="s")
lim <-range(-1:1)
plot(B[, 1:2],
     xlab="Dim1(100%)",ylab="Dim2(0.064%)",xlim=lim,ylim=lim,
     pch=15,col=2,
     main="SCRA Algorithm : 이원분할표")
text(B[, 1:2],rownames(B),cex=0.8,col=2,pos=3)
points(A[, 1:2],pch=16, col=4)
text(A[, 1:2],rownames(A),cex=0.8,pos=3, col=4)
abline(v=0,h=0)


########################################
# PCA
eigen.R<-eigen(R)
D_R<-round(eigen.R$values,2)
V_R<-round(eigen.R$vectors,2)

D_R
V_R

gof_R<-D_R/sum(D_R)*100
round(gof_R,2)
plot(D_R, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

V_R2<-V_R[,1:3]
round(V_R2,2)

P_R<-Z%*%V_R2
P_R<-as.matrix(P_R)
P_R

par(mfrow=c(2,2))
plot(P_R[,1], P_R[,2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P_R[,1], P_R[,2], labels=rownames(X), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P_R[,1], P_R[,3], main="Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P_R[,1], P_R[,3], labels=rownames(X), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P_R[,2], P_R[,3], main="Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P_R[,2], P_R[,3], labels=rownames(X), cex=0, col="blue", pos=1)
abline(v=0, h=0)

##############################################
pca.R<-princomp(X, cor=T)
round(pca.R$loadings[,1:3],2)
# Principle component biplot (SVD)

par(mfrow=c(2,2))
biplot(pca.R, choices=c(1,2), scale=0,  xlab="1st PC",ylab="2nd PC", main="1PC vs 2PC biplot function")
abline(v=0, h=0)
biplot(pca.R, choices=c(1,3), scale=0,  xlab="1st PC",ylab="3nd PC", main="1PC vs 3PC biplot function")
abline(v=0, h=0)
biplot(pca.R, choices=c(2,3), scale=0,  xlab="2st PC",ylab="3nd PC", main="2PC vs 3PC biplot function")
abline(v=0, h=0)

##############################################
# FA
library(psych)
pcfa<-principal(Z, nfactors=3, rotate="varimax") 
pcfa
mlfa<-factanal(Z, factors = 3, rotation="varimax")
mlfa

L<-pcfa$loadings[,1:3]
fpc<-pcfa$scores
Psi<-pcfa$uniquenesses
Rm<-R-(L%*%t(L)+diag(Psi))

L
fpc
Psi
Rm

gof<-pcfa$values/ncol(Z)*100
gof

par(pty="s")   # square figure.
lim<-c(min(fpc),max(fpc))

#
plot(fpc[,1], fpc[,2],main=" (a) Factor Scores : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,2], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
text(pcfa$loadings[,1], pcfa$loadings[,2], labels=rownames(pcfa$loadings), cex=0.8, col="red", pos=1)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)

#
plot(fpc[,1], fpc[,3],main=" (b) Factor Scores : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,3], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
text(pcfa$loadings[,1], pcfa$loadings[,3], labels=rownames(pcfa$loadings), cex=0.8, col="red", pos=1)
arrows(0,0,pcfa$loadings[,1], pcfa$loadings[,3], col=2, code=2, length=0.1)

#
plot(fpc[,2], fpc[,3],main="(c) Factor Scores : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,2], fpc[,3], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
text(pcfa$loadings[,2], pcfa$loadings[,3], labels=rownames(pcfa$loadings), cex=0.8, col="red", pos=1)
arrows(0,0,pcfa$loadings[,2], pcfa$loadings[,3], col=2, code=2, length=0.1)

##############################################
# CA
install.packages("NbClust")
library(NbClust)
allindex<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8, 
                  method = "kmeans", index = "all" )
allindex

ds <- dist(Z, method="euclidean")
round(ds, 3)
#단일연결법
sinle=hclust(ds, method="single")
plot(sinle, hang=-1, main="(a) Sinle Linkage")
rect.hclust(sinle,k=7)
#완전연결법
complete=hclust(ds, method="complete")
plot(complete,hang=-1, main="(b) Complete Linkage")
rect.hclust(complete,k=7)
#평균연결법
average=hclust(ds, method="average")
plot(average, hang=-1, main="(c) Average Linkage")
rect.hclust(average,k=7)
#와드연결법
ward=hclust(ds, method="ward.D2")
plot(ward, hang=-1, main="(d) Ward Linkage")
rect.hclust(ward,k=7)

# K-means Method
kmeans <- kmeans(Z, 7) # 7 cluster solution
cluster=data.frame(model,cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C5=cluster[(cluster[,2]==5),]
C6=cluster[(cluster[,2]==6),]
C7=cluster[(cluster[,2]==7),]
C1;C2;C3;C4;C5;C6;C7

# Get cluster means 
aggregate(X, by=list(kmeans$cluster),FUN=mean)
aggregate(Z, by=list(kmeans$cluster),FUN=mean)


# K-medoids Method
kmedoids <- pam(Z, 7) # 7 cluster solution
cluster=data.frame(model,cluster=kmedoids$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C5=cluster[(cluster[,2]==5),]
C6=cluster[(cluster[,2]==6),]
C7=cluster[(cluster[,2]==7),]
C1;C2;C3;C4;C5;C6;C7

# Get cluster means 
aggregate(X, by=list(kmedoids$cluster),FUN=mean)
aggregate(Z, by=list(kmedoids$cluster),FUN=mean)
