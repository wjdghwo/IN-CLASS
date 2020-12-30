setwd("D:/2020 1학기 정호재/다변량통계학(1)/200402 다변량 실습1/Rdata")
Data1.3.3<-read.table("protein1.txt", header=T) 
X<-Data1.3.3

# Barplot of 25 Countries 
X<-t(X)
par(las=2) 		# label style 1,2,3
par(mar=c(4,4,1,2))	# 여백 mar=(c(아래,왼쪽,위,오른쪽))
barplot(X, legend=rownames(X), horiz=TRUE) 

# Star Plot 
X<-scale(Data1.3.3) 
stars(X, key.loc=c(0, 2), full=FALSE)

# MVN tests based on the Skewness and Kurtosis Ststistics
install.packages("MVN")
library("MVN") # for mardia test
mvn(X, mvnTest = "mardia", multivariatePlot =  "qq")

#############################################################
#[Step 1] Data Matrix X
Data1.3.3<-read.table("protein.txt", header=T)
X=Data1.3.3[,-c(1,2)]
rownames(X)<-Data1.3.3[,"국가"]
p=ncol(X) 
n=nrow(X)
head(X)
dim(X)
Z<-scale(X)
Z

#[Step 2] Covariance Matrix S(or Correlation Matix R)
R=cor(X)
R

#[Step 3] Spectral Decomposition (# of factor)
eigen.R=eigen(R)
round(eigen.R$values, 2)
V=eigen.R$vectors
round(V,3)

#[Step 4] Number of factors : m (# of factor)
gof=eigen.R$values/p*100
round(gof, 3)
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Factor Number", ylab="Eigenvalue")
#[Step 5]Factor LoadinPCAgs and Communality
V2=V[,1:3]
L=V2%*%diag(sqrt(eigen.R$values[1:3]))
rownames(L) = colnames(X)
colnames(L) = c("요인1","요인2","요인3")
round(L, 3)
round(diag(L%*%t(L)), 3)

#[Step 6]Specific Variance : 특정분산(Psi) ( 1- Communality )
Psi=diag(R-L%*%t(L))
round(Psi, 3)

#{Step 7] Residual Matrix ( 전체 = 공통성 + 특정분산 + 잔차 )
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm,3) 

#########################################################
par(mfrow=c(2,2))
lim<-range(pretty(L))
#요인 1과 2
plot(L[,1], L[,2],main="(a) PC Factor Loadings : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=2)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)

#요인 1과 3
plot(L[,1], L[,3],main="(b) PC Factor Loadings : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,1], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=2)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 3], col=2, code=2, length=0.1)

#요인 2과 3
plot(L[,2], L[,3],main="(c) PC Factor Loadings : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,2], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,2], L[, 3], col=2, code=2, length=0.1)





#####################################################
library(psych)
pcfa<-principal(Z, nfactors=3, rotate="varimax")
round(pcfa$values, 3)
gof=pcfa$values/p*100 # Goodness-of fit(적합도)
round(gof, 3)

# Residual Matrix
L=pcfa$loading[, 1:3]
round(L, 3)
Psi=pcfa$uniquenesses
round(Psi,3)
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 3)

#####################################################
# Plot of PC Factor Loadings 
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





#####################################################
# Factor Scores : Regression Method #관측치와 관련한 그래프를 그려보자
fpc=pcfa$scores

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

#####################################################
##### MLFA using the factanal( ) #### 
library(psych)
mlfa<-factanal(Z, factors = 3, rotation="varimax", score="regression")

# Residual Matrix
Lm=mlfa$loading[, 1:3]
round(Lm, 3)
Psi=mlfa$uniquenesses
Rm = R-(Lm%*%t(Lm) + diag(Psi))
round(Rm, 3)









#####################################################
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

plot(Lm[,2], Lm[,3],main="(b) ML Factor Loadings : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(Lm[,2], Lm[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,2], Lm[, 3], col=2, code=2, length=0.1)

#####################################################
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
#####################################################
# Plot of Factor Scores : Pairs(MLFA, PCFA) #MLFA와 PCFA의 차이
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
#두요인 비슷

#####################################################(1-2)
# Biplot based on the Singular Value Decomposition
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
gof <- sum(per[1:2]) 
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





#####################################################(1-3)
svd.Z <- svd(Z) 
U <- svd.Z$u    
V <- svd.Z$v 
D <- diag(svd.Z$d)
F <- (sqrt(n-1)*U)[,c(1,3)]  # Factor Scores Matrix : F
L <- (sqrt(1/(n-1))*V%*%D)[,c(1,3)] # Factor Loadings Matrix : Lambda
C<- rbind(F, L)
rownames(F)<-rownames(X)
rownames(L)<-colnames(X)

# Godness-of-fit
eig <- (svd.Z$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[c(1,3)]) 
per
gof


# Biplot: Joint Plot of Factor Loadings and Scores #회전 안했을 때
par(mfrow=c(1,2))
par(pty="s")
lim1 <- range(pretty(L))
lim2 <- range(pretty(F))
biplot(F,L, xlab="f1",ylab="f3", main=" (a) Unrotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

# Varimax Rotated Biplot: Joint Plot of Rotated Factor Loadings and Scores
#회전 했을 때

varimax<-varimax(L)
Lt = varimax$loadings 
T=varimax$rotmat
T
Ft= F%*%T
biplot(Ft,Lt, xlab="f1",ylab="f3", main="(b) Varimax Rotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)


#####################################################(2-3)
svd.Z <- svd(Z) 
U <- svd.Z$u    
V <- svd.Z$v 
D <- diag(svd.Z$d)
F <- (sqrt(n-1)*U)[,2:3]  # Factor Scores Matrix : F
L <- (sqrt(1/(n-1))*V%*%D)[,2:3] # Factor Loadings Matrix : Lambda
C<- rbind(F, L)
rownames(F)<-rownames(X)
rownames(L)<-colnames(X)

# Godness-of-fit
eig <- (svd.Z$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[2:3]) 
per
gof





# Biplot: Joint Plot of Factor Loadings and Scores #회전 안했을 때
par(mfrow=c(1,2))
par(pty="s")
lim1 <- range(pretty(L))
lim2 <- range(pretty(F))
biplot(F,L, xlab="f2",ylab="f3", main=" (a) Unrotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

# Varimax Rotated Biplot: Joint Plot of Rotated Factor Loadings and Scores
#회전 했을 때

varimax<-varimax(L)
Lt = varimax$loadings 
T=varimax$rotmat
T
Ft= F%*%T
biplot(Ft,Lt, xlab="f2",ylab="f3", main="(b) Varimax Rotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)