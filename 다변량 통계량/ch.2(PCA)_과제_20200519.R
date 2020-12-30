##################################################1
setwd("D:/2020 1학기 정호재/다변량통계학(1)/200402 다변량 실습1/Rdata")
data<-read.table("airpollution.txt", header=T)
rownames<-rownames(data) 
head(data)
dim(data)

S<-cov(data)
S
R<-cor(data)
R
eigen.S<-eigen(S)
D_S<-round(eigen.S$values,2)
V_S<-round(eigen.S$vectors,2)

eigen.R<-eigen(R)
D_R<-round(eigen.R$values,2)
V_R<-round(eigen.R$vectors,2)

gof_S<-D_S/sum(D_S)*100
round(gof_S, 2)
plot(D_S, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")
gof_R<-D_R/sum(D_R)*100
round(gof_R,2)
plot(D_R, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

V_S2<-V_S[,1:2]
round(V_S2,2)

V_R2<-V_R[,1:3]
round(V_R2,2)

Y<-scale(data, scale=F) 
P_S<-Y%*%V_S2
P_S
Z<-scale(data,scale=T) 
P_R<-Z%*%V_R2

plot(P_S[,1], P_S[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P_S[,1], P_S[, 2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)


par(mfrow=c(1,3))
plot(P_R[,1], P_R[,2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P_R[,1], P_R[,2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P_R[,1], P_R[,3], main="Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P_R[,1], P_R[,3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P_R[,2], P_R[,3], main="Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P_R[,2], P_R[,3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
###########################################################
X=data
pca.R<-princomp(X, cor=T)
summary(pca.R, loadings=T)
round(pca.R$scores, 3)
screeplot(pca.R, type="lines")

# Principle component biplot (SVD)
par(mfrow=c(1,3))
biplot(pca.R, choices=c(1,2), scale=0,  xlab="1st PC",ylab="2nd PC",
       main="1PC vs 2PC biplot function")
abline(v=0, h=0)
biplot(pca.R, choices=c(1,3), scale=0,  xlab="1st PC",ylab="3nd PC",
       main="1PC vs 3PC biplot function")
abline(v=0, h=0)
biplot(pca.R, choices=c(2,3), scale=0,  xlab="2st PC",ylab="3nd PC",
       main="2PC vs 3PC biplot function")
abline(v=0, h=0)
#############################3
X=data
n <- nrow(X) 
rownames(X)
colnames(X)

Y <- scale(X,scale=T)

svd.Y <- svd(Y) 
U <- svd.Y$u    
V <- svd.Y$v 
D <- diag(svd.Y$d)
G <- (sqrt(n-1)*U)[,1:2]
H <- (sqrt(1/(n-1))*V%*%D)[,1:2]
rownames(G)<-rownames(X)
rownames(H)<-colnames(X) 

biplot(G,H, xlab="1st PC",ylab="2nd PC", main="1PC vs 2PC biplot function",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

G <- (sqrt(n-1)*U)[,c(1,3)]
H <- (sqrt(1/(n-1))*V%*%D)[,c(1,3)]
rownames(G)<-rownames(X)
rownames(H)<-colnames(X) 

biplot(G,H, xlab="1st PC",ylab="3nd PC", main="1PC vs 3PC biplot function",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

G <- (sqrt(n-1)*U)[,2:3]
H <- (sqrt(1/(n-1))*V%*%D)[,2:3]
rownames(G)<-rownames(X)
rownames(H)<-colnames(X) 

biplot(G,H, xlab="2st PC",ylab="3nd PC", main="2PC vs 3PC biplot function",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

########################################################################2

setwd("D:/2020 1학기 정호재/다변량통계학(1)/200402 다변량 실습1/Rdata")
data<-read.table("trackrecord2005-men.txt", header=T)
rownames<-rownames(data) 
head(data)
dim(data)

S<-cov(data)
S
R<-cor(data)
R
eigen.S<-eigen(S)
D_S<-round(eigen.S$values,2) # Eigenvalus
V_S<-round(eigen.S$vectors,2) # Eigenvaectors

eigen.R<-eigen(R)
D_R<-round(eigen.R$values,2) # Eigenvalus
V_R<-round(eigen.R$vectors,2) # Eigenvaectors

gof_S<-D_S/sum(D_S)*100 # Goodness-of fit
round(gof_S, 2)
plot(D_S, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")
gof_R<-D_R/sum(D_R)*100
round(gof_R,2)
plot(D_R, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

V_S2<-V_S[,1:2]  #one selected eigen vectors corresponding eigen values
round(V_S2,2)

V_R2<-V_R[,1:2]
round(V_R2,2)

Y<-scale(data, scale=F) # Centred Data Matrix
P_S<-Y%*%V_S2            # PCs Scores
P_S
Z<-scale(data,scale=T) 
P_R<-Z%*%V_R2

plot(P_S[,1], P_S[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P_S[,1], P_S[, 2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)


plot(P_R[,1], P_R[,2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P_R[,1], P_R[,2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

###############################
rank(-sort(P_R[,1],decreasing = T))

X=data
pca.R<-princomp(X, cor=T)
summary(pca.R, loadings=T)
round(pca.R$scores, 3)
screeplot(pca.R, type="lines")

# Principle component biplot (SVD)
biplot(pca.R, scale=0,  xlab="1st PC",ylab="2nd PC",
       main="1PC vs 2PC biplot function")
abline(v=0, h=0)

#################################
X=data
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

biplot(G,H, xlab="1st PC",ylab="2nd PC", main="1PC vs 2PC biplot function",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)