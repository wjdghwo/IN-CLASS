setwd("H:/학교/2020 2학기 정호재/다변량통계학2/실습/20201029/Rdata")
Data7.7.2<-read.table("color3.txt", header=T)
Data7.7.2
C<-as.matrix(Data7.7.2)
color=colnames(C)
n<-nrow(C)

# Standard Transformation : cij(similarity) to dij(dissimilarity)
J<-matrix(1,n,n)
cii=diag(diag(C))%*%J
cij=C
cjj=J%*%diag(diag(C))
D<-sqrt(cii-2*cij+cjj)
D

# Non-Metric MDS
library(MASS)
con<-isoMDS(D, k=2)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y))-0.2, max(abs(y)))

plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,color, cex=0.8, pos=1)
abline(v=0,h=0)

# Shepard Diagram
color_sh <- Shepard(D[lower.tri(D)], con$points)
color_sh
plot(color_sh, pch = ".", xlab = "Dissimilarity", ylab = "Distance", 
     xlim = range(color_sh$x), ylim = range(color_sh$x))
lines(color_sh$x, color_sh$yf, type = "S")

# image Diagram
ccolor_sh=cbind(color_sh$x, color_sh$y, color_sh$yf)
ccolor_sh
plot(ccolor_sh[,2], ccolor_sh[,3], pch = ".", xlab = "FitDissimilarity", ylab = "Distance", 
     xlim = range(ccolor_sh[,2]), ylim = range(ccolor_sh[,2]))
lines(ccolor_sh[,2],  ccolor_sh[,3], type = "p")

# Metric MDS
con<-cmdscale(D, k=2, eig=T)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))
plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,color, cex=0.8, pos=1)
abline(v=0,h=0)

#####################################
Data7.7.1<-read.table("railroad2.txt", header=T)
Data7.7.1
D<-as.matrix(Data7.7.1)
city=colnames(D)
n<-nrow(D)

library(MASS)
con<-isoMDS(D, k=2)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y))-0.2, max(abs(y)))

plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,city, cex=0.8, pos=1)
abline(v=0,h=0)

# Shepard  Diagram
railroad_sh <- Shepard(D[lower.tri(D)], con$points)
railroad_sh
plot(railroad_sh, pch = ".", xlab = "Dissimilarity", ylab = "Distance", 
     xlim = range(railroad_sh$x), ylim = range(railroad_sh$x))
lines(railroad_sh$x, railroad_sh$yf, type = "S")

# image  Diagram
crailroad_sh=cbind(railroad_sh$x, railroad_sh$y, railroad_sh$yf)
crailroad_sh
plot(crailroad_sh[,2], crailroad_sh[,3], pch = ".", xlab = "FitDissimilarity", ylab = "Distance", 
     xlim = range(crailroad_sh[,2]), ylim = range(crailroad_sh[,2]))
lines(crailroad_sh[,2],  crailroad_sh[,3], type = "p")

# Metric MDS
con<-cmdscale(D, k=2, eig=T)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))
plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,city, cex=0.8, pos=1)
abline(v=0,h=0)
