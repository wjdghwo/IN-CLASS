library(ISLR)
data(USArrests)
str(USArrests)
USArrests[1:10,]

states <- row.names(USArrests)
states

apply(USArrests, 2, mean)
apply(USArrests, 2, sd)

pr.out <- prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation
pr.out$x






## scale X matrix so that mean=0, sd=1
scale.X <- scale(USArrests, scale=TRUE)
apply(scale.X, 2, mean)
apply(scale.X, 2, sd)

## singular value decomposition of X
s <- svd(scale.X)

## eigen decomposition of covariance X
XX <- cov(scale.X)
e <- eigen(XX)

## eigen vector is equal to PCA loadings
s$v
e$vec
pr.out$rotation

## eigne vector * x is equal to PCA score
scale.X %*% e$vec
pr.out$x








biplot(pr.out, scale=0)
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale =0)










par(mfrow=c(1, 2))
pr.out <- prcomp(USArrests, scale=TRUE) # scale 했을 때
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale=0, col=c("darkblue", "darkred"),
       xlabs=rep("*",50), xlab="First Principal Component",
       ylab="Second Principal Component", main="Scaled")
abline(h=0, lty=3, col="grey")
abline(v=0, lty=3, col="grey")
pr.out2 <- prcomp(USArrests, scale=FALSE) # scale 안 했을 때
biplot(pr.out2, scale=0, col=c("darkblue", "darkred"),
       xlabs=rep("*",50), xlab="First Principal Component",
       ylab="Second Principal Component", main="Unscaled")
abline(h=0, lty=3, col="grey")
abline(v=0, lty=3, col="grey")






pr.out$sdev
apply(pr.out$x, 2, sd)
pr.var <- pr.out$sdev^2
pr.var
apply(pr.out$x, 2, var)
pve <- pr.var/sum(pr.var)
pve
par(mfrow=c(1, 2))
plot(pve, xlab="Principal Component",
     ylab="Prop. of Variance Explained",
     ylim=c(0,1), type="b", col="darkblue")
plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative Prop. of Variance Explained",
     ylim=c(0,1), type="b", col="darkblue")








set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
x

y <- c(rep(2, 25), rep(1, 25))
plot(x, col=(y+1), pch=20, cex=2)

km.out <- kmeans(x, 2, nstart=20)
km.out
km.out$cluster

plot(x, col=(km.out$cluster +1),
     main="K-Means Clustering Results with K=2",
     xlab="", ylab="", pch=20, cex=2)
set.seed(4)
km.out <-kmeans(x, 3, nstart=20)
km.out






plot(x, col=(km.out$cluster +1),
     main="K-Means Clustering Results with K=3",
     xlab="", ylab="", pch=20, cex=2)

set.seed(111)
km.out <- kmeans(x, 3, nstart=1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart=20)
km.out$tot.withinss

set.seed(12345)
par(mfrow=c(2, 3))
tot <- 0
for (i in 1:6) {
  km.out <- kmeans(x, 3, nstart=1)
  tot[i] <- round(km.out$tot.withinss, 4)
  plot(x, col=(km.out$cluster +1), pch=20, cex=2, xlab="",
       ylab="", main=paste("(", tot[i], ")"))
}








set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
dist(x) # defalt euclidean distance
dim(dist(x)) # dim이 없음
as.matrix(dist(x))[1:8,1:8] # symm

y <- c(rep(2, 25), rep(1, 25))
plot(x, col=(y+1), pch=20, cex=2)

hc.complete <- hclust(dist(x), method="complete")
hc.average <- hclust(dist(x), method="average")
hc.single <- hclust(dist(x), method="single")
hc.cent <- hclust(dist(x), method="centroid")

par(mfrow =c(2,2))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex =.9)
plot(hc.average, main="Average Linkage", xlab="", sub ="", cex =.9)
plot(hc.single, main="Single Linkage", xlab="", sub ="", cex =.9)
plot(hc.cent, main="Centroid Linkage", xlab="", sub ="", cex =.9)









cutree(hc.complete, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

x2 <- x; x2[ ,2] <- x[ ,2]*100
par(mfrow=c(1,3))
plot(hclust(dist(scale(x)), method="complete"), xlab="",
     sub="", main="Complete Linkage with Scaled Features")
plot(hclust(dist(scale(x2)), method="complete"), xlab="",
     sub="", main="Complete Linkage with Scaled Features")
plot(hclust(dist(x2), method="complete"), xlab="",
     sub="", main="Complete Linkage with Unscaled Features")
# scale을 하지 않으면 값이 영향을 받음음


par(mfrow=c(1,2))
x <- matrix(rnorm(30*3), ncol=3)
plot(hclust(dist(scale(x)), method="complete"), xlab="", sub="",
     main="Complete Linkage with Euclidean-Based Distance")
plot(hclust(as.dist(1-cor(t(x))), method="complete"), xlab="",
     sub="", main="Complete Linkage with Correlation-Based Distance")









library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)
nci.labs[1:4]
table(nci.labs)
summary(as.numeric(nci.data))

pr.out <- prcomp(nci.data, scale=TRUE) #pca 진행
Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow=c(1,2))
plot(pr.out$x[ ,1:2], col=Cols(nci.labs), pch=19, xlab ="Z1",
     ylab="Z2")
plot(pr.out$x[ ,c(1,3)], col=Cols(nci.labs), pch=19,
     xlab="Z1", ylab="Z3")










summary(pr.out)
plot(pr.out) # variance계산

pve <- 100* pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
     col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE",
     xlab="Principal Component", col="brown3")

sd.data <- scale(nci.data, scale=TRUE)
par(mfrow=c(3,1))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels=nci.labs,
     main="Complete Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs,
     main="Average Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,
     main="Single Linkage", xlab="", sub="", ylab="")









hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4) # 4개의 그룹으로 분할
table(hc.clusters, nci.labs)
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs, xlab="", sub="")
abline(h=139, col="red", lty=2)
hc.out

set.seed(2)
km.out <- kmeans(sd.data, 4, nstart=20) #k-mean
km.clusters <- km.out$cluster
table(km.clusters, nci.labs)

par(mfrow=c(1,3))
plot(pr.out$x[ ,1:2], col=Cols(nci.labs), pch=19, xlab ="Z1",
     ylab="Z2", main="Original data")
plot(pr.out$x[ ,1:2], col=Cols(km.clusters), pch=19, xlab ="Z1",
     ylab="Z2", main="K-means clustering")
plot(pr.out$x[ ,1:2], col=Cols(hc.clusters), pch=19, xlab="Z1",
     ylab="Z2", main="Hierarchical clustering")








newx <- pr.out$x[ ,1:2]
hc.out <- hclust(dist(newx))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

set.seed(2)
km.out <- kmeans(newx, 4, nstart=20)
km.clusters <- km.out$cluster
table(km.clusters, nci.labs)

par(mfrow=c(1,3))
plot(pr.out$x[ ,1:2], col=Cols(nci.labs), pch=19, xlab ="Z1",
     ylab="Z2", main="Original data")
plot(pr.out$x[ ,1:2], col=Cols(km.clusters), pch=19, xlab ="Z1",
     ylab="Z2", main="K-means clustering")
plot(pr.out$x[ ,1:2], col=Cols(hc.clusters), pch=19, xlab="Z1",
     ylab="Z2", main="Hierarchical clustering")









