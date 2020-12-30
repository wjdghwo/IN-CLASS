setwd("E:/학교/2020 2학기 정호재/다변량통계학2/실습/20201117/Rdata")
Freq <-array(c(14,483,1,411,497,100,462,157))
이탈<-c("yes", "no")
사망<- c("yes", "no")
안전벨트<-c("착용","미착용")
data<-expand.grid(이탈=이탈, 사망=사망, 안전벨트=안전벨트)
accident<-cbind(data, Freq)
accident

table<-xtabs(Freq~이탈+사망+안전벨트, data=accident)
table

#install.packages("ca")
library(ca)
mjca(table)
plot(mjca(table),main="MCRA : 3-ways contingency table")
############################################
apitude_burt<-read.table("apitude.txt",header=T)

B<-as.matrix(apitude_burt)
B
P <- B / sum(B)
round(P,3)
cm <- apply(P, 2, sum)  #apply(object, direction, function)#
Dc<-diag(1/sqrt(cm))    #direction: 1(행방향), 2(열방향)#
eP <- cm %*% t(cm)
Y <- (P - eP) / sqrt(eP)
round(Y,3)

# Singular Value Decomposition
svd.Y<-svd(Y)
V<-svd.Y$v
Dl<-diag((svd.Y$d)^2)

lam<-(svd.Y$d)^2
fit<-lam/sum(lam)*100
rbind(round(lam, 3),round(fit, 3))

Cb<- Dc%*%V%*%Dl
rownames(Cb)<-colnames(apitude_burt)

Cb2<-Cb[, 1:2]
Cb2


lim<-range(pretty(Cb))

plot(Cb2, xlab="Dim1", ylab="Dim2", xlim=lim, ylim=lim, main="MCRA algorithm : Burt Matrix")
text(Cb2, colnames(apitude_burt), pos=3, col=1)
abline(v=0, h=0)


###########################################
view<-read.table("view.txt",header=T)
view<-view[,-1]
head(view)

library(ca)
mjca(view)
par(pty="s")
plot(mjca(view), main="MCRA : 성별, 나이 수입에 따른 경제전망과 정책선호도")


