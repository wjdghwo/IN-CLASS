#CRA:분할표로 나타내어지는 자료의 행과 열범주를 저차원 공간상(2차원)의 점들로 동시에 나타내어, 그들의 관계를 탐구#
#다중대응분석 : 다원분할표를과로 나타낸 표시행렬(indicator matrix)나 Burt행렬 의 대응분석#
#n*q표시행렬=>q*q버트행렬=>대응행렬=>잔차행렬의 비정칙값분해=>다중cra그림의 좌표점,설명력#

setwd("C:/Users/user/Desktop/다변량 수업/2020 2학기/20201117/Rdata")

# [보기 8.4.1] 성별, 나이, 키에 따른 삼원분할표의 다중 CRA#
# MCRA : Burt Table from Indicator Matrix
Z<-matrix(c(1, 0, 1, 0, 1, 0,
            1, 0, 1, 0, 0, 1,
            1, 0, 1, 0, 0, 1,
            1, 0, 1, 0, 0, 1,
            1, 0, 0, 1, 1, 0,
            1, 0, 0, 1, 1, 0,
            1, 0, 0, 1, 0, 1,
            0, 1, 1, 0, 1, 0,
            0, 1, 1, 0, 0, 1,
            0, 1, 1, 0, 0, 1,
            0, 1, 0, 1, 1, 0,
            0, 1, 0, 1, 0, 1), byrow=T, nrow=12)
colnames(Z)<-c("남", "여", "중장년", "청소년", "키큼", "키작음")
Z
B <- t(Z) %*% Z 
B 
P <- B / sum(B)
#apply(object, direction, function)#
#direction: 1(행방향), 2(열방향)#
cm <- apply(P, 2, sum)
Dc<-diag(1/sqrt(cm))
eP <- cm %*% t(cm)
Y <- (P - eP) / sqrt(eP)
## Singular Value Decomposition
svd.Y<-svd(Y)
V<-svd.Y$v
Dl<-diag((svd.Y$d)^2)
lam<-(svd.Y$d)^2
fit<-lam/sum(lam)*100
rbind(round(lam, 3),round(fit, 3))
Cb<- Dc%*%V%*%Dl
rownames(Cb)<-colnames(Z)
Cb2=Cb[, 1:2]
Cb2
#다중cra에서는 열좌표점만 제공#
par(pty="s")
lim<-range(pretty(Cb))
plot(Cb2, xlab="Dim1", ylab="Dim2", xlim=lim, ylim=lim, main="MCRA algorithm : Burt Matrix")
text(Cb2, colnames(Z), pos=3, col=1)
abline(v=0, h=0)

#제2축에서는 남과 여가 서로 반대쪽에 놓여 있어 기하적으로 이들 범주의 이질성을 나타내고 있다. #

#고객들의 경향은 남자 고객은 청소년과 키가 크며 여자 고객은 그 반대의 경향과 대응하고 있다#


# [보기 8.4.2] 사고운전자 부상정도의 삼원분할표에 대한 다중 CRA 알고리즘#
accident<-read.table("accident.txt", head=T) 
B<-as.matrix(accident)
rownames(B)<-colnames(B)
B
P <- B / sum(B)
cm <- apply(P, 2, sum)
Dc<-diag(1/sqrt(cm))
eP <- cm %*% t(cm)
Y <- (P - eP) / sqrt(eP)

## Singular Value Decomposition
svd.Y<-svd(Y)
V<-svd.Y$v
Dl<-diag((svd.Y$d)^2)
lam<-(svd.Y$d)^2
fit<-lam/sum(lam)*100
rbind(round(lam, 3),round(fit, 3))
Cb<- Dc%*%V%*%Dl
rownames(Cb)<-colnames(B)
Cb2=Cb[, 1:2]
Cb2
limy<-range(pretty(Cb2))
limx<-c(-1, 1)
plot(Cb2, xlab="Dim1", ylab="Dim2", xlim=limx, ylim=limy, main="MCRA algorithm : Burt Matrix")
text(Cb2, rownames(Cb), col=1,  pos=3)
abline(v=0, h=0)


# [보기 8.4.3] 인구통계적 변인과 사회경제적 변인에 의한 버트표#
sociecono<-read.table("sociecono.txt", head=T) 
B<-as.matrix(sociecono)
colnames<-colnames(B)
B
P <- B / sum(B)
cm <- apply(P, 2, sum)
Dc<-diag(1/sqrt(cm))
eP <- cm %*% t(cm)
Y <- (P - eP) / sqrt(eP)

## Singular Value Decomposition
svd.Y<-svd(Y)
V<-svd.Y$v
Dl<-diag((svd.Y$d)^2)
lam<-(svd.Y$d)^2
lam
fit<-lam[1:2]/sum(lam)*100
rbind(round(lam[1:2], 3),round(fit, 3))
Cb<- Dc%*%V%*%Dl
Cb2=Cb[, 1:2]
rownames(Cb2)<-colnames
round(Cb2, 3)
par(pty="s")
lim<-range(pretty(Cb))
plot(Cb2, xlab="Dim1", ylab="Dim2", xlim=lim, ylim=lim, main="MCRA algorithm : Burt Matrix")
text(Cb2, colnames, col=2,  pos=3)
abline(v=0, h=0)


###########################

group <- as.factor(c(rep(1,2),rep(2,5),rep(3,4),rep(4,2),rep(5,2),rep(6,5),rep(7,6)))

plot(Cb2, xlab="Dim1", ylab="Dim2", xlim=lim, ylim=lim, main="MCRA algorithm : Burt Matrix")
text(Cb2, colnames, col=c(1:7)[group],  pos=3)
abline(v=0, h=0)


###########################

# [보기 8.4.4] 삼원분할표 사고운전자 부상정도 입력자료에 대한 다중CRA#
acdtable<-read.table("accidenttable.txt", header=T)
library(ca)
table=xtabs(빈도 ~ 벨트 +  부상 + 운전, data=acdtable)
mjca(table)
plot(mjca(table), main="MCRA : 삼원분할표")

# [보기 8.4.5] 삼원불할표에 대한 다중 CRA
Data1.3.6<-read.table("sucidefreq.txt")
Data1.3.6
Freq<-as.matrix(Data1.3.6)

sex<-c("Male", "Female")
age<- c("10-20", "25-35", "40-50", "55-65", "70-90")
method<-c("Poison", "Gas",  "Hang", "Drown", "Gun", "Jump")
?expand.grid
data<-expand.grid(Sex=sex, Age=age, Method=method)
data<-cbind(data, Freq)

# Makeing a 3-way table
table<-xtabs(Freq~Sex+Age+Method, data=data)
plot(mjca(table), main="MCRA : 성별, 나이, 자살유형의 삼원분할표")

# [보기 8.5.2] 외상 후 스트레스 증후군에 대한 다시점자료의 다중 CRA

ptsd<-read.table("ptsd.txt", header=T)
v1 <- as.numeric(ptsd$자아통제력>=3)
v2 <- as.numeric(ptsd$인생문제수>=3)
v3<- as.numeric(ptsd$스트레스수>=3)
v4 <- as.numeric(ptsd$가족결속력>=6)
nptsd <- data.frame(ptsd[,1:3],자아통제력=v1,인생문제수=v2, 스트레스수=v3, 가족결속력=v4)
nptsd<-nptsd[, -1] 
library("ca")
mjca(nptsd)
par(pty="s")
plot(mjca(nptsd), main="MCRA : 스트레스증후군 PTSD")


