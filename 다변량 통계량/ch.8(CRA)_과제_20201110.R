# Simple CRA : Two-Way Table
O<-matrix(c(91, 90, 51,
            150, 200, 155,
            109, 198, 172), byrow=T, nrow=3)
chisq.test(O)
# 나이에 따른 유방자가 진단 빈도는 서로 연관성이 없다 (서로 독립).
#p-value = 4.835e-05 귀무가설 기각 
# -> 나이에 따른 유방 자가 진단 빈도는 서로 연관성이 있다.
O

F <- O/sum(O) #상대도수의 이원분할표로 이용
#apply(object, direction, function)#
#direction: 1(행방향), 2(열방향)#
r <- apply(F,1,sum)
c <- apply(F,2,sum)
r;c;

#행과 열의 주변 비율#
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


#A:행좌표점, B:열좌표점#
A <- (Dr%*%U%*%D)[,1:2] #4행 4열에서 2개만 선택 
B <- (Dc%*%V%*%D)[,1:2]
rownames(A) <- c("<45", "45-49", "50+")
rownames(B) <- c("monthly", "frequently", "do not")
A;B
#대응분석도 차원축소의 한 방법


#고유값과 설명력# eigenvalue로 설명력 확인
eig <- (svd.Y$d)^2
per <- eig/sum(eig)*100
gof <- sum(per[1:2])
rbind(round(eig, 3),round(per, 3))
# 1차원의 설명력 99.759 2차원의 설명력 0.241


#행좌표점과 열좌표점 사이의 거리는 기하적으로 의미없음#
#두 좌표점이 같은 방향에 위치한다면 이들이 나타내는 행범주와 열범주가 대응관계에 있다고 봄#
par(pty="s")
lim <-range(pretty(A))
plot(B[, 1:2], xlab="Dim1(99.759%)",ylab="Dim2(0.241%)",xlim=lim,ylim=lim,pch=15,col=2,
     main="SCRA Algorithm : 이원분할표")#행좌표점-혼전 성관계를 반대하는 것
text(B[, 1:2],rownames(B),cex=0.8,col=2,pos=3)
points(A[, 1:2],pch=16, col=4)#열좌표점-피임허용에 대한 자료
text(A[, 1:2],rownames(A),cex=0.8,pos=3, col=4)
abline(v=0,h=0)


#행좌표점과 열좌표점의 거리는 상관은 없고 방향성을 알아봄(중요)

# "<45", "45-49","monthly" / "frequently", "do not", "50+" 같은 방향성
# 50세 미만인 "<45", "45-49"와 유방 자가 진단 빈도가 매달인 점이 같은 방향성으로 상관을 띔
# 50대 이상과 유방 자가 진단 빈도가 수시나 하지않는다가 같은 방향을 띄고 있다.

# 유방자가진단 빈도는 나이가 낮을수록 높아지고 나이가 높을수록 자가진단빈도가 낮아 진다는것에 의미가 있다.


O<-matrix(c(91, 90, 51,
            150, 200, 155,
            109, 198, 172), byrow=T, nrow=3)
rownames(O)<-c("<45", "45-49", "50+")
colnames(O)<-c("monthly", "frequently", "do not")
O
install.packages("ca")
library(ca)
sca=ca(O)
sca
par(pty="s")
plot(sca, main="SCRA package ca : 이원분할표")

# CRA를 위한 R의 함수 ca()의 활용했을때도 위와 동일한 결과를 나타낸다.

#################################################################
AZT<-array(c(14,32,93,81,11,12,52,43),
           dim=c(2, 2, 2),
           dimnames=list(AZT_Use=c("Yes", "No"),
                            AIDS_Symptoms =c("Yes", "No"),
                            response =c("White", "Black")))

mantelhaen.test(AZT) 
# H0 인종에 따라 AZT를 사용함에 따른 AIDS 증상이 독립이다.
# p-value = 0.01367 귀무가설을 기각 서로 독립이 아님 서로 연관성이 있다
# 인종에 따라 AZT를 사용함에 따른 AIDS 증상에 차이가 있다.

#인종에 따라 table을 분할하지 않고 AZT_Use와 AIDS_Symptoms 두변수에 대해서만 고려하고 싶으므로
#각각의 변수에 해당하는 값을 더함
apply(AZT,c(1,2),sum)
AZT_sum=apply(AZT,c(1,2),sum)

chisq.test(AZT_sum)
# H0 AZT를 사용함에 따른 AIDS 증상은 서로 연관성이 없다(서로 독립).
# p-value = 0.01299 귀무가설 기각 
# -> AZT를 사용함에 따른 AIDS 증상은 서로 연관성이 있다.

O=AZT_sum

F <- O/sum(O) #상대도수의 이원분할표로 이용
#apply(object, direction, function)#
#direction: 1(행방향), 2(열방향)#
r <- apply(F,1,sum)
c <- apply(F,2,sum)


#행과 열의 주변 비율#
Dr<- diag(1/sqrt(r))
Dc<- diag(1/sqrt(c))
r;c;Dr;Dc
cF<- F-r%*%t(c)
Y <- Dr%*%(cF)%*%Dc
svd.Y <- svd(Y)
U <- svd.Y$u
V <- svd.Y$v
D <- diag(svd.Y$d)


#A:행좌표점, B:열좌표점#
A <- (Dr%*%U%*%D)[,1:2]  
B <- (Dc%*%V%*%D)[,1:2]
rownames(A) <- c("AZT_USE_Yes", "AZT_USE_No")
rownames(B) <- c("AIDS_Symptoms_Yes", "AIDS_Symptoms_No")
A;B
#대응분석도 차원축소의 한 방법


#고유값과 설명력# eigenvalue로 설명력 확인
eig <- (svd.Y$d)^2
per <- eig/sum(eig)*100
gof <- sum(per[1:2])
rbind(round(eig, 3),round(per, 3))
# 1차원의 설명력 100 2차원의 설명력 0


#행좌표점과 열좌표점 사이의 거리는 기하적으로 의미없음#
#두 좌표점이 같은 방향에 위치한다면 이들이 나타내는 행범주와 열범주가 대응관계에 있다고 봄#
par(pty="s")
lim <-range(c(-0.3,0.3))
plot(B[, 1:2], xlab="Dim1(100%)",ylab="Dim2(0%)",xlim=lim,ylim=lim,pch=15,col=2,
     main="SCRA Algorithm : 이원분할표")#행좌표점-혼전 성관계를 반대하는 것
text(B[, 1:2],rownames(B),cex=0.8,col=2,pos=3)
points(A[, 1:2],pch=16, col=4)#열좌표점-피임허용에 대한 자료
text(A[, 1:2],rownames(A),cex=0.8,pos=3, col=4)
abline(v=0,h=0)


#행좌표점과 열좌표점의 거리는 상관은 없고 방향성을 알아봄(중요)

# AZT_USE_Yes, AIDS_Symptoms_No/ AZT_USE_No, AIDS_Symptoms_Yes 같은 방향성
# AZT를 투약했을때와 AIDS의 증상이 발현되지않는 경우가 같은 방향성으로 상관을띔
# AZT를 투약하지 않을때와 AIDS의 증상이 발되는 경우가 같은 방향성으로 상관을띔

# AZT의 투약을 하면 AIDS의 증상이 없어진다는것에 의미가 있다.











