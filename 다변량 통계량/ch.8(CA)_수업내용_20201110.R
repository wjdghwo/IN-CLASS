# 12/3일 기말
# 12/19`21일까지 메일로 term project 받을예정`
#대응분석-단순대응부석+다중대응분석
#CRA:분할표로 나타내어지는 자료의 행과 열범주를 저차원 공간상(2차원)의 점들로 동시에 나타내어, 그들의 관계를 탐구#
#단순대응분석 : *행과 열범주를 나타내는 변수가 둘뿐인* 이원분할표의 대응분석#
# Simple CRA : Two-Way Table
#EX1)결혼 전 성관계에 대한 반응과 피임허용의 이원분할표의 단순 CRA 수행단계#
O<-matrix(c(81, 68, 60, 38,
            24, 26, 29, 14,
            18, 41, 74, 42,
            36, 57, 161, 157), byrow=T, nrow=4)
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
A <- (Dr%*%U%*%D)[,1:2] #4행 4열에서 2개만 선택 
B <- (Dc%*%V%*%D)[,1:2]
rownames(A) <- c("매우나쁨", "거의나쁨", "조금나쁨", "전혀안나쁨")
rownames(B) <- c("매반", "반대", "찬성", "매찬")
A;B
#대응분석도 차원축소의 한 방법


#고유값과 설명력# eigenvalue로 설명력 확인
eig <- (svd.Y$d)^2
per <- eig/sum(eig)*100
gof <- sum(per[1:2])
rbind(round(eig, 3),round(per, 3))
# 1차원의 설명력 92.055 2차원의 설명력 7.941


#행좌표점과 열좌표점 사이의 거리는 기하적으로 의미없음#
#두 좌표점이 같은 방향에 위치한다면 이들이 나타내는 행범주와 열범주가 대응관계에 있다고 봄#
par(pty="s")
lim <-range(pretty(A))
plot(B[, 1:2], xlab="Dim1",ylab="Dim2",xlim=lim,ylim=lim,pch=15,col=2,
     main="SCRA Algorithm : 이원분할표")#행좌표점-혼전 성관계를 반대하는 것
text(B[, 1:2],rownames(B),cex=0.8,col=2,pos=3)
points(A[, 1:2],pch=16, col=4)#열좌표점-피임허용에 대한 자료
text(A[, 1:2],rownames(A),cex=0.8,pos=3, col=4)
abline(v=0,h=0)


#행좌표점과 열좌표점의 거리는 상관은 없고 방향성을 알아봄(중요)
#혼전성관계를 반대하는 것과 피임을 반대하는쪽 이 상관
#피임허용 나쁘지 않고 혼전성관계를 찬성하는 쪽이 같은 방향

#혼전성관계를 나쁘지 않게 생각하는 것이 피임허용을 나쁘지 않게 생각하는 것에 의미가 있다.



#EX1)에서 CRA를 위한 R의 함수 ca()의 활용#
# Simple CRA ca() : Matrix for Two-Way Table
O<-matrix(c(81, 68, 60, 38,
            24, 26, 29, 14,
            18, 41, 74, 42,
            36, 57, 161, 157), byrow=T, nrow=4)
rownames(O)<-c("매우나쁨", "거의나쁨", "조금나쁨", "전혀안나쁨")
colnames(O)<-c("매반", "반대", "찬성", "매찬")
install.packages("ca")
library(ca)
sca=ca(O)
par(pty="s")
plot(sca, main="SCRA package ca : 이원분할표")

# Simple CRA ca() : Text for Two-Way Table
setwd("G:/학교/2020 2학기 정호재/다변량통계학2/실습/20201110/Rdata")
Data8.2.1<-read.table("presex.txt", header=T)
O=Data8.2.1
library(ca)
sca=ca(O)
sca

win.graph()
par(pty="s")
plot(sca, main="SCRA package ca : 이원분할표")


#지위에 따른 흡연습관의 이원분할표에 대한 단순 CRA#
# Simple CRA: smoker data
library(ca)
data(smoke)
O=smoke
sca=ca(O)
sca
#1차원 87.76% 2차원설명력 11.76

win.graph()
par(pty="s")
plot(sca, main="SCRA: 지위와 흡연습관 분할표")
#왼쪽을 담배를 피지않는쪽 오른쪽을 담배를 피는쪽 담패를 파지않을수록 상위 지위


#연관 및 독립성검정-카이제곱
#결혼 전 성관계에 대한 반응과 피임허용의 독립성과 동질성 검정#
#H_0 : 혼전 성관계에 대한 반응과 10대들의 피임에 대한 생각은 서로 연관성이 없다.(서로 독립)
setwd("C:/Users/user/Desktop/2019 다변량통계2/191121/Rdata")
Data8.2.1<-read.table("presex.txt", header=T)
O=Data8.2.1
chisq.test(O)
#p-value < 2.2e-16 귀무가설 기각 -> 혼전 성관계에 대한 반응과 10대들의 피임에 대한 생각은 서로 연관성이 있다.
#대응분석 후 카이제곱검정을 하면 신빙성을 더 불어넣어줄 수 있다.
#여기까지 다음시간 단순대응분석 삼원 사원에 대한 다응대응분석을 함



# Chi-square test for Three Ways Table 
#삼원분할표에 대한 
## Customer Data 
customer <-array(c(1,1,2,1,3,2,1,1),    
                 dim=c(2, 2, 2), 
                 dimnames=list(sex=c("male", "female"),
                               age =c("Old", "Young"),
                               response =c("Tall", "Short")))#반응변수 설정 을 잘해야함 이것에 따라 결과가 달라짐


#키가 큰 아이들에 대한 성별과 나이로 만든것
#키가 작은 아이들에 대한 성별과 나이로 만든것
#귀무가설 이것들이 독립이다.

customer
mantelhaen.test(customer) 
# p-value = 0.948 귀무가설을 기각할수없으므로 서로 독립
#공통 오즈비로 테스트

#sample estimates:
#  common odds ratio 
#0.9166667 



## Driver Injury Data #음주운전과 자동차 사고에 대한 자료
driver<-array(c(12500, 313, 61971, 3992,
                604, 43, 3519, 481,
                344, 15, 2272, 370,
                38, 4, 237,66), dim=c(2, 2, 4),
              dimnames=list(condition=c("Normal", "Drinking"),#음주여부
                            Belt =c("Yes", "No"),#안전벨트여부
                            response =c("None", "Minimal", "Minor", "Major")))#부상의 정도
driver
# p-value < 2.2e-16 귀무가설을 기각 서로 독립이 아님 서로 연관성이 있다

mantelhaen.test(driver)




## Driver Injury Tabel Data
acdtable<-read.table("accidenttable.txt", header=T)

#이렇게 생긴 데이터를 테이블 형태로 바꿔주는 방법
table=xtabs(빈도 ~ 운전 + 벨트 + 부상, data=acdtable)
mantelhaen.test(driver)


