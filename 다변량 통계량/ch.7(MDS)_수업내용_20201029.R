
#다차원 척도법 : 개체들 사이의 유사성/비유사성을 측정하여 2차원 이상의 공간상에 점으로 표현
#개체들간의 거리계산은 주로 유클리드거리 행렬 사용
#계량형 MDS: 데이터가 연속형 변수(구간척도, 비율척도)인 경우사용.
#비계량형 MDS: 데이터가 순서척도인 변수를 가지는 경우 사용
#계량형 mds : cmdscale, 비계량형 mds : isoMDS

# MDS 수행단계
# 1. 개체 간의 비유사성에 의한 n x n '거리행렬(D)'을 구한다. 
# 2. MDS '알고리즘'을 선택 ( 계량형 or 비계량형 )
# 3. MDS '적합도' 계산
# 4. MDS '그림 해석' 및 개체 간의 '군집'을 시도

#economicview data :  Metric MDS
setwd("E:/학교/2020 2학기 정호재/다변량통계학2/실습/20201029/Rdata")
Data1.3.5<-read.table("economicview.txt", header=T)
X<-Data1.3.5[, -1]
Data1.3.5

# Dissimilarity Matrix from Raw Data(거리행렬)
rownames(X)<-Data1.3.5[, 1]
X<-scale(as.matrix(X))
m <-as.matrix(dist(X, method="euclidean", diag=T))
#dist 함수로 거리행렬 구함/ as.matrix를 사용하여 대각원소들도 모두 입력하여 round로 반올림함
#굳이 as.matrix로 안바꿔도 됨
d<-round(m, 3)
d

# Metric MDS 
con<-cmdscale(d, k=2, eig=T)
#cmdscale: matrix 데이터 상의 데이터들의 값을 거리로 생각하고, 2차원상으로 나타냄(k=2)
#eig=T:결과값에 eigenvalue를 나타내줌
con
# eig:eigenvalue, GOF:적합도

x<-con$points[,1]
y<-con$points[,2]
lim<-c(-max(abs(con$points)), max(abs(con$points)))
plot(x,y, xlab="Dimension 1", ylab="Dimension 2", xlim=lim, ylim=lim)
text(x,y+0.6, rownames(d), cex=0.8, pos=1)
abline(v=0, h=0)
#해석은 데이터에대한 정보를 잘 알아야됨 현대자동차와 중앙대학교 정책비판하는 경향높ㅇ므
#전경력 대우경제연구소 비관적인 특성이 있다.
#텀프때 어떤 특성이 있는지 추가해줘야함 그냥 집단간 비슷하다 라고 적으면 애매

# con$GOF 의 의미
?cmdscale
#a numeric vector of length 2, equal to say (g.1,g.2),
#where g.i = (sum{j=1..k} λ[j]) / (sum{j=1..n} T.i(λ[j])),
#where λ[j] are the eigenvalues (sorted in decreasing order), T.1(v) = abs(v), and T.2(v) = max(v, 0).

# Metric mds에 이용되는 토거슨 알고리즘은 eigenvalue가 음수이면 사용 x ,
# 음수의 개수가 적으면 음수인 eigenvalue 값을 절대값 or 0으로 치환하는 방법 사용 음수의 개수가 많으면 비계량형 mds를 적용

con$eig
which(con$eig<0)

sum(con$eig[1:2])/sum(abs(con$eig)) #음수인 eig을 절대값을 씌워서 적합도를 구해봄
tmp = con$eig
tmp[13:14] = 0 
sum(tmp[1:2])/sum(tmp)

c(sum(con$eig[1:2])/sum(abs(con$eig)),sum(tmp[1:2])/sum(tmp) )
con$GOF
# con$GOF 첫번째 값 : 음수 eig을 절대값으로 사용해서 적합도 구함
# con$GOF 두번째 값 : 음수 eig을 0으로 만들어 적합도 구함

# Morse data의 계량형 MDS : 유사성행렬이므로 비유사성행렬로 변환 
# Dissimilarity Matrix from Similarity Matrix
Data7.2.3<-read.table("morse.txt", header=T, check.names=F)
Data7.2.3
# 1이란 모스부호를보고 1부터 10까지 대답한 사람 수 유사성을 나타내는 데이터
C<-as.matrix(Data7.2.3)
부호=colnames(C)
n<-nrow(C)

# Standard Transformation : cij(similarity) to dij(dissimilarity) 
# 유사성행렬을 비유사성 행렬로 만들어줌
J<-matrix(1,n,n)
cii=diag(diag(C))%*%J
cij=C
cjj=J%*%diag(diag(C))
D<-sqrt(cii-2*cij+cjj)
D

# Metric MDS
#win.graph()
con<-cmdscale(D, k=2, eig=T)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))
plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,부호, cex=0.8, pos=1)
abline(v=0,h=0)
#0과 9사이 거리가 가까움(선이 4개, 5개)
# 그림 오른쪽으로 갈때 점이 늘어나고 올라가면서 점이 줄어듬
# MDS는 적합도를 크게 신경을 쓰지 않음 적합도 0.55는 낮은값이지만 큰 신경을 안써도 됨



#은행 경영평가 자료의 계량형 MDS #
# Dissimilarity Matrix from Binary Data (이진데이터)
Data7.2.2<-read.table("bankbinary.txt", header=T) 
Data7.2.2
X<-Data7.2.2[, -1]
은행<-Data7.2.2[, 1] 
n<-nrow(X) # i번째와 j번째 개체 간에 일치하지 않는 항목의 수
p<-ncol(X) # 전체 항목의 수


# dig= i번째와 j번째 개체 간에 일치하지 않는 항목의 수/ 전체 항목의 수
# Dissimilarity Matrix from Binary Data 
m <-as.matrix(dist(X, method="euclidean", diag=T)) #유클리드 거리를 구함
D<-round(m^2, 3)/p #비유사성 행
D

# Metric MDS 
#win.graph() 
con<-cmdscale(D, k=2, eig=T) 
con
# GOF값의 차이가 큼 - 데이터가 2진데이터라서 그런듯하다.
x<-con$points[, 1] 
y<-con$points[, 2] 
lim<-c(-max(abs(con$points)), max(abs(con$points)))
plot(x, y, xlab="Dim1", ylab="Dim2", xlim=lim, ylim=lim) 
text(x, y, 은행, cex=0.8, pos=1) 
abline(v=0, h=0)
#가까이 있는 점들은 유사 떨어진 점들은 다른 특성
#하나,한미,주택,신한 - 인수하는 은행 / 국민,한일,조공,산업 - 경영평가 대상인 은행 / 나머지는 인수가 된 은행
#특성에 따라서 잘 나눠짐 
# 텀프때 의미있는 군집의 특성을 설명해주어야함

# 비계량형 MDS : 데이터가 순서척도인 경우 사용, 음수인 eig이 많을때 많이 사용
# 계량형보다는 비계량형MDS가 더 좋다는 말이 있음

# Nine Jobs's Dissimilarity
Data7.3.1<-read.table("jobs.txt", header=T)
Data7.3.1 #9가지 직업들의 유사을 점수화함, 점수가 클수록 직업끼리 다르다.
D<-as.matrix(Data7.3.1)
직업=colnames(D)

# Nonmetric MDS
#win.graph()
library(MASS)
con<-isoMDS(as.matrix(D), k=2)
con
#stress :차원수 결정을 위한 크루스칼의 판별기준 (p.417)

x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))
plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x+0.5,y,직업, cex=0.8, pos=2)
abline(v=0, h=0)
# 군집은 자신만의 기준이나 사분면으로 나눔

# 비계량형 MDS의 경우 2가지의 그림을 추가로 얻을수 있음
# 비계량형 MDS 알고리즘의 특성(크루스칼 셰퍼드)
#             d12, d13, d14, d23, d24, d34(각 점들간 ㅓ거리)
# 실제값      0.1  0.2  0.4  0.3  0.5  0.6
# 2차원 순위   1    3    2    4    5    6
# 실제값 순위  1    2    3    4    5    6

# 실제값 순위와 2차원 순위를 비교 모두 똑같으면 적합도가 1
# 알고리즘은 크기에 따라 순서화함 크기가 작은건 1번

# Shepard  Diagram 
#win.graph()
dist_sh <- Shepard(D[lower.tri(D)], con$points) #셰퍼드함수
dist_sh
#$x : 실제값
#$y : 2차원순위
#$yf : 실제값 순위
cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
cdist_sh
plot(cdist_sh[,1], cdist_sh[,3], pch = ".", xlab = "Dissimilarity", ylab = "Distance", 
     xlim = range(cdist_sh[,1]), ylim = range(cdist_sh[,1]))
lines(cdist_sh[,1],  cdist_sh[,3], type = "S")
#x실제값 순위
#y거리
#의미 실제값을 실제값 순위로 단조증가 단조증가가 아니면 데이터문제 또는 알고리즘 문제가 있는지 확인
# 단조증가이므로 과정을 잘 만족함

# Image Diagram
# 2차원순위를 그림으로 
#win.graph()
plot(cdist_sh[,2], cdist_sh[,3], pch = ".", xlab = "FitDissimilarity", ylab = "Distance", 
     xlim = range(cdist_sh[,2]), ylim = range(cdist_sh[,2]))
lines(cdist_sh[,2],  cdist_sh[,3], type = "p")
# y차원축소후 순위 x차원축소전 순위
# y=x 일수록 적합도가 높음 벗어날수록 적합도 떨어짐

# 세퍼드그림은 알고리즘의 사용하는 과정을 보여줌
# 이미지그림은 적합도를 보여주는데 사용하는 그림
# y=x꼴로 어느정도 잘나와있으므로 mds그림이 적합이 잘되었다.

#KLPGA 선수의 성적(klpga.txt)에 대한 비계량형 MDS #
Data1.3.2<-read.table("klpga.txt", header=T)
X<-scale(Data1.3.2, scale=T)
X<-as.matrix(Data1.3.2)
선수<-rownames(X) 
n<-nrow(X)
p<-ncol(X)

# 표준화 유클리드거리
D <- as.matrix(dist(X, method="euclidean"))

# Dissimilarity Matrix 
m <-as.matrix(dist(X, method="euclidean", diag=T))
D<-round(m^2, 3)/p

# Nonmetric MDS
#win.graph()
library(MASS)
con<-isoMDS(D, k=2)
con
#적합도 보통
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))
plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,선수, cex=0.8, pos=2)
abline(v=0, h=0)

# win.graph()
# Shepard  Diagram
klpga_sh <- Shepard(D[lower.tri(D)], con$points)
klpga_sh
plot(klpga_sh, pch = ".", xlab = "Dissimilarity", ylab = "Distance", 
     xlim = range(klpga_sh$x), ylim = range(klpga_sh$x))
lines(klpga_sh$x, klpga_sh$yf, type = "S")
#감소하는 부분없이 단조증가 미리 설정한 가정이 위배되지 않는다.

# image  Diagram
cklpga_sh=cbind(klpga_sh$x, klpga_sh$y, klpga_sh$yf)
cklpga_sh
plot(cklpga_sh[,2], cklpga_sh[,3], pch = ".", xlab = "FitDissimilarity", ylab = "Distance", 
     xlim = range(cklpga_sh[,2]), ylim = range(cklpga_sh[,2]))
lines(cklpga_sh[,2],  cklpga_sh[,3], type = "p")



#Morse data의 비계량형 MDS
Data7.2.3<-read.table("morse.txt", header=T, check.names=F)
C<-as.matrix(Data7.2.3)
부호=colnames(C)
n<-nrow(C)

# Standard Transformation : cij(similarity) to dij(dissimilarity)
J<-matrix(1,n,n)
cii=diag(diag(C))%*%J
cij=C
cjj=J%*%diag(diag(C))
D<-sqrt(cii-2*cij+cjj)
D

# Non Metric MDS
#win.graph()
library(MASS)
con<-isoMDS(D, k=2)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y))-0.2, max(abs(y)))

plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,부호, cex=0.8, pos=1)
abline(v=0,h=0)
# 원이 조금 찌그러졌지만 전체적으로는 비슷함



