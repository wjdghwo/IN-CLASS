####feed-forward neural networks(신경회로망)
library(nnet)
nn.iris <- nnet(Species~., data=iris, size=2, rang=.1, decay=5e-4, maxit=1000) #c  은닉층의 갯수와 노드의 갯수, c(2(길이), 3(길이)), decay:결과값(소용안해되됨), maxit=최대반복횟수
summary(nn.iris)

#install.packages('devtools')
library(devtools)
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nn.iris)

#install.packages(c('clusterGeneration','scales','reshape'))
library(clusterGeneration)
library(scales)
library(reshape)
plot(nn.iris)

table(iris$Species, predict(nn.iris, iris, type='class'))


###backpropagation neural networks
#install.packages('neuralnet')
library(neuralnet)
data(infert)
net.infert <- neuralnet(case~age+parity+induced+spontaneous, hidden=2, data=infert, linear.output=F)
net.infert
net.infert$result.matrix
plot(net.infert)

net.case <- rep(0, dim(infert)[1])
net.case[ net.infert$net.result[[1]] >= .5 ] <- 1
pred <- table(infert$case, net.case)
sum(diag(pred))/sum(pred)#정분류률

out <- cbind(net.infert$covariate, net.infert$net.result[[1]])
dimnames(out) <- list(NULL, c("age","parity","induced","spontaneous", "nn-output"))
head(out)

results <- data.frame(actual = infert$case, prediction = net.infert$net.result[[1]])
results
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)


head(net.infert$generalized.weights[[1]])
par(mfrow=c(2,2))
gwplot(net.infert, selected.covariate='age', min=-2.5, max=5)
gwplot(net.infert, selected.covariate='parity', min=-2.5, max=5)
gwplot(net.infert, selected.covariate='induced', min=-2.5, max=5)
gwplot(net.infert, selected.covariate='spontaneous', min=-2.5, max=5)


############################################################
library(MASS)
Boston
str(Boston) #str(객체) : 데이터 구조, 변수 개수, 변수 명, 관찰치 개수, 관찰치의 미리보기
summary(Boston)
apply(Boston,2,function(x) sum(is.na(x)))#1은 행, 2는 열 / 0은 결측값이 없다 하지만 이상치 판단은 안했음

index <- sample(1:nrow(Boston),round(0.75*nrow(Boston)))
train <- Boston[index,]
test <- Boston[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
#################################
maxs <- apply(Boston, 2, max) 
mins <- apply(Boston, 2, min)
scaled <- as.data.frame(scale(Boston, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
net.Boston <- neuralnet(f,data=train_,hidden=c(5,3), linear.output=T)

plot(net.Boston)
net.Boston$result.matrix

pr.nn <- compute(net.Boston,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
test.r <- (test_$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
c(MSE.lm,MSE.nn)

par(mfrow=c(1,2))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=1)
points(test$medv,pr.lm,col='blue',pch=18,cex=1)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))



#############################################################
# 유전자 알고리즘
x <- seq(-1, 2, 1e-6)
f <- function(x) {
  y = x*sin(10*pi*x)+1
  return(y)
}
y <- f(x)
max<-max(y)
num_y<-which(y==max)#which.max(y)똑같음
x[num_y]

#R은 리눅스에 적합 윈도우에서는 램을 많이 써서 윈도우에서는 적합하지는 않음 윈도우에서 최소 32기가는 되야 돌아감

plot(x, y, type='l')
abline(h=1, col='red')
legend('topleft',legend='f(x)=x*sin(10*pi*x)+1',bty='n')


# function for computing f(x) 
funs <- function(x) {
  x = -1 + sum(x*2^seq(0,21))*3/(2^22-1)
  y = x*sin(10*pi*x)+1
  return(c(x,y))
}

set.seed(12345)
# root generation
M <- 50; G <- 40; PC <- 0.25; PM <- 0.01 #M=개체집합의 크기, G=최대 세대수, PC=교차확률, PM=변이확률, PR=재생확률은 1이라서 표기안함
x0 <- matrix(sample(0:1,22*M,replace=TRUE),ncol=22)     # to generate chromosomes of 0 generation ,replace=TRUE 복원추출
eval <- apply(x0,1,funs)                                # to compute values of evaluation function
fx <- t(eval)                                           # to transpose matrix
max.fx <- max(fx[,2])                                   # to find maximin value
xhat <- fx[which.max(fx[,2]),1]                         # to find argmax f(x) 

# initial values for variables
N <- 0 #각 세대
diff <- 10 
results <- c(N, xhat, max.fx)
# evolutionary process

for(N in 1:40){
  if(abs(diff)<=1e-6) break;
}

while (N < G || abs(diff) > 1e-6) {                     # looping rule 
  nxt <- rep(0,22)                                     # platform for new chromosome 
  for (i in 1:(M-1)) { 
    for (j in (i+1):M) {
      nx <- rbind(x0[i,],x0[j,])                       # to select two rows ,뒤에서 부터 입력
      ss <- (runif(3) < c(PC,PM,PM))                   # to generate uniform variates, i,j번째 확율이 필요해서 PM을 두 번 설정
      if (ss[1]) {                                     # cross-over
        nx[1,]<-c(x0[i,1:17],x0[j,18:22])             # i와 j를 교차해서 나타내라     
        nx[2,]<-c(x0[j,1:17],x0[i,18:22])
      } 
      if (ss[2]) {
        r <- sample(1:22,1)                           # to select position for mutation/변이 발생위치 찾기(임의로 정하는 것이기 때문에 임의로 설정함)
        nx[1,r] <- 1-nx[1,r]                          # mutation for group 1
      }
      if (ss[3]) {
        r <- sample(1:22,1)                           # to select position for mutation
        nx[2,r] <- 1-nx[2,r]                          # mutation for group 2
      } 
      if (sum(abs(nx[1,]-x0[i,]))>0) nxt <- rbind(nxt, nx[1,])
      if (sum(abs(nx[2,]-x0[j,]))>0) nxt <- rbind(nxt, nx[2,])
    }
  }
  nxx <- rbind(x0,nxt)                                 # new chromosomes
  fxx <- t(apply(nxx,1,funs))                          # to compute f(x) and to transpose
  diff <- max.fx-max(fxx[,2])                        # to compute change amounts
  if (max.fx < max(fxx[,2])) {                         # to change max and argmax f(x) /직전세대를 저장 
    max.fx <- max(fxx[,2]) 
    xhat <- fxx[which.max(fxx[,2]),1]
  }
  x0 <- nxx[order(fxx[,2],decreasing=T)[1:M],]  
  # to sort f(x) in decreasing order and to select 50-best values 
  N <- N +1    
  results <- rbind(results, c(N, xhat, max.fx))         # to add new results 
}    
results[seq(1,41,2),]
plot(x, y, type='l')
abline(h=1, col='red')
legend('topleft',legend='f(x)=x*sin(10*pi*x)+1',bty='n')
points(results[dim(results)[1],2], results[dim(results)[1],3], col = 'red', pch = 20)

#install.packages('GA')
library(GA)
GA <- ga(type = "real-valued", 
         fitness =  function(x) f(x),
         lower = -1, upper = 2, 
         popSize = 50, maxiter = 1000, run = 100) #개체의 수가 커지면 표본의 수가 커지고 빨리진다.
summary(GA)


f <- function(x)  (x^2+x)*cos(x)
lbound <- -10; ubound <- 10
curve(f, from = lbound, to = ubound, n = 1000)
GA <- ga(type = "real-valued", fitness = f, lower =lbound, upper = ubound)
summary(GA)
plot(GA)
curve(f, from = lbound, to = ubound, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)


Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20, color.palette = bl2gr.colors)
filled.contour(x1, x2, f, color.palette = bl2gr.colors)


GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), #x1,x2에 대한 범위
         popSize = 50, maxiter = 1000, run = 100)
summary(GA)
plot(GA)
filled.contour(x1, x2, f, color.palette = bl2gr.colors, 
               plot.axes = { axis(1); axis(2); 
                 points(GA@solution[,1], GA@solution[,2], 
                        pch = 3, cex = 2, col = "white", lwd = 2) }
)

##monitor
monitor <- function(obj) 
{ 
  contour(x1, x2, f, drawlabels = FALSE, col = grey(0.5))
  title(paste("iteration =", obj@iter), font.main = 1)
  points(obj@population, pch = 20, col = 2)
  Sys.sleep(0.2)
}
GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         popSize = 50, maxiter = 100, 
         monitor = monitor)

suggestedSol <- matrix(c(0.2,1.5,-1.5,0.5), nrow = 2, ncol = 2, byrow = TRUE)
GA1 <- ga(type = "real-valued", 
          fitness =  function(x) -Rastrigin(x[1], x[2]),
          lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
          suggestions = suggestedSol,
          popSize = 50, maxiter = 1)
head(GA1@population)


GA <- ga(type = "real-valued", 
         fitness =  function(x) -Rastrigin(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         suggestions = suggestedSol,
         popSize = 50, maxiter = 100)
summary(GA)
'''
해가 바뀌는 이유 임의의 데이터 즉 난수를 사용하여 값을 추정하기 때문에 
set.seed를 사용해야함 
but, 비슷한 값을 찾아내긴 한다.
'''
#####################################################
# 칼만필터링

t <- 1:150
x <- sqrt(t)  #임의 숫자를 부여
y <- rep(0,150)
a <- rep(0,150)
b <- rep(0,150)
w1 <- rep(0,150)
w2 <- rep(0,150)
v <- rep(0,150)
a[1] <- 50; b[1] <- 5
set.seed(5102215)

for(i in 2:150){
  w1[i] <- rnorm(1,0,5)
  w2[i] <- rnorm(1,0,2)
  a[i] <- a[i-1]+w1[i]
  b[i] <- b[i-1]+w2[i]
  v[i] <- rnorm(1,0,30)
  y[i] <- a[i]+b[i]*x[i]+v[i] #칼만필터 모형으로 수렬 생성
}

sim.dlm <- cbind(t, y, x, a, b, w1, w2, v)

par(mfrow=c(2,2))
plot(t,a,type='l')
plot(t,b,type='l')
plot(t,v,type='l')
plot(t,y,type='l')

reg.model <- lm(y~x)  #회귀분석 실시
summary(reg.model)

x11()
par(mfrow=c(1,2))
plot(y~x, type='o') #동적자료이므로 정적인 회귀분석 모형에 적합할 경우 기간이 늘어날수록 설명력 떨어짐
abline(coef(reg.model), lty=5)

plot(t, resid(reg.model)) #잔차가 비선형의 경향성을 가지고 있다. 따라서 단순선형회귀모형에 적합 아님 비선형회귀모형은 가능할수도 근데 이건 어려움 SO, 동적 선형모형인 칼만필터링을 사용해서 이야기를 해보자

R <- matrix(1,2,2); c1 <- matrix(1,2,2); c2<-matrix(1,2,2) 
yhat <- rep(0,150); b0 <- rep(0,150); b1 <- rep(1,150); #b0=hat(a) b1=hat(b)
cons <- rep(1,150); err <- rep(0,150)
n <- rep(1,150); s <- rep(1,150)
SSE <- rep(1,100); result <- matrix(0,100,2) #오차제곱합=SSE

for(k in 1:100){
  d=k/100 #discount factor/k값을 모르니까 100개의 칼만필터링 모형에서 sse가 가장 작은 칼만필터링 모형을 찾는다.(k의 값을 늘리면 더 정확함)(하나씩 다 값을 대입하는 노가다 느낌ㅋㅋㅋ)(델타값은 0과 1사이의 값이어야함)
  SSE[k]=0
  b0[1]=0
  b1[1]=1
  c1=diag(2)
  for(t in 2:150){
    R=c1/d
    yhat[t]=b0[t-1]+b1[t-1]*x[t]
    F <- cbind(cons[t], x[t])
    q <- (F%*%R) %*% t(F) + s[t-1] #행렬의 연산->값이 행렬이므로 상수(숫자)값으로 사용하기 위하여
    A <- R %*% t(F)/as.numeric(q) #칼만이득
    err[t] <- y[t]-yhat[t] #예측오차
    n[t] <- n[t-1] + 1
    s[t] <- s[t-1] + (err[t]**2/ q - 1)*s[t-1]/n[t]
    b0[t] <- b0[t-1] + A[1,]*err[t] #직전시점의 정보를 가지고 t시점에 대한 정보를 계산
    b1[t] <- b1[t-1] + A[2,]*err[t]
    c2 <- R * s[t] / as.numeric(q)
    if(t>=30) SSE[k] <- SSE[k] + err[t]**2 #초기치가 참 값에 거리가 멀수도 있으므로 일정 시점 이후의 값을 사용하도록 함/즉 t가 30이후 부터 150까지의 값을 사용하여 오차나 값을 확인하여 모형을 평가함
    c1 <- c2 #sse값이 최소가 되는 델타값을 찾는다.
  }
  result[k,] <- cbind(k, SSE[k])  
}
result
which.min(result[,2])#오차의 제곱합이 최소인 모형의 위치를 찾는 함수
delta<-1:100/100
delta[61]

x11()
plot(result[,1], result[,2], type='l')

####EX
d <- .61 #최소가 되는 델타값을 적용시켜 모형을 찾는다.
b0[1] <- 0
b1[1] <- 1
c1 <- diag(2)

for(t in 2:150){
  R <- c1/d
  yhat[t] <- b0[t-1] + b1[t-1]*x[t]
  F <- cbind(cons[t], x[t])
  q <- (F%*%R) %*% t(F) + s[t-1]
  A <- R %*% t(F)/as.numeric(q)
  err[t] <- y[t]-yhat[t]
  n[t] <- n[t-1] + 1
  s[t] <- s[t-1] + (err[t]**2/ q - 1)*s[t-1]/n[t]
  b0[t] <- b0[t-1] + A[1,]*err[t]
  b1[t] <- b1[t-1] + A[2,]*err[t]
  c2 <- R * s[t] / as.numeric(q)
  c1 <- c2
}

t <- 1:150
result.DLM <- cbind(t, y, yhat, x, b0, b1, err, n, s)
result.DLM #결과값 확인/ 초반 에러가 크므로 30이후 값부터 이야기했었음

x11()
par(mfrow=c(2,2))
plot(t, b0, ylim=c(15,30), type='o')
plot(t, b1, ylim=c(20,60), type='o')
plot(t, err, ylim=c(-100,200), type='o')
abline(h=0)
plot(y,yhat)
abline(0,1)

x11()
par(mfrow=c(1,1))
plot(t, y, type='l', lwd=2)
lines(t, yhat, lty=5, col='red', lwd=2)


################################################
#통계적 모의실험 - 버스 대기시간 모의실험
install.packages('msm')
library(msm) #지수분포의 대부분 3근방 낮은확률로 15근방, 정규분포 평균 15 표준편차 5 적분하면 1이 안나옴(절단함수이기 때문) 확률밀도함수처럼 모든 값의 합이 1이 되도록 만듬 
set.seed(5102215)
n<-1

bus.time <- 0 #버스의 도착시간 (모의실험 누적 합)
bus.n <- 0 #도착한 버스의 수
no.pass <- 0 #자리없을 때
cpass.in <- 0 #총 대기시간
cwait <- 0 #승객의 대기시간
cseat <- 0 #빈 좌석의 수

wait <- NA #승객의 대기시간
pass <- NA #
j <- 1
for(i in 1:2000){ #2000개의 난수가 나올 때까지 돌림
  pass.int <- rexp(1,1/3) #1/3평균이 3이되도록 설정
  if(pass.int < 15){ #버스의 배차간격 15분 미만으로 설정
    pass[j] <- pass.int
    j <- j+1
  }
}

pass

cpass <-cumsum(pass)#총 대기 시간을 정함

while(bus.time <= 3000){
  bus <- rtnorm(1, 15, 5, 0, 30)
  seat <- rpois(1, 5) #빈 좌석의 수
  
  bus.n <- bus.n + 1
  bus.time <- bus.time + bus
  if(seat==0) next #빈 좌석이 없을경우 다음버스로 
  for(i in 1:seat){ #적어도 좌석이 하나 이상 있을 때
    if(cpass[n]>bus.time) next #승객의 대기 누적시간이 버스 도착 시간보다 클 때 넘겨줌(버스없음)
    wait[n] <- bus.time - cpass[n]
    cwait <- cwait + wait[n]
    n <- n + 1
  }
  cseat <- cseat + seat
}
n-1 #승차한 승객의 총 수(종료가 된 상태는 n-1임)
bus.n #도착한 버스의 총 수
(mean.wait=cwait/n) #한 승객당 대기시간의 평균

hist( wait[wait!=0], main='Histogram of waiting time', ylim=c(0,200), xlab='waiting time', #혹시나 0일경우 제외 wait[wait!=0]
      cex.main=1.5, cex.lab=1.5)
n <- 1:length(wait[wait!=0])
plot(n,wait,ylab='waiting time')
#사람마다 얼마나 기다리는지 (첫번째부터 마지막 승객까지) 
# 400-600에 기다리는 시간이 높으므로 배차시간 조정필요