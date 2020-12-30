# 𝑎 = 3, 𝑐 = 1, 𝑥 = 3, 𝑚 = 8일 때, 난수의 생성결과
m <- 8
a <- 3
c <- 1
x <- 3
for(i in 2:10) x[i] <- (a*x[i-1]+c)%%m
x


i <- 1
y <- 3
while(1){
  y[i+1] <- (a*y[i]+c)%%m
  if(length(y)>=1000) break;
  i <- i+1 }
y



library(MASS)
data(survey)
str(survey)
x<-table(survey$W.Hnd)
x
x/236
chisq.test(x,p=c(0.3, 0.7))
chisq.test(x,p=c(0.05, 0.95))


##########################################################
## K-S test as a goodness-of-fit test
x <- rnorm(100,0.5,1) 
u <- runif(50,0,1)
summary(x)
summary(u)

x11()
par(mfrow=c(1,2), mar=c(2,2,2,2), cex=1.2)
hist(x,freq=FALSE)
hist(u,freq=FALSE)

# k-s test
ks.test(x,u)                
ks.test(x,"pnorm",0.5,1)   
ks.test(u,"punif")         

##########################################################
# Run above and below the mean
x.y <- 1:20
plot(y[x.y])
plot(x.y, y[x.y], 'l')


install.packages('tseries')
library(tseries)

x <- factor(sign(rnorm(100)))
runs.test(x)

diff_y <- 100*diff(log(y))
runs.test(factor(diff_y > 0))

##########################################################
# hit or miss
n=100000000
x=runif(n, -1, 1)
y=runif(n, -1, 1)
lambda=mean(x^2+y^2 < 1)
cat("pi=",lambda*4, "\n");

############# EX1 ###################
g=function(x) 4*sqrt(1-x^2)
n=10000
const=4 # c:최댓값
x=runif(n, 0,1)
y=runif(n, 0, const)
p.hat=mean(y<=g(x))
I.hat = 4*p.hat #c*(b-a)*hat(p)
####################plot
col=1+as.integer(y<=g(x))
curve(g, xlim=c(0,1), col="red")
points(x,y, col=col)

############# EX2 ######################
g=function(x) exp(x)
n=10000
const=exp(1) # c
x=runif(n, 0,1)
y=runif(n, 0, const)
p.hat=mean(y<=g(x))
I.hat = exp(1)*p.hat

####################
col=1+as.integer(y<=g(x))
curve(g, xlim=c(0,1), col="red")
points(x,y, col=col)


#################sample mean####################
g=function(x) 4*sqrt(1-x^2)
n=10000
out1=numeric(100)
out2=numeric(100)

system.time(
  for(i in 1:100){
    x=runif(n,0,1)
    out1[i]=mean(g(x))
  }
)

system.time( #system.time 코드가 돌아가는동안 걸리는 시간을 구해주는 함수
  for(i in 1:100){
    const=4 # c
    x=runif(n, 0,1)
    y=runif(n, 0, const)
    out2[i]=4*mean(y<=g(x))
  }
)

apply(cbind(out1,out2), 2, summary)
ss=apply(cbind(out1,out2), 2, sd)
apply(cbind(out1,out2), 2, var)
abs(ss[1]-ss[2])/max(ss)

boxplot(cbind("Hit or Miss"=out2,"Sample mean"=out1))


###################importance sampling########################
g=function(x) sqrt(1-x^2)
n=1000000
rep=100
out2=numeric(rep)
####################
x=runif(n)
f<-function(x)6/5*(1-x^2/2)
mean(g(x)/f(x))
####################
for(i in 1:rep){
  x=rep(NA,n)
  C <- 6/5
  h <- 1
  k <- 1
  while(k<=n){
    U <- runif(1)
    Y <- runif(1)
    if(U <= f(Y)/(C*h)){
      x[k] <- Y;
      k <- k+1
    }
  }
  out2[i] <- mean(g(x)/f(x))
}
boxplot(out2)
abline(h=pi/4, col="red")
summary(out2)
pi/4

################## recursive #################
x1 <- 0
n <- 1
while(T){
  x0 <- x1
  x1 <- x0^2-2
  change <- abs((x1-x0)/x0)
  cat(n, x1, change, '\n') #출력하는 포멧 n x1 e 엔터
  if( change < 1e-6 ) break; #허용한계치보다 작을때까지 반복
  n <- n+1
}

################## Newton #################불연속함수는 못 씀
x1 <- 10
n <- 1
while(T){
  x0 <- x1
  fx <- x0^3-7*x0^2-7*x0-8
  dfx <- 3*x0^2-14*x0-7
  x1 <- x0-fx/dfx
  change <- abs((x1-x0)/x0) #x0로 나누면 수렴속도 빠름, but 덜 정확함(대부분 1보다 작음)
  cat(n, x1, change, '\n')
  if( change < 1e-6 ) break;
  n <- n+1
  if( n>100 ) break;
}

#############################################
# 뉴턴-랩슨 알고리즘
newton <- function(f, df, tol = 1e-7, x0 = 1, N = 300){ #f, df는 입력해야하고 나머지는 디폴트값, N=최대반복횟수
  i <- 1
  x1 <- x0
  p <- numeric(N)
  while(i <= N){
    x1 <- (x0 - (f(x0) / df(x0)))
    p[i] <- x1
    i = i+1
    if(abs((x1 - x0)/x0) < tol) break
    x0 = x1
  }
  return(p[1:(i - 1)])
}
f <- function(x) x^3+2
df <- function(x) 3*x^2
newton(f, df, x0 = 2)


#############################################
x<-c(.18740, .07779, 1.07893, .03122, .06175, 
     .46898, .44239, .04364, .94141, .21975)
f <- function(lambda, x, n) n/lambda-sum(x) #lambda에 대하여 편미분
df <- function(lambda, n) -n/(lambda^2) #로그우도함수에서 한 번 더 미분
newton <- function(f, df, tol = 1e-7, x0 = 1, x, N = 300){
  h <- 1e-7
  i <- 1
  x1 <- x0
  p <- numeric(N)
  while(i <= N){
    x1 <- (x0 - (f(x0, x, length(x)) / df(x0, length(x))))
    p[i] <- x1
    i = i+1
    if(abs((x1 - x0)/x0) < tol) break
    x0 = x1
  }
  return(p[1:(i - 1)])
}
newton(f, df, x0 = 1, x=x)

f(2.814317,x,length(x))#해를 찾았으면 0이 되아함, 하지만 0에 가깝지만 0을 찾지는 못함,항상 최적해를 찾지는 않고 근사치를 찾음
f(1/mean(x),x,length(x))#표본평균의 해 (역수를 넣었을 때 0이 나옴) but해를 찾기가 쉽지는 않음

'''
감마분포에서 알파와 베타에 대한 해를 찾기
표본값 rgamma
sed.seed(값)
x값 만들어서 
알파 베타에 대한 값을 추정하기
한시간 안에 해결할수있음

tip!!
  hat알파=bar x+베타(hat베타 대신 사용)
'''

#################################################
# 붓스트랩 알고리즘
dice <- c(1,2,3,2,6,6,5,1,1,1,4,2,4,1,4,5,6,6,3,2,5,6,4,1,2,3,2,2,5,3)
n <- length(dice)
B <- 200
sd(dice)
boot.sample <- matrix(0, B, n)
dice.sd <- numeric(B)

for(i in 1:B){
  boot.sample[i,] <- sample(dice, replace=T, n) #표본을 추출할때 사용하는 함수
  dice.sd[i] <- sd(boot.sample[i,]) #시그마 스타 헷
}

summary(dice.sd)
quantile(dice.sd, probs=c(.025, .975))


dice.mean <- numeric(B)

for(i in 1:B) dice.mean[i] <- mean(boot.sample[i,])

summary(dice.mean)
quantile(dice.mean, probs=c(.025, .975))


##############################################

# 붓스트랩 구간추정
x <- rnorm(30,10,3)
mean(x)-1.96*sqrt(3^2/30)
mean(x)-1.96*sqrt(var(x)/30)
t.CI <- t.test(x)
t.CI$conf.int

var.interval <- function(data) {
  df = length(data) - 1
  chi.lower = qchisq(0.025,df) #분위수를 나타냄
  chi.upper = qchisq(0.025,df,lower.tail=FALSE) #lower.tail=TRUE<-P(x^2<x)
  s2 = var(data)
  c(df * s2/chi.upper, df * s2/chi.lower)
}
var.interval(x)

########################################################

dice <-x
n <- length(dice)
B <- 200
sd(dice)
boot.sample <- matrix(0, B, n)
dice.sd <- numeric(B)

for(i in 1:B){
  boot.sample[i,] <- sample(dice, replace=T, n)
  dice.sd[i] <- var(boot.sample[i,])}

summary(dice.sd)
quantile(dice.sd, probs=c(.025, .975))
quantile(replicate(200, sd(sample(dice, replace=TRUE))), probs=c(0.025, 0.975))


#var.interval(x)와 quantile(dice.sd, probs=c(.025, .975)) 비교 -> 신뢰구간의 길이가 짧은쪽이 정밀도가 더 높다고 말할수가 있음


#########################################################
#중간고사 문제

#hit or miss
set.seed(123)
g=function(x) cos(x)
n=10000
const=1
x=runif(n,0,1)
y=runif(n,0,const)
mean(y<=g(x))

#표본평균법
x=runif(n)
mean(g(x))

#주표본기법
x=runif(n)
f<-function(x) 6/5*(1-x^2/2)
mean(g(x)/f(x))


set.seed(123)
x1<-0
n<-1
while(T){
  x0<-x1
  fx<- exp(x0)-4*cos(x0)
  dfx <- exp(x0)+4*sin(x0)
  x1<- x0-fx/dfx
  change <- abs((x1-x0)/x0)
  cat(n,x1,change, '\n')
  if(change < 1e-6) break;
  n<-n+1}


set.seed(123)
install.packages('Lock5Data')
library(Lock5Data)
data(CommuteAtlanta)
A<-CommuteAtlanta$Time
n<-length(A)
B<-200
mean(A)
boot.sample<-matrix(0,B,n)
Time.mean <- numeric(B)
for(i in 1:B){
  boot.sample[i,]<-sample(A, replace=T,n)
  Time.mean[i] <- mean(boot.sample[i,])}
summary(Time.mean)
quantile(Time.mean,probs=c(.025, .975))




set.seed(20191010)
x<-rgamma(n=100, shape=5, scale=2)
mean(x)
f<-function(alpha, x, n) n*log(alpha/mean(x))-n*digamma(alpha)+sum(log(x))
df<-function(alpha, n) ((n/alpha)-n*trigamma(alpha))
a0<-1
alpha<-a0