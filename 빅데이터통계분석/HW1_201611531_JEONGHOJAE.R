##########################################
library(MASS)
data(Boston)

y <- Boston[-55, 1]
x <- Boston[-55, -c(1,4,8,9)]
x <- as.matrix(scale(x))

Boston <- data.frame(x)
row.names(Boston)<-c(1:505)

RNGkind(sample.kind = "Rounding")
set.seed(123)
tran <- sample(nrow(x), 450)

variable <- names(Boston)


############문제 1######################
pe <- NULL
a <- NULL
b <- NULL
poly <- matrix(NA, 15, length(variable))

for (i in 1:length(variable)) {
  a <- Boston[,variable[i]]
  for (k in 1:15) {
    g <- lm(y ~ poly(a, k), subset = tran)
    pe[k] <- sqrt(mean((y - predict(g, Boston))[-tran]^2)) # train set을 pe계산에 포함시기지 않음
  }
  poly[,i] <- pe 
  colnames(poly) <- variable
}
poly

df <- NULL
model_poly_min <- NULL
for (i in 1:10) {
  df[i] <- which(poly[,i] == min(poly[,i]))
  model_poly_min[i] <- min(poly[,i])
  min_poly <- cbind(df, model_poly_min)
}
rownames(min_poly) <- variable



poly_df<-matrix(c(1,11,13,2,8,4,8,1,3,3),,1)

poly_df_pe <- NULL

for (i in 1:10) {
  poly_df_pe[i] <- poly[as.numeric(poly_df[i]),i]
}
poly_df_pe_ <- cbind(poly_df, poly_df_pe)
poly_df <- cbind(min_poly, poly_df_pe_)
poly_df



dg <- 1:15
par(mfrow=c(3,4))
for(i in 1:10) {
  plot(dg, poly[,i], type="b", col=2, xlab="Degree of Polynomial",
       ylab=variable[i], lwd=2, pch=19)
}


############문제 2######################

data <- data.frame(crim = y,x)
train_set <- data[tran,]
tran.x <- train_set[,-1]
tran.y <- train_set$crim
test_set <- data[-tran,]
test.x <- test_set[,-1]
test.y <- test_set$crim

df <- 1:15
variable <- names(Boston)
pe <- NULL
a <- NULL
b <- NULL
spline <- matrix(NA, 15, length(variable))

for (i in 1:length(variable)) {
  a <- tran.x[,variable[i]]
  b <- test.x[,variable[i]]
  for (k in 2:length(df)) {
    tran.fit <- smooth.spline(a, tran.y, df=df[k])
    pe[k] <- sqrt(mean((test.y - predict(tran.fit, b)$y)^2))
  }
  spline[,i] <- pe 
  colnames(spline) <- variable
}
spline

df <- NULL
model_spline_min <- NULL
for (i in 1:10) {
  df[i] <- which(spline[,i] == min(spline[,i],na.rm = TRUE))
  model_spline_min[i] <- min(spline[,i],na.rm = TRUE)
  min_spline <- cbind(df, model_spline_min)
}
rownames(min_spline) <- variable

min_spline


spline_df<-matrix(c(4,8,15,4,12,8,12,2,3,4),,1)

spline_df_pe <- NULL

for (i in 1:10) {
  spline_df_pe[i] <- spline[as.numeric(spline_df[i]),i]
}
spline_df_pe_ <- cbind(spline_df, spline_df_pe)
spline_df <- cbind(min_spline, spline_df_pe_)
spline_df



dg <- 1:15
par(mfrow=c(3,4))
for(i in 1:10) {
  plot(dg, spline[,i], type="b", col=2, xlab="Degree of smooth.spline",
       ylab=variable[i], lwd=2, pch=19)
}


###################문제 3#######################################
variable <- names(Boston)

pe <- NULL
a <- NULL
cut <- matrix(NA, 15, length(variable))

for (i in 1:length(variable)) {
  a <- Boston[,variable[i]]
  for (k in 2:15) {
    g <- lm(y ~ cut(a, k), subset = tran)
    pe[k] <- sqrt(mean((y - predict(g, Boston))[-tran]^2)) # train set을 pe계산에 포함시기지 않음
  }
  cut[,i] <- pe 
  colnames(cut) <- variable
}
cut

df <- NULL
model_cut_min <- NULL
for (i in 1:10) {
  df[i] <- which(cut[,i] == min(cut[,i], na.rm = TRUE))
  model_cut_min[i] <- min(cut[,i], na.rm = TRUE)
  model_cut_min1 <- cbind(df, model_cut_min)
}
rownames(model_cut_min1) <- variable


cut_df<-matrix(c(4,6,9,7,6,12,8,7,2,4),,1)

cut_df_pe <- NULL

for (i in 1:10) {
  cut_df_pe[i] <- cut[as.numeric(cut_df[i]),i]
}
cut_df_pe_ <- cbind(cut_df, cut_df_pe)
cut_df <- cbind(model_cut_min1, cut_df_pe_)
cut_df


dg <- 1:15
par(mfrow=c(3,4))
for(i in 1:10) {
  plot(dg, cut[,i], type="b", col=2, xlab="Degree of cut",
       ylab=variable[i], lwd=2, pch=19)
}
##################문제 4-1##################################################
# k-fold CV
dg <- 1:15
K <- 10 ## 10-fold cross validation
KCV1 <- matrix(NA, length(dg), length(variable))


set.seed(123)
u <- sample(rep(seq(10), length=length(y))) # 10개의 그룹을 각 데이터별로 설정

for (j in 1:10) {
  a <- Boston[,variable[j]]
  for (i in 1:length(dg)) {
    pe <- NULL
    for (k in 1:K) {
      tran <- which(u!=k) # u로 선정된 개체는 test 나머지는 train set으로 설정
      g <- lm(y ~ poly(a, i), subset=tran)
      pe[k] <- sqrt(mean((y - predict(g, Boston))[-tran]^2)) 
    }
    KCV1[i, j] <- mean(pe)
    colnames(KCV1) <- variable
  }
}

###################문제 4-2###########################################
pe <- NULL
a <- NULL
b <- NULL

dg <- 1:15
K <- 10 ## 10-fold cross validation
KCV2 <- matrix(NA, length(dg), length(variable))


set.seed(123)
u <- sample(rep(seq(10), length=length(y))) # 10개의 그룹을 각 데이터별로 설정

for (j in 1:10) {
  
  for (i in 2:length(dg)) {
    pe <- NULL
    for (k in 1:K) {
      tran <- which(u!=k) # u로 선정된 개체는 test 나머지는 train set으로 설정
      
      data(Boston)
      y <- Boston[-55, 1]
      x <- Boston[-55, -c(1,4,8,9)]
      x <- as.matrix(scale(x))
      
      Boston <- data.frame(x)
      row.names(Boston)<-c(1:505)
      variable <- names(Boston)
      data <- data.frame(crim = y,x)
      train_set <- data[tran,]
      tran.x <- train_set[,-1]
      tran.y <- train_set$crim
      test_set <- data[-tran,]
      test.x <- test_set[,-1]
      test.y <- test_set$crim
      
      a <- tran.x[,variable[j]]
      b <- test.x[,variable[j]]
      
      g <- smooth.spline(a, tran.y, df=dg[i])
      pe[k] <- sqrt(mean((test.y - predict(g, b)$y)^2))
    }
    KCV2[i, j] <- mean(pe)
    colnames(KCV2) <- variable
  }
}

######################문제 4-3#############################################
dg <- 1:15
K <- 10 ## 10-fold cross validation
KCV3 <- matrix(NA, length(dg), length(variable))


set.seed(123)
u <- sample(rep(seq(10), length=length(y))) # 10개의 그룹을 각 데이터별로 설정

for (j in 1:10) {
  a <- Boston[,variable[j]]
  for (i in 2:15) {
    pe <- NULL
    first <- NULL
    for (k in 1:10) {
      tran <- which(u!=k) # u로 선정된 개체는 test 나머지는 train set으로 설정
      g <- lm(y ~ cut(a, i), subset=tran)
      predict <- try(predict(g, Boston), silent = TRUE)
      pe[k] <- tryCatch(sqrt(mean((y[-tran] - predict[-tran])^2)), error=function(ex){pe[k]=NA})
    }
    KCV3[i, j] <- mean(pe, na.rm = TRUE)
    colnames(KCV3) <- variable
  }
}

###################문제 4 결과 정리##################################
df <- NULL
model_KCV1 <- NULL
for (i in 1:10) {
  df[i] <- which(KCV1[,i] == min(KCV1[,i], na.rm = TRUE))
  model_KCV1[i] <- min(KCV1[,i], na.rm = TRUE)
  model_KCV1_ <- cbind(df, model_KCV1)
}
rownames(model_KCV1_) <- variable

model_KCV1_


df <- NULL
model_KCV2 <- NULL
for (i in 1:10) {
  df[i] <- which(KCV2[,i] == min(KCV2[,i], na.rm = TRUE))
  model_KCV2[i] <- min(KCV2[,i], na.rm = TRUE)
  model_KCV2_ <- cbind(df, model_KCV2)
}
rownames(model_KCV2_) <- variable

model_KCV2_


df <- NULL
model_KCV3 <- NULL
for (i in 1:10) {
  df[i] <- which(KCV3[,i] == min(KCV3[,i], na.rm = TRUE))
  model_KCV3[i] <- min(KCV3[,i], na.rm = TRUE)
  model_KCV3_ <- cbind(df, model_KCV3)
}
rownames(model_KCV3_) <- variable

model_KCV3_


KCV <- cbind(model_KCV1_, model_KCV2_, model_KCV3_)
summary(KCV)
##################문제 4 그래프 및 문제 5 결과정리##########################
dg <- 1:15
par(mfrow=c(3,4))
for(i in 1:10) {
  plot(dg, KCV1[,i], type="b", col=2, xlab="Degree of Polynomial",
       ylab=variable[i], lwd=2, pch=19)
}

dg <- 1:15
par(mfrow=c(3,4))
for(i in 1:10) {
  plot(dg, KCV2[,i], type="b", col=2, xlab="Degree of smooth.spline",
       ylab=variable[i], lwd=2, pch=19)
}

dg <- 1:15
par(mfrow=c(3,4))
for(i in 1:10) {
  plot(dg, KCV3[,i], type="b", col=2, xlab="Degree of cut",
       ylab=variable[i], lwd=2, pch=19)
}

par(mfrow=c(3,4))
for (i in 1:10) {
  model<-cbind(KCV1[,i],KCV2[,i],KCV3[,i])
  matplot(dg, model, type="l", xlab="Degree of KCV", lty=1,
          ylab=variable[i], ylim=c(5,9), main="10-fold CV")
}

kfold1<-matrix(c(1,11,13,5,6,4,8,1,1,3),,1)
kfold2<-matrix(c(4,8,12,12,10,8,12,2,3,5),,1)
kfold3<-matrix(c(5,6,9,4,8,12,13,11,4,7),,1)
KCV_poly <- NULL

for (i in 1:10) {
  KCV_poly[i] <- KCV1[as.numeric(kfold1[i]),i]
}
KCV_poly_ <- cbind(kfold1, KCV_poly)


KCV_spline <- NULL
for (i in 1:10) {
  KCV_spline[i] <- KCV2[as.numeric(kfold2[i]),i]
}
KCV_spline_ <- cbind(kfold2, KCV_spline)

KCV_cut <- NULL
for (i in 1:10) {
  KCV_cut[i] <- KCV3[as.numeric(kfold3[i]),i]
}
KCV_cut_ <- cbind(kfold3, KCV_cut)

KCV_fit <- cbind(KCV_poly_, KCV_spline_, KCV_cut_)
rownames(KCV_fit) <- variable
KCV_fit
summary(KCV_fit)

val_fit <- cbind(poly_df_pe_,spline_df_pe_,cut_df_pe_)
rownames(val_fit) <- variable
val_fit
summary(val_fit)


