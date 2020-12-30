setwd("E:/학교/2020 2학기 정호재/통계조사론")

survey<-read.csv("survey_sampling.csv", header=T, fileEncoding = "UCS-2LE")
head(survey)
dim(survey)
str(survey)

summary(survey[,21]) #월 지출에서의 교통비 관계

grad_sch=table(survey[,17],survey[,18])
chisq.test(grad_sch)
# p-value = 1 대학별 학년은 동립

home_sch=table(survey[,1],survey[,18])
chisq.test(home_sch)
# p-value = 0.4 대학별 거주형태는 동립

home_grad=table(survey[,1],survey[,17])
chisq.test(home_grad)
# p-value = 6e-05 학년별 거주형태는 차이남

home_type=table(survey[,1],survey[,2])
chisq.test(home_type)
# p-value = 5e-11 거주형태별 통학형태는 차이남

gender_type=table(survey[,16],survey[,2])
chisq.test(gender_type)
# p-value = 0.8 성별에 따른 통학형태는 동일

gender_home=table(survey[,16],survey[,1])
chisq.test(gender_home)
# p-value = 0.8 성별에 따른 거주형태는 동일

home_time=table(survey[,1],survey[,3])
chisq.test(home_time)
# p-value = 4e-10 거주형태에 따른 통학시간은 차이남

home_money=table(survey[,1],survey[,4])
chisq.test(home_money)
# p-value = 2e-06 거주형태에 따른 교통비는 차이남(그래프)

home_mmoney=table(survey[,1],survey[,20])
chisq.test(home_mmoney)
# p-value = 5e-04 거주형태에 따른 월 지출은 차이남(그래프)

time_mmoney=table(survey[,4],survey[,20])
chisq.test(time_mmoney)
# p-value = 0.7 교통비에 따른 월 지출이 동일

home_town=table(survey[,1],survey[,19])
chisq.test(home_town)
# p-value = 3e-08 본가 행정구역에 따른 거주형태 차이남

time_town=table(survey[,4],survey[,19])
chisq.test(time_town)
# p-value = 0.07 본가 행정구역에 따른 통학시간 동일(p-value가 큰건아님)

grad_town=table(survey[,17],survey[,19])
chisq.test(grad_town)
# p-value = 0.2 학년에 따른 본가 행정구역 동일

sch_town=table(survey[,18],survey[,19])
chisq.test(sch_town)
# p-value = 0.007 학교에 따른 본가 행정구역 차이남
barplot(sch_town, legend=T)


table(survey[,2],survey[,1])
table(survey[,5],survey[,1])

# 1,2차 교통계획 vs 3차 교통계획 만족도 비교
boxplot(survey[,8],survey[,9],ann=FALSE)

shapiro.test(survey[,8])
shapiro.test(survey[,9])
#정규성 만족 x

# H0 : 교통계획의 만족도에 변화가 없다.
wilcox.test(survey[,9],survey[,8])

# p-value = 4.01e-05 H0 기각 만족도의 변화가 있다.

mean(survey[,8])
mean(survey[,9])
# 3차 대중교통계획의 만족도가 더 높다.
#############################################
home_q1=table(survey[,1],survey[,10])
chisq.test(home_q1)

home_q2=table(survey[,1],survey[,11])
chisq.test(home_q2)

home_q3=table(survey[,1],survey[,12])
chisq.test(home_q3)

home_q4=table(survey[,1],survey[,13])
chisq.test(home_q4)

home_q5=table(survey[,1],survey[,14])
chisq.test(home_q5)

home_q6=table(survey[,1],survey[,15])
chisq.test(home_q6)

# 거주형태와 각 질문에 대한 응답은 차이남

q1_q2=table(survey$X10,survey$X11)
chisq.test(q1_q2)
q1_q3=table(survey$X10,survey$X12)
chisq.test(q1_q3)
q1_q4=table(survey$X10,survey$X13)
chisq.test(q1_q4)
q1_q5=table(survey$X10,survey$X14)
chisq.test(q1_q5)
q1_q6=table(survey$X10,survey$X15)
chisq.test(q1_q6)

q2_q3=table(survey$X11,survey$X12)
chisq.test(q2_q3)
q2_q4=table(survey$X11,survey$X13)
chisq.test(q2_q4)
q2_q5=table(survey$X11,survey$X14)
chisq.test(q2_q5)
q2_q6=table(survey$X11,survey$X15)
chisq.test(q2_q6)

q2_q3=table(survey$X11,survey$X12)
chisq.test(q1_q3)
q2_q4=table(survey$X11,survey$X13)
chisq.test(q1_q4)
q2_q5=table(survey$X11,survey$X14)
chisq.test(q1_q5)
q2_q6=table(survey$X11,survey$X15)
chisq.test(q1_q6)

q3_q4=table(survey$X12,survey$X13)
chisq.test(q3_q4)
q3_q5=table(survey$X12,survey$X14)
chisq.test(q3_q5)
q3_q6=table(survey$X12,survey$X15)
chisq.test(q3_q6)

q4_q5=table(survey$X13,survey$X14)
chisq.test(q4_q5)
q4_q6=table(survey$X13,survey$X15)
chisq.test(q4_q6)
q5_q6=table(survey$X14,survey$X15)
chisq.test(q5_q6)

# 각 질문 간 응답은 차이남
barplot(table(survey$X10))
barplot(table(survey$X11))
barplot(table(survey$X12))
barplot(table(survey$X13))
barplot(table(survey$X14))
barplot(table(survey$X15))


barplot(home_q1, legend=T)
barplot(home_q2, legend=T)
barplot(home_q3, legend=T)
barplot(home_q4, legend=T)
barplot(home_q5, legend=T)
barplot(home_q6, legend=T)
###############################################
# Data manipulation
library(tidyverse)
library(plyr)
library(ggplot2)

# Analysis
library(arules)      
library(arulesViz)  

# Grid
library(patchwork)

ass <- data.frame(cbind(survey[,c(1,10:15)]))
for (i in 1:6){
  ass[,i] <- as.factor(ass[,i])
}
ass <- as.data.frame(ass)
sapply(ass, class)

asso <- as(ass, "transactions")
str(asso)


# association rule generation
#asso <- apriori(asso,
#                parameter = list(support = 0.3, confidence = 0.5))


# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- rep(0,9)
rules_sup5 <- rep(0,9)
rules_sup1 <- rep(0,9)
rules_sup0.5 <- rep(0,9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(asso, 
                                   control = list(verbose=F),
                                   parameter=list(sup=supportLevels[1], 
                                                  conf=confidenceLevels[i], 
                                                  target="rules"
                                   )))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  
  rules_sup5[i] <- length(apriori(asso, 
                                  control = list(verbose=F),
                                  parameter=list(sup=supportLevels[2], 
                                                 conf=confidenceLevels[i], 
                                                 target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)){
  
  rules_sup1[i] <- length(apriori(asso, 
                                  control = list(verbose=F),
                                  parameter=list(sup=supportLevels[3], 
                                                 conf=confidenceLevels[i], 
                                                 target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)){
  
  rules_sup0.5[i] <- length(apriori(asso, 
                                    control = list(verbose=F),
                                    parameter=list(sup=supportLevels[4], 
                                                   conf=confidenceLevels[i], 
                                                   target="rules")))
  
}


# Number of rules found with a support level of 10%
p1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
            xlab="Confidence level", ylab="Number of rules found", 
            main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
p2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
            xlab="Confidence level", ylab="Number of rules found", 
            main="Apriori with a support level of 5%") +   
  
  scale_y_continuous(breaks=seq(0, 20, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
p3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
            xlab="Confidence level", ylab="Number of rules found",
            main="Apriori with a support level of 1%") +
  
  scale_y_continuous(breaks=seq(0, 500, 50)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
p4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
            xlab="Confidence level", ylab="Number of rules found",
            main="Apriori with a support level of 0.5%") +
  
  scale_y_continuous(breaks=seq(0, 2000, 200)) +
  theme_bw()



# Subplot
p1+p2+p3+p4 + plot_layout(ncol=2)


# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  
  
  scale_x_continuous(breaks=seq(0,1, 0.1)) +
  scale_y_continuous(breaks=seq(0, 30000, 500)) +
  theme_bw() +
  theme(legend.title=element_blank(),
        legend.position = "bottom")




options(digits=3) # 소수점 3째자리까지만 표현


# 최대한 많은 경우의 수를 확인(대부분 관계가 비슷)
rule <- apriori(asso, 
                control = list(verbos=F),
                parameter = list(support=0.01,
                                 conf =0.5,
                                 target = "rules"))



summary(rule)

inspect(sort(rule, by = "lift", decreasing = T)[1:20])

inspect(sort(rule, by = "lift", decreasing = F)[1:200])

rule_interest <- subset(rule, rhs %ain% "X1=본가 거주")
inspect(sort(rule_interest, by = "support", decreasing = T)[1:70])

rule_interest <- subset(rule, rhs %ain% "X1=해당 학교소재 자치구에서 자취(금정구, 남구)")
inspect(sort(rule_interest, by = "lift", decreasing = T)[1:100])

rule_interest <- subset(rule, rhs %ain% "X1=해당 학교소재 자치구에서 자취(금정구, 남구)")
inspect(sort(rule_interest, by = "lift", decreasing = T)[1:100])


###########################################################################################