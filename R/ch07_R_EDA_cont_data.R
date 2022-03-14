
## 7장 2절  

library(reshape2)
head(tips)

summary(tips)

mean(tips$total_bill)
median(tips$total_bill)

total_bill_missing<-c(tips$total_bill, NA)
mean(total_bill_missing)
median(total_bill_missing)

mean(total_bill_missing, na.rm=TRUE)
median(total_bill_missing, na.rm=TRUE)

mean(tips$total_bill, trim = 0.1) 

max(tips$total_bill) - min(tips$total_bill) 

sd(tips$total_bill) 
sqrt(var(tips$total_bill)) # 분산에 제곱근을 취함 

IQR(tips$total_bill) 
quantile(tips$total_bill, 3/4) - quantile(tips$total_bill, 1/4) 
# [참고] 함께 출력되는 75%는 값이 아니고 이름에 해당 

cor(tips$total_bill, tips$tip) 

cor(tips$total_bill, tips$tip, method='spearman')
cor(tips$total_bill, tips$tip, method='kendall')


tips$day_ordered<-factor(tips$day, levels=c('Thur', 'Fri', 'Sat', 'Sun'))
tapply(tips$total_bill, tips$day_ordered, mean) 
# with(tips, tapply(total_bill, day_ordered, mean)) # 위와 동일  

aggregate(total_bill ~ day_ordered, data=tips, mean) 

tips$time_ordered<-factor(tips$time, levels=c('Lunch', 'Dinner'))
with(tips, tapply(total_bill, list(day_ordered, time_ordered), mean)) 

aggregate(total_bill ~ day_ordered + time_ordered, data=tips, mean) 

library(dplyr)
tips %>% group_by(day_ordered, time_ordered) %>% 
    summarize(mean_bill = mean(total_bill))

# R의 기본 함수 활용 
hist(tips$total_bill) 

hist(tips$total_bill, main='Total Bill', xlab = 'Total Bill') 

boxplot(tips$total_bill)

tips<-tips %>% mutate(ratio = tip / total_bill)
boxstats<-boxplot(tips$ratio)
# boxstats$out : 이상치 값들

tips %>% filter(ratio %in% boxstats$out) %>% select(total_bill, tip, ratio, sex, smoker, day_ordered, time_ordered)

boxplot(tips$total_bill, horizontal = TRUE)
m<-mean(tips$total_bill)
points(m, 1, pch = 5, col='red')

boxplot(total_bill ~ day_ordered, data=tips)

means<-tapply(tips$total_bill, tips$day_ordered, mean) # sex에 따른 total_bill의 평균 
boxplot(total_bill ~ day_ordered, data=tips)
points(1:4, means, pch = 5, col='red')

par(mfrow=c(1,2))
plot(tips$total_bill, tips$tip) 
plot(tips$total_bill, tips$tip, xlab='Total Bill', ylab='Tip', pch= 20, col='orange') 

# ggplot2 활용 

library(ggplot2)
d<-ggplot(data=tips, aes(x=total_bill)) 
d + geom_histogram()

d + geom_boxplot()

ggplot(data=tips, aes(x=1, y=total_bill)) + 
    geom_boxplot( ) + 
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="blue") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

ggplot(data=tips, aes(x=day_ordered, y=total_bill)) + 
    geom_boxplot( ) + 
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="blue") 

tips$ratio = tips$tip / tips$total_bill

ggplot(data=tips, aes(x=sex, y=ratio)) + 
    geom_boxplot( ) +
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="blue") 

ggplot(tips, aes(x=total_bill, y=tip)) + 
    geom_point() +
    geom_smooth(method='lm', se=F)

ggplot(tips, aes(x=total_bill, y=tip)) + 
    geom_point() +
    geom_smooth(method='lm', se=F) +
    facet_grid(sex ~ day_ordered)

ggplot(tips, aes(x=total_bill, y=tip, color=sex)) + 
    geom_point() +
    geom_smooth(method='lm', se=F) +
    facet_grid(. ~ day_ordered)

## 7장 3절 

load(file='D:/Stat_Book/Data/hsb2.rdata')
names(hsb2)

scores<-subset(hsb2, select = c(read, write, math, science, socst))
summary(scores)

sapply(scores, sd)

ggplot(data=hsb2, aes(x=ses_f, y=math)) + 
    geom_boxplot( ) + 
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="blue") 

ggplot(data=hsb2, aes(x=gender, y=science)) + 
    geom_boxplot( ) +
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="blue") 

ggplot(hsb2, aes(x=math, y=science, color=gender)) + 
    geom_point() +
    geom_smooth(method='lm', se=F)

ggplot(hsb2, aes(x=math, y=science)) + 
    geom_point() +
    geom_smooth(method='lm', se=F) +
    facet_grid(gender ~ ses_f)

ggplot(hsb2, aes(x=math, y=science, color=gender)) + 
    geom_point() +
    geom_smooth(method='lm', se=F) +
    facet_grid(. ~ ses_f)

cor(scores)

GGally::ggpairs(scores)
