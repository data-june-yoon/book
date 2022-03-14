
## 15장 2절  

data<-read.table("D:/Stat_Book/Data/reg4.txt", header=T, sep="\t") # 실행시 경로 수정
reg1<-lm(y ~ x, data=data)
summary(reg1)

library(ggplot2)
ggplot(data = data, aes(x = x, y = y)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "blue")

library(MASS)
idx<-which(abs(stdres(reg1))>2.5)
idx

cooks.distance(reg1)

plot(reg1, which=4)

data2<-data[-idx, ]
reg2<-lm(y ~ x, data=data2)
summary(reg2)

ggplot(data = data2, aes(x = x, y = y)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "blue")


## 15장 3절

load(file='D:/Stat_Book/Data/hsb2.rdata') # 실행시 경로 수정
hsb2_score<-subset(hsb2, select=c(read, write, math, science, socst))
GGally::ggpairs(hsb2_score)

r1<-lm(science ~ read + write + math+ socst + as.factor(race) + as.factor(female), hsb2)
summary(r1)

r2<-step(r1, direction = "both")
formula(r2)
summary(r2)

r3<-lm(science ~ read + write + math+ race_f + gender, hsb2)
summary(r3)

library(car)
vif(r3)

plot(r3, which=2, pch = 16, cex = 0.5)
