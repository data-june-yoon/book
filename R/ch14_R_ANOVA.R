

## 14장 2절 

load(file='D:/Stat_Book/Data/hsb2.rdata') # 실행시 경로 수정
names(hsb2)

library(dplyr)
hsb2<-hsb2 %>% mutate(score = (read + write + math + science + socst)/5)
head(hsb2)

table(hsb2$ses_f)

aggregate(score ~ ses_f, hsb2, mean)

library(ggplot2)
ggplot(data=hsb2,aes(x=ses_f, y=score)) + geom_boxplot() + 
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="blue")

bartlett.test(score ~ ses_f, data=hsb2)

oneway.test(score ~ ses_f, data=hsb2, var.equal = TRUE)  

aov.out<-aov(score ~ ses_f, data=hsb2)
summary(aov.out)

TukeyHSD(aov.out)
