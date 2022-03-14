
## 6장 1절 

sapply(anscombe, mean)
sapply(anscombe, sd)

diag(cor(anscombe[,1:4], anscombe[, 5:8]))

op <- par(las=1, mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,0,0),
          lab=c(6,6,7), cex.axis=0.8, mgp=c(3,1,0))
ff <- y ~ x
for(i in 1:4) {
  ff[[2]] <- as.name(paste("y", i, sep=""))
  ff[[3]] <- as.name(paste("x", i, sep=""))
  lmi <- lm(ff, data= anscombe)
  xl <- substitute(expression(x[i]), list(i=i))
  yl <- substitute(expression(y[i]), list(i=i))
  plot(ff, data=anscombe, col="black", pch=16, cex=1.1,
       xlim=c(3,19), ylim=c(3,13), xlab=eval(xl), ylab=eval(yl))
  abline(lmi, col="blue")
}
par(op)


## 6장 3절 

if(!require(reshape2)) install.packages('reshape2')
library(reshape2)

str(tips)

table(tips$sex)

table(tips$day)

tips$day_ordered<-factor(tips$day, levels=c('Thur', 'Fri', 'Sat', 'Sun'))
table(tips$day_ordered)

tips$time_ordered<-factor(tips$time, levels=c('Lunch', 'Dinner'))
table(tips$day_ordered, tips$time_ordered)

# R의 기본 함수 활용 
day_tb<-table(tips$day_ordered)
barplot(day_tb)

par(mar=c(0,0,0,0))
pie(day_tb)

cross_tb<-table(tips$sex, tips$day_ordered)
barplot(cross_tb, legend = rownames(cross_tb), ylim=c(0, 120))

barplot(cross_tb, legend = rownames(cross_tb), beside=TRUE, ylim=c(0, 90))

# ggplot2 활용 
if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
ggplot(data=tips) + geom_bar(aes(x=day_ordered))

ggplot(data=tips) + 
  geom_bar(aes(x=day_ordered, fill=day_ordered)) 

ggplot(data=tips) + 
  geom_bar(aes(y=day_ordered, fill=day_ordered)) 

tb<-table(tips$day_ordered)
df<-data.frame(round(tb*100/sum(tb),2))
names(df)<-c('Days', 'Percent')
df

ggplot(df, aes(x="", y=Percent, fill=Days)) +
  geom_bar(stat="identity", color="white") +
  coord_polar("y") +
  theme_void() # remove background, grid, numeric labels

ggplot(data=df, aes(x="", y= Percent, fill=Days)) + 
  geom_bar(stat="identity", color="white") +
  coord_polar("y") +
  theme_void() + 
  geom_text(aes(label = paste0(Percent, '%')), 
            position = position_stack(vjust = 0.5)) 

b <- ggplot(data=tips, aes(x=day_ordered, fill=sex))
b + geom_bar(position='stack')

b + geom_bar(position='dodge')


## 6장 4절 

hsb2<-read.table('D:/Stat_Book/Data/hsb2.txt', header=TRUE, sep='\t') # 실행시 경로 수정 
names(hsb2)

hsb2$gender<-factor(hsb2$female, labels=c('male', 'female'))
hsb2$race_f<-factor(hsb2$race, labels=c('african american', 'asian', 'hispanic', 'white'))
hsb2$ses_f<-factor(hsb2$ses, labels=c('low', 'middle', 'high'))
hsb2$prog_f<-factor(hsb2$prog, labels=c('general', 'academic', 'vocational'))
hsb2$schtyp_f<-factor(hsb2$schtyp, labels=c('public', 'private'))
hsb2$prog_f<-factor(hsb2$prog, labels=c('general', 'academic', 'vocational'))

summary(hsb2)

ggplot(data=hsb2) + 
  geom_bar(aes(x=ses_f, fill=schtyp_f), position='dodge') 

save(hsb2, file='D:/Stat_Book/Data/hsb2.rdata') # 실행시 경로 수정 
