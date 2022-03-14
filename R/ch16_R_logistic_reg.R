
## 16장 2절

admission<-read.csv("D:/Stat_Book/Data/grad.csv") # 실행시 경로 수정
names(admission)
admission$admit<-factor(admission$admit)
admission$rank<-factor(admission$rank)
summary(admission)

model<-glm(admit ~ gre + gpa + rank, data=admission, family="binomial")
summary(model)

exp(coef(model))

new<-with(admission, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
new$rankP<-predict(model, newdata=new, type='response')
new

new2<-with(admission, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                                 gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 
new2$PredictedProb<-predict(model, newdata=new2, type='response')

library(ggplot2)
ggplot(new2, aes(x=gre, y = PredictedProb)) + 
    geom_line(aes(colour=rank))

admission$pred<-predict(model, admission, type="response")
admission$class<-factor(round(admission$pred))

library(ROCR)
predictions<-prediction(admission$pred, admission$admit)
perf <- performance(predictions, "tpr", "fpr")
plot(perf,col="black",lty=1, lwd=2, main='ROC curve', cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
abline(0,1, col="red")
auc <- unlist(slot(performance(predictions,"auc"), "y.values"))
legend(0.4,0.4,legend=c(paste0("AUC: ",round(auc,2))),cex=0.6, bty = "n",box.col = "white")
