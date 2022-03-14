
## 11장 2절 


library(ggplot2)

sample_means_plot<-function(n){
    sample_means<-NULL 
    for(i in 1:1000){
        x<-rbinom(n, 5, 0.7)
        xbar<-mean(x)
        sample_means<-c(sample_means, xbar)
    } # end i  
    df<-data.frame(x= sample_means)
    g<-ggplot(df, aes(x)) + geom_histogram() + ggtitle(paste("sample size=", n))
    return(g)
} # end function


set.seed(123)

d1<-sample_means_plot(3)
d2<-sample_means_plot(10)
d3<-sample_means_plot(100)
d4<-sample_means_plot(5000)

gridExtra::grid.arrange(d1, d2, d3, d4, ncol=2)


## 11장 3절 

load(file='D:/Stat_Book/Data/hsb2.rdata')
x<- hsb2$math 
m<-mean(x)
m


n<-length(x)
se<- sd(x) / sqrt(n)
t<-qt(0.025, df = n-1, lower.tail=FALSE) 
lower = m - se *t
upper = m + se * t
print(paste('l=', round(lower,5), ',u=', round(upper,5)))

t.test(x)$conf.int

t.test(x, conf.level = 0.99)$conf.int
