
## 8장 1절 

kospi<-read.csv('D:/Stat_Book/Data/KOSPI.csv')
head(kospi)

str(kospi)

# kospi$Date[10] - kospi$Date[1] # 에러 발생

kospi$new_Date<-as.Date(kospi$Date)
str(kospi)

kospi$new_Date<-as.Date(kospi$Date)
str(kospi)

kospi$new_Date[10]
kospi$new_Date[1]
kospi$new_Date[10] - kospi$new_Date[1]

as.Date('01-08-2021', format='%d-%m-%Y')

Sys.setlocale('LC_ALL', 'English') 
format(as.Date('2021-08-21'), format="%B %d %Y")


## 8장 2절

library(ggplot2)
p <- ggplot(kospi, aes(x=new_Date, y=Close)) +
    geom_line() + xlab("")
p

t1<-p+scale_x_date(date_labels = "%Y %b %d")
t2<-p+scale_x_date(date_labels = "%m-%Y")
gridExtra::grid.arrange(t1, t2, ncol=1)

t3<-p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
t4<-p + scale_x_date(date_breaks = "6 months", date_labels = "%Y-%B") +
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
gridExtra::grid.arrange(t3, t4, ncol=1)
