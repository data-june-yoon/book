## 5장 1절

bank<-read.csv('D:/Stat_Book/Data/bank.csv') # 실행시 경로 수정 
head(bank, n=5)

?read.csv

hsb2<-read.table('D:/Stat_Book/Data/hsb2.txt', header=TRUE, sep='\t') # 실행시 경로 수정 
head(hsb2)

library(readxl)
apt<-read_excel('D:/Stat_Book/Data/아파트_실거래가_서울.xlsx') # 실행시 경로 수정 
tail(apt)

library(gapminder)
head(gapminder)

data<-gapminder::gapminder
head(data)

library(quantmod)
getSymbols('AAPL', from = '2000-01-01', to='2021-09-30')
head(AAPL)

getSymbols(c('GOOG', 'TSLA'), from = '2000-01-01', to='2021-09-30')
tail(GOOG)
tail(TSLA)

s<-getSymbols('005930.KS', from = '2000-01-01', to='2021-09-30', auto.assign=FALSE)
head(Cl(s))
