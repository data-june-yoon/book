
## 12장 2절 

load(file='D:/Stat_Book/Data/hsb2.rdata') # 실행시 경로 수정
head(hsb2)
mean(hsb2$math)

# 단일 표본 T-검정 
t.test(hsb2$math, mu = 55)

# 쌍체 표본 T-검정  
t.test(hsb2$math, hsb2$write, paired = TRUE)

# 독립인 두 집단 T-검정
var.test(math ~ female, data = hsb2) 

t.test(math ~ female, data=hsb2, var.equal = TRUE)

t.test(math ~ gender, data=hsb2, var.equal = TRUE)

## 12장 3절 

# 적합성 검정 
coffee<-read.csv('D:/Stat_Book/Data/coffee.csv') # 실행시 경로 수정
tb<-table(coffee$brand)
tb
tb/sum(tb)

chisq.test(tb, p=c(0.3, 0.5, 0.2))


# 독립성 검정 
vec<-c(43, 30, 104, 230, 72, 75, 231, 198, 130, 20, 118, 87, 33, 40, 25, 11, 27, 64, 37, 18)
income_table<-matrix(vec, nrow = 5)
rownames(income_table)<-c("low-30", "30-50", "50-100", "100-300", "300-high")
colnames(income_table)<-LETTERS[1:4]
income_table

chisq.test(income_table)
