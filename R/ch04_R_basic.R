
## 4장 1절

a1<-3
A1<-4
a1+A1


x<-'Apple Pie'
x


for(i in 1:3){
    print(paste("i=", i))
}


for(i in 1:4){
    if(i %% 2 == 0){
        print('짝수')
    }
    else{
        print('홀수')
    }
}

w1<-1:4
ifelse(w1 %% 2 == 0, "짝수", "홀수")

sum_a_b<-function(a=1,b=1){
    c = a+b
    return(c)
}

sum_a_b()

sum_a_b(3,5)


# 데이터프레임 생성 
df = data.frame(ID = 1:5, name = c("Kim", "Choi", "Park", NA, "Lee"), 
                class = c(1, 1, 1, 2, 2), score =c(100, NA, 92, 70, 80))
df

class(df)
dim(df)
nrow(df)
ncol(df)

names(df)

str(df)

# 1~3번째 관측치 선택 
df[1:3, ]

# 1, 4번째 관측치 선택
df[c(1,4),]


# score가 80보다 크다는 조건을 넣었는데 결측치인 것도 같이 포함되는 것을 확인 
df[df$score > 80,]

# score가 80보다 큰 관측치 선택 (결측치 제거)
df[!is.na(df$score) & df$score > 80, ]

# 결측값에 대한 처리를 따로 안해도 됨   
subset(df, score > 80)

# 1,3번째 변수 선택 
df[, c(1,3)]

# 3번째 변수 선택 
df[, 3]


#  score 변수 선택 
df[, "score"]

# subset 함수 활용 : score 변수 선택 
subset(df, select = score)

#  ID, score 변수 선택 
df[, c("ID", "score")]

# subset 함수 활용 : ID, score 변수 선택  
subset(df, select = c(ID, score))

#  1~2번째 관측치와 score 변수 선택 
df[1:2, "score"]

# score 변수 중 80 보다 큰 값만을 선택 
subset(df, score>80, select = score)


apply(is.na(df), 2, sum)

df[!is.na(df$score), ]

df[!is.na(df$score) & !is.na(df$name), ]

na.omit(df)

df[is.na(df$score), "score"]<-mean(df$score, na.rm = TRUE)
df

df$diff <- df$score - mean(df$score)
df$new_score<-c(98, 90, 80, 85, 70)
df

colnames(df)[5] <- 'mean_diff'
colnames(df)

tapply(df$score, df$class, mean)

aggregate(score ~ class, data=df, mean)

# dplyr 활용 

df = data.frame(ID = 1:5, name = c("Kim", "Choi", "Park", NA, "Lee"), 
                class = c(1, 1, 1, 2, 2), score =c(100, NA, 92, 70, 80))

library(dplyr)
df %>% slice(1:3)

df %>% slice(1,4)

df %>% filter(score > 80)

df %>% select(ID, score)

df %>% filter(!is.na(score))

df %>% filter(!is.na(score) & !is.na(name))

df %>% na.omit()

df<-df %>% mutate(score = ifelse(is.na(score), mean(score, na.rm=TRUE), score)) 
df

df %>% mutate(diff = score - mean(score), new_score = c(98, 90, 80, 85, 70))

df2<-df %>% mutate(diff = score - mean(score, na.rm=TRUE)) %>% 
    rename(mean_diff = diff)
df2

df2 %>% select(-mean_diff) 

df2 %>% select(-c(ID, mean_diff)) 

df %>% summarize(score_mean = mean(score, na.rm=TRUE))

df %>% group_by(class) %>% 
    summarize(score_mean = mean(score, na.rm=TRUE))


## 4장 3절


name<-data.frame(class = c(1, 2, 3, 4, 5),
                 teacher = c("moon", "lee", "park", "choi", "jung"))
name

exam_new<-df %>% left_join(name, by = "class")
exam_new


## 알아두면 좋아요 : R의 자료형 (벡터, 행렬, 배열, 리스트)

## 벡터 


v1 <- c(140, 2750.23, NA, 296.41, NA)  
v1

# 데이터 구조 및 형태 
class(v1)
is.vector(v1)

# 데이터의 길이 
length(v1)

# 1번째 값
v1[1]

# 2~4번째 값 
v1[2:4]

# 1번째 값 제외 
v1[-1]

# 1~2번째 값 제외 
v1[-(1:2)]

# 200보다 큰 값을 가지고 있는 값들만 선택 
v1[v1>200]

is.na(v1)

v1[is.na(v1)]

# !는 R에서 not의 의미 
v1[!is.na(v1)]

v2 <- c("Apple", "Google", "Samsung", "Microsoft", "LG")  
v2
class(v2)
is.vector(v2)

## 행렬

m <- matrix(1:12, nrow=3)  
m

# 데이터 구조 및 형태 
class(m)
is.matrix(m)

# 행과 열의 수
dim(m)

nrow(m)

ncol(m)

# 1번째 행과 2번째 열 선택 
m[1,2]

# 1~2번째 행과 3~4번째 열 선택 
m[1:2, 3:4]

# 1~2번째 행과 모든 열 선택 
m[1:2, ]

## 배열

a <- array(1:24, dim=c(3,4,2))  
a
class(a)

dim(a)

a[3,1,2]

## 리스트

ex = list(e1 = v1, e2 = v2, e3 = m, e4 = a, e5 = 3) 
ex

# e1으로 입력된 v1을 선택하여 출력  
ex$e1

# e5로 입력된 3을 선택하여 출력
ex$e5

# 3번째에 해당하는 m을 출력 
ex[[3]]

# 1,3번째 해당하는 v1과 m 출력 
ex[c(1,3)]
