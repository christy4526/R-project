getwd()

setwd("C:/Rtest/wcdata")
library(KoNLP)
library(wordcloud)

useSejongDic()
mergeUserDic(data.frame("xxx", "ncn"))

## 분석할 원본 데이터를 변수로 읽어 들이기
data1 <- readLines("seoul_new.txt")
data1

## 데이터 중에서 명사만 골라내기
data2 <- sapply(data1, extractNoun, USE.NAMES = F)
data2

head(unlist(data2), 30)

data3 <- unlist(data2)
data3

# 원하지 않는 내용 걸러내기(1)
data3 <- gsub("\\d+", "", data3)
data3 <- gsub("서울시", "", data3)
data3 <- gsub("서울", "", data3)
data3 <- gsub("요청", "", data3)
data3 <- gsub("제안", "", data3)
data3 <- gsub(" ", "", data3)
data3 <- gsub("-", "", data3)

# 1-7 파일로 저장하고, 테이블 형태로 불러들이기
write(unlist(data3), "seoul_2.txt")
data4 <- read.table("seoul_2.txt")
data4

nrow(data4)
wordcount <- table(data4)
wordcount
head(sort(wordcount, decreasing=T), 20)

# 원하지 않는 내용 걸러내기(2)
ata3 <- gsub("○○", "", data3)
data3 <- gsub("개선", "", data3)
data3 <- gsub("문제", "", data3)
data3 <- gsub("관리", "", data3)
data3 <- gsub("민원", "", data3)
data3 <- gsub("이용", "", data3)
data3 <- gsub("관련", "", data3)
data3 <- gsub("시장", "", data3)
data3

write(unlist(data3), "seoul_3.txt")
data4 <- read.table("seoul_3.txt")
wordcount <- table(data4)
wordcount
head(sort(wordcount, decreasing=T), 20)
data4

library(RColorBrewer)
palete <- brewer.pal(9, "Set3")
wordcloud(names(wordcount), freq = wordcount, scale=c(5,1), rot, per=0.25
        , min.freq=1, random.color = F.