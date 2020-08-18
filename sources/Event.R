setwd("C:/Rtest")
mytooth <- read.csv("mytooth.csv", header = TRUE)
mytooth

#install.packages("Hmisc")
#install.packages("prettyR")

library(Hmisc)
library(prettyR)

table(mytooth$buy)
table(mytooth[2])

prop.table(table(mytooth[2]))
round(prop.table(table(mytooth[2]))*100,2)

buyFreq <- c(table(mytooth[2]))
buyProp <- c(round(prop.table(table(mytooth[2]))*100,1))
buytable <- data.frame(Freq=buyFreq, Prop=buyProp)
buytable
View(buytable)

describe(mytooth)
describe(mytooth$buy)

freq(mytooth)
freq(mytooth$buy)

#이항 분포 검증
# 특정 변수의 선택 항목이 2개 중 하나일 때, 선택 비율이 동일한지 검정
binom.test(c(40,40), p=0.10)
binom.test(c(40,40), p=0.10, alternative = "two.sided", 
           conf.level = 0.95)
s