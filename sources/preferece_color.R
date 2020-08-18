setwd("C:/Rtest")
mycar <- read.csv("mycar.csv", header = TRUE)

install.packages("Hmisc")
library(Hmisc)

install.packages("prettyR")
library(prettyR)

table(mycar$color)
table(mycar[2])

# 빈도수를 이용해 항목별 비율 출력
prop.table(table(mycar$color))
prop.table(table(mycar[2]))
#소수 첫째자리까지 출력:round
round(prop.table(table(mycar[2]))*100, 1)

#항목 별 빈도수 및 백분율 값을 저장
surveyFreq <- c(table(mycar$color))
surveyProp <- c(round(prop.table(table(mycar[2]))*100, 1))
#항목별 빈도수, 백분율 값으로 데이터 테이블 생성
surveytable <- data.frame(Freq=surveyFreq, Prop=surveyProp)
surveytable

#Hmisc의 내장함수, 변수와 데이터 현황을 기술
#기술 통계량 분석결과를 보여줌
describe(mycar)
describe(mycar$color)

#항목 별 빈도 및 백분율데 대한 빈도분석 테이블
freq(mycar)
freq(mycar$color)

#실제 선호빈도의 비율차이 분석(카이스퀘어 검증)
#유의확률(p-value), 유의수준(alpa=0.05)보다 낮으면 
# 귀무가설 기각, 대립가설 채택
chisq.test(surveyFreq)
