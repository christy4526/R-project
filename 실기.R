setwd("C:/Rtest/datas")

## 1번 문제 요일별 a/s방문 손님 수가 다른가
#  --> 교차검증 독립 : 요일(범주), 종속 : 손님 연속

service <- read.csv("service.csv", header = TRUE)
head(service)

service_freq <- table(service)
chisq.test(service_freq)

library(psych)
describe(service)
#p값이 0.05이상이므로 요일별로 a/s는 빈도는 다르지 않다
#해석 : p값이 0.05보다 크므로 대립가설(요일에 따른 as수가 차이가 있다)를 기각하고
#귀무가설(as수가 차이가 없다)를 채택한다

################################################################
## 2번 문제 가족구성원 수에 따른 자동차 사이즈 
# 독립성 검정
# T-test 전에 확인해야 할 것 3가지
# 1. 등분산성 검정 : p-value > 0.05
# 2. 정규성 검정   : p-value > 0.05 
car <- read.csv("carS.csv", header = TRUE)
View(car)
str(car)
range(car$family)
range(car$carsize)

cars<-table(car$family, car$carsize)

chisq.test(cars)
#해석 : 귀무가설은, 대립가설은, pvalue가 0.05보다 작기 때문에
#가족규모에 따른 자동차 크기의 차이가 있다(대립가설 채택)


## 3번 문제
# 성별에 따른 환경 점수
#데이터 : 영화관 만족도// 환경,성별 // 성별에 따른 환경점수는?
#엑셀부르는게 안되서 csv로 만들었음

movie <- read.csv("복사본 영화관 만족도조사.csv", header = TRUE)
head(movie)

#신뢰도 분석
library(dplyr)
satisfation <- dplyr::select(movie, 관람환경만족1, 
                             관람환경만족2, 관람환경만족2)
psych::alpha(satisfation)

#raw_alpha값을 확인한 결과 전체적인 신뢰도가 높기 때문에 
# 같이 묶어서 계산
# 관람환경이 3개라 mutate로 새로운 열 만들어서 세개를 
# 더한 값을 값으로 넣음
movie <- mutate(movie, satisfation = 관람환경만족1+
                  관람환경만족2+관람환경만족3)
movie_f <- subset(movie, 성별==2, select=satisfation)
movie_m <- subset(movie, 성별==1, select=satisfation)

#정규성검정 pass
#등분산검정
var.test(movie_f$satisfation, movie_m$satisfation)
# p값이 0.05보다 크므로 등분산성을 띈다.(#등분산 만족)

t.test(movie_f$satisfation, movie_m$satisfation)
#p값이 0.05보다 작으므로 남녀에 따른 관람환경만족은 다르다고 할 수 있다.
#해석 :p값 0.05보다 작으므로  대립가설(차이가 있다) 채택

movie <- read.csv("복사본 영화관 만족도조사.csv", header = TRUE)
head(movie)
movie <- mutate(movie, enviro_all = 관람환경만족1+관람환경만족2+관람환경만족3)
# 관람환경이 3개라 mutate로 새로운 열 만들어서 세개를 더한 값을 값으로 넣음
man <- movie %>% filter(성별 == 1)
woman <- movie %>% filter(성별 == 2)

var.test(man$enviro_all, woman$enviro_all)
# p값이 0.05보다 크므로 등분산성을 띈다.
t.test(man$enviro_all, woman$enviro_all)
#p값이 0.05보다 작으므로 남녀에 따른 관람환경만족은 다르다고 할 수 있다.



# 영화 개봉 전과 후의 영화 평점 변화
# 대응 표본 검사
movies <- read.csv("rating.csv")

shapiro.test(movies$berating)
shapiro.test(movies$afrating)

t.test(movies$berating, movies$afrating, paired = TRUE)







##4번 대응표본
rate <- read.csv("rating.csv", header = TRUE)
head(rate)

#결측치 확인
range(rating)

length(rate$berating)
length(rate$afrating)

#샤피로 생략
#등분산성 검사
var.test(rating$berating, rating$afrating, paired=TRUE)
var.test(rate$berating, rate$afrating)

wilcox.test(rating$berating, rating$afrating, paired = T, 
            alternative = "two.sided", conf.int = T, 
            cof.level=0.95)
t.test(rating$berating, rating$afrating, var.equal = F, 
       paired = T)
#P값이 0.05보다 작으므로 귀무가설(영화 개봉 전과 후의 평점변화는 차이가 없다)를
#기각하고 대립가설(영화 개봉 전과 후의 평점변화는 차이가 있다)를 채택한다

wilcox.test(rate$berating, rate$afrating, paired=TRUE, 
            alternative = "two.sided", conf.level = 0.95)
#비포 애프터가 다르다.
wilcox.test(rate$berating, rate$afrating, paired=TRUE, 
            alternative = "less", conf.level = 0.95)
# 애프터의 점수가 더 좋아졌다고 볼 수 있다.

## 5번 
grape <- read.csv("Grape.csv", header = T)
head(grade) 

library(car)
vif(grade_result)

grape_lm <- lm(price~size+period, data=grape)
summary(grape_lm)

#p값이 0.05보다 낮으므로 이 모델은 유효하며, 사이즈와 기간 모두 가격에 영향을
#많이 미치는 것을 알 수 있다. 이 모델은 88% 설명력을 지니고 있다.

## 1번 as요일별 차이가 있는가?
as <- read.csv("service.csv", header = TRUE)
head(as)
table(as)
chisq.test(table(as))

## 2번 가족구성원 수와 차량사이즈에 차이가 있는가
car <- read.csv("carS.csv", header = TRUE)
head(car)
range(car$family)
range(car$carsize)
table(car)
chisq.test(table(car))
