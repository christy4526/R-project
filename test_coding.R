setwd("C:/R/R-datas")
#------------------ 1번 문제 ------------------
# 요일에 따라서 a/s 받은 손님의 수가 다르다고 할 수 있는지 검증하시오.
# Service.csv
# --> 교차-적합성 검증
services <- read.csv("service.csv", head=T)
head(services)

table(services)

chisq.test(table(services))
# 해석 : 0.05보다 크기 때문에 요일에 따라 서비스 받은 손님 수는 다르지 않음



#------------------ 2번 문제 ------------------
#  가족 규모에 따라서 자동차 크기가 다르다는 대립가설을 검증하시오.
# carS.csv
# --> 교차-독립성 검증
carS <- read.csv("carS.csv", head=T)
head(carS)

table(carS)

chisq.test(table(carS))
# 해석 : p-value가 0.05보다 작기 때문에 가족 규모에 따라서 자동차의 크기는 
# 다르다는 대립가설 채택
#가족 규모에 따른 차량 사이즈의 차이가 유의미하게 있다.


#------------------ 3번 문제 ------------------
# 성별에 따른 환경 점수
# 영화관 만족도조사.csv
#  --> 범주-연속 : 독립 t-test(양측검정-두개가 같냐 다르냐)
scores <- read.csv("영화관 만족도조사.csv", head=T)
head(scores)
# 원하는 데이터 추출 1. 환경점수, 2. 여성, 3. 남성
scores$Hscore <- scores$관람환경만족1+scores$관람환경만족2+scores$관람환경만족3
man_score <- subset(scores,성별 == 1,c(Hscore))
woman_score <- subset(scores,성별 == 2,c(Hscore))

head(man_score)
# 등분산성 분석
var.test(man_score$Hscore, woman_score$Hscore)
# 해석 : 0.05보다 크기 때문에 등분산성을 띈다 --> t-test
# t-test
t.test(man_score$Hscore, woman_score$Hscore, 
       alternative = "two.side", conf.level = 0.95)
# 해석 : p값이 0.05보다 작기 때문에 성별에 따라서 관람환경만족은 차이가 있다


#------------------ 4번 문제 ------------------
# 영화 개봉 전과 후의 영화 평점 변화
# rating.csv
# --> 대응표본 검사
rating <- read.csv("rating.csv", head=T)
head(rating)

#length(rating$berating)
range(rating$berating) #결측치 제거
range(rating$afrating) #결측치 제거

#등분산성 검사
var.test(rating$berating, rating$afrating, paired=TRUE)
# --> p값이 0.05보다 작기 때문에 wilcox를 쓴다.

#t-test
wilcox.test(rating$berating, rating$afrating, paired=TRUE,
            alternative = "two.side", conf.level = 0.95)
# 해석 : p값이 0.05보다 작기 때문에 영화 개봉 전과 후의 평점은 차이가 있다.

install.packages("psych")
#------------------ 5번 문제 ------------------
# 포도의 크기와 속성기간이 포도 와인의 가격에 어떻게 영향을 미치는가?
# grape.csv
#  --> 회귀분석
grapes <- read.csv("grape.csv", head=T)
head(grapes)
# vif
library(car)
# * lm, vif 순서 중요!
grapes.lm <- lm(formula=price~size+period, data = grapes)
vif(grapes.lm) # 다중공산성이 4보다 작으면 괜찮은거고 10이 넘어가면 제거
#독립변수들이 서로 영향을 주고받지 않아서 제외할 필요가 없다
# 다중공산성이 심각하지 않다
summary(grapes.lm)
# 해석 : size, period의 Pr이 0.05보다 작기 때문에 와인 가격에 영향을 미치는 독립변수다.
# 모델 전체의 p값이 0.05보다 작기 떄문에 모델은 유효하다.

#제일 밑에 보이는 p값이 0.05보다 낮으므로 이 모델은 유효하며, 
#사이즈와 기간 모두 각각의 p값이 0.05보다 작으므로 두 변수 둘다 가격에 영향을
#많이 미치는 것을 알 수 있다.  Adjusted R-squred 값을 통해 이 모델은 88% 설명력을 
# 지니고 있다는 것을 알 수 있습니다.

#------------------ ANOVA ------------------
# 교육 방법 별 영업사원의 영업 실적
jobedues <- read.csv("jobedu.csv", head=T)
head(jobedues)

# 정규성 분석
tapply(jobedues$performance, jobedues$method, shapiro.test)
shapiro.test(jobedues$method)
# --> p값이 0.05보다 작기 때문에 정규성을 만족하지 않는다.

# 등분산성 분석
