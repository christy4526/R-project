getwd()
setwd("C:/Rtest")

library(Hmisc)
library(prettyR)
library(psych)
install.packages("nortest")
library(nortest)
library(dplyr)

health <- read.csv("health.csv", header = TRUE)
head(health)

################ 1번 #################
range(health$허리둘레)
#남자 허리둘레 추출
manH <- health %>% filter(성별코드 == 1)
manH<- subset(health, 성별코드 ==1, c(허리둘레))

#사피로가 안될때
# shapiro.test(health$허리둘레)
nortest::ad.test(manH$허리둘레)

describe(manH$허리둘레)

t.test(manH$허리둘레,mu=85)


############### 2번 #################
manCol<- subset(health, 성별코드 ==1, c(성별코드,총콜레스테롤))
womanCol<- subset(health, 성별코드 == 2, c(성별코드,총콜레스테롤))

var.test(c(manCol$총콜레스테롤), c(womanCol$총콜레스테롤))

#분산이 동질적이면 ttest, 분산이 동질적이지 않으면 wilcox
wilcox.test(c(manCol$총콜레스테롤), c(womanCol$총콜레스테롤))


############### 3번 #################
chisq.test(health$성별코드,health$흡연상태)


############### 4번 #################
#smk <- subset(health, !is.na(흡연상태), select=c(흡연상태, 총콜레스테롤))
tapply(health$총콜레스테롤, health$흡연상태, ad.test)
bartlett.test(health$총콜레스테롤, health$흡연상태, data=health)
health.lm <- lm(health$총콜레스테롤 ~ health$흡연상태, data=health)
anova(health.lm)


############### 5번 #################

health2 <- health %>% 
  mutate(agedae = ifelse(연령대코드.5세단위. >= 16 , "5",
                              ifelse(연령대코드.5세단위. < 16, "4",
                                          ifelse(연령대코드.5세단위. < 12, "3",
                                                      ifelse(연령대코드.5세단위. < 8, "2",
                                                                  ifelse(연령대코드.5세단위. < 4, "1", ""))))))

#transform(health, 5 = ifelse(연령대코드>=17),
#          4 = ifelse(연령대코드>12),
#          3 = ifelse(연령대코드>8),
#          2 = ifelse(연령대코드>4),
#          1 = ifelse(연령대코드>0))

tapply(health2$신장.5Cm단위., health2$agedae, ad.test)
bartlett.test(신장.5Cm단위., agedae, data=health2)
health2.lm <- lm(신장.5Cm단위. ~ agedae, data=health2)
anova(health2.lm)


############### 6번 #################
names(health)[23:25] <-c("AST", "ALT","감마지티피")
cor.pearson <- cor.test(~ AST + ALT, 
                        method="pearson", data=health)
cor.pearson

cor.pearson <- cor.test(~ AST + 감마지티피, 
                        method="pearson", data=health)
cor.pearson

cor.pearson <- cor.test(~ ALT+ 감마지티피, 
                        method="pearson", data=health)
cor.pearson
#cor.spearman <- cor.test(~ .혈청지오티.AST + .혈청지오티.ALT + 감마지티피, 
#                        method="spearman", data=health)
#cor.kendall <- cor.test(~ .혈청지오티.AST + .혈청지오티.ALT + 감마지티피, 
#                        method="kendall", data=health)


############### 7번 #################
#lm(종속변수 ~ 독립변수) 회귀분석
names(health)[6]<-c("체중")
health <- transform(health, 피지않는다 = ifelse(흡연상태=="1", 1, 0),
                    피웠으나끊었다 = ifelse(흡연상태 == "2", 1, 0),
                    피고있다 = ifelse(흡연상태 == "3", 1, 0))
weights.lm <- lm(체중 ~ 식전혈당.공복혈당. + 총콜레스테롤 + 트리글리세라이드 +
                     혈색소+ 혈청크레아티닌 + 감마지티피 + 피지않는다 + 피웠으나끊었다
                   +피고있다, data=health)
summary(weights.lm)

install.packages("car")
libray(car)

health8 <- health
names(health8)[7] <- 'weight5kg'
names(health8)[15] <- 'bloodsugarb'
names(health8)[16] <-'allchol'
names(health8)[17] <- 'tri'
names(health8)[20] <- 'hemo'
names(health8)[22] <- 'crea'
names(health8)[26] <- 'smoking'

health8lm <- lm(weight5kg ~ bloodsugarb + allchol + tri + hemo + crea + rGT + smoking, data=health8)
vif(health8lm)
summary(health8lm)

healthmodel=step(health8lm, direction = "both")
health8lm2 <- lm(weight5kg ~ bloodsugarb + allchol + tri + hemo + crea + rGT + smoking, data=health8)
summary(health8lm2)


#~~이 음주여부에 미치는 영향(8번)
health9 <- health

names(health9)[7] <- 'weight5kg'
names(health9)[15] <- 'bloodsugarb'
names(health9)[16] <-'allchol'
names(health9)[17] <- 'tri'
names(health9)[20] <- 'hemo'
names(health9)[22] <- 'crea'
names(health9)[26] <- 'smoking'
names(health9)[27] <- 'drinking'
names(health9)[3] <- 'gender'

health9lm <- glm(drinking~factor(gender)+bloodsugarb + allchol + tri + hemo + crea + rGT,data=health9,family = binomial)
summary(health9lm)
healthmodel2=step(health9lm, direction = "both")

health9lm2 <- glm(drinking ~ factor(gender) + bloodsugarb + allchol + tri + hemo + crea + rGT,data=health9,family = binomial)
exp(cbind(OR=coef(health9lm2), confint(health9lm2)))
