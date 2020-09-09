setwd("C:/Rproject")
# J 산업군에서 
#################### 연차별 엥겔지수
#범주 - 연속 : 분산분석
cworkers <- read.csv("가계동향조사_J개인.csv", head=TRUE)

#정규성
tapply(cworkers$엥겔지수, cworkers$연차구분, shapiro.test)
#등분산성
bartlett.test(cworkers$엥겔지수, cworkers$연차구분)

jengel.lm <- lm(엥겔지수 ~ 연차구분, data=cworkers)
anova(jengel.lm)
summary(jengel.lm)

plotly::plot_ly(ggplot2::diamonds, y = ~cworkers$엥겔지수, x = ~cworkers$연차구분, type = "box")


#################### 연차별 여가지수
tapply(cworkers$여가지수, cworkers$연차구분, shapiro.test)
bartlett.test(cworkers$여가지수, cworkers$연차구분)

#정규성
cworkers2 <- cworkers %>% filter(여가지수 <= 20)
tapply(cworkers2$여가지수, cworkers2$연차구분, shapiro.test)

#등분산성
bartlett.test(cworkers2$여가지수, cworkers2$연차구분)

jengel.lm <- lm(여가지수 ~ 연차구분, data=cworkers2)
anova(jengel.lm)
oneway.test(여가지수 ~ 연차구분, data=cworkers2)#, var.equal=FALSE)
model <- aov(여가지수 ~ 연차구분, data=cworkers2)
comparison <- LSD.test(model, "연차구분", p.adj="bonferroni", group=T)
comparison
groupA <- subset(cworkers, 연차구분==2)
groupB <- subset(cworkers, 연차구분==5)
mean(groupA$여가지수)
mean(groupB$)
mean(cworkers$여가지수)
library(agricolae)
summary(jengel.lm)

plotly::plot_ly(ggplot2::diamonds, y = ~cworkers2$여가지수, x = ~cworkers2$연차구분, type = "box")



################## 연차별 행복지수
#정규성
cworkers2 <- cworkers %>% filter(행복지수 <= 250)
tapply(cworkers2$행복지수, cworkers2$연차구분, shapiro.test)

#등분산성
bartlett.test(cworkers2$행복지수, cworkers2$연차구분)
# 
test.lm <- lm(행복지수 ~ 연차구분, data=cworkers2)
anova(test.lm)

oneway.test(행복지수 ~ 연차구분, data=cworkers2, var.equal=FALSE)
model <- aov(행복지수 ~ 연차구분, data=cworkers2)
comparison <- LSD.test(model, "연차구분", p.adj="bonferroni", group=T)
comparison
groupA <- subset(cworkers, 연차구분==2)
groupB <- subset(cworkers, 연차구분==5)
mean(groupA$여가지수)
mean(groupB$)
mean(cworkers$여가지수)
library(agricolae)
summary(jengel.lm)

plotly::plot_ly(ggplot2::diamonds, y = ~cworkers2$행복지수, x = ~cworkers2$연차구분, type = "box")
