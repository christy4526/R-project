apts <- ts(AirPassengers, frequency = 12)
plot(apts)

acf(apts)

pacf(apts)

spectrum(apts)

acf(diff(log(AirPassengers)))

pacf(diff(log(AirPassengers)))

install.packages("zoo")
library(zoo)

m<- lm(coredata(apts) ~ index(apts))
apts.eltr <- ts(resid(m), index(apts))
plot(apts.eltr)

plot(diff(log(apts)))

f <- decompose(apts)
attributes(f)

plot(f$figure, type="b", xaxt="n", xlabel="")

monthNames <- months(ISOdate(2012,1:12,1))
axis(1,at=1:12, labels=monthNames, las=2)

plot(f)

install.packages("forecast")

library(forecast)

apts.arima <- auto.arima(apts) # create arima
summary(apts.arima)

fore <- predict(apts.arima, n.ahead = 24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
apts.smoothing <- HoltWinters(apts, seasonal="mul")
fore2 <- predict(apts.smoothing, n.ahead=24)
ts.plot(apts, fore$pred, U, L, fore2, col=c(1,2,4,4,6), lty=c(1,1,2,2,3))
legend("topleft", c("Acual", "ARIMA", "ARIMA Error Bounds(95%, Confidence)", "exponential smoothing"), 
       col=c(1,2,4,6), lty=c(1,1,2,3))

a<-c(1,5)
b<-c(2,3)
c<-c(5,7)
d<-c(3,5)
e<-c(5,2)
data<-data.frame(a,b,c,d,e)
data<-t(data)
data

(m1<-hclust(dist(data)^2, method = "single"))
plot(m1)
(m2<-hclust(dist(data)^2, method = "complete"))
plot(m2)
(m3<-hclust(dist(data)^2, method = "ward.D2"))
plot(m3)
(m4<-hclust(dist(data)^2, method = "average"))
plot(m4)


rm (list=ls(all=TRUE))
data<-iris
head(data)

data$Species <- NULL
head(data)

(m <- kmeans(data,3))
table(iris$Species, m$cluster)

plot(data[c("Sepal.Length", "Sepal.Width")], main="kmeans", col=m$cluster)
plot(iris$Sepal.Length, iris$Sepal.Width, main="true", col=c(1,2,3)[unclass(iris$Species)])

(m <- kmeans(data,4))

table(iris$Species, m$cluster)
plot(data[c("Sepal.Length", "Sepal.Width")], col=m$cluster)

head(iris)
nrow(iris)
ncol(iris)
str(iris)


rm (list=ls(all=TRUE))
x <- c(0,2,5,6,5,7,9,11,5,7,9,13,15,17)
y <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)
par(mfrow=c(2,5))

plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="p")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="l")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="b")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="c")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="o")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="h")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="s")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="S")
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="n")

z<-c(3.5,1.5,2.3,6.6,4.7)
a<-c(2,7,12)
plot(x,y,main="PLOT", sub="Test", xlab="x-label", ylab="y-label", type="n")
points(z, pch=1, cex=1)
points(z, pch=3, cex=1)
points(z, pch=5, cex=1)
points(z, pch=7, cex=1)
points(z, pch=9, cex=1)
points(z, pch=11, cex=1)
points(z, pch=13, cex=1)
points(z, pch=15, cex=1)
points(z, pch=17, cex=1)
points(z, pch=19, cex=1)

plot(0:8, 0:8, type="n", ylim=c(0,20))
lines(c(2,6), c(20,20), lty=1)
lines(c(2,6), c(19,19), lty=2)

plot(x,y,type='n')
legend("center", "(x,y)", pch=1, title="centor")
legend("top", "(x,y)", pch=1, title="top")
legend("left", "(x,y)", pch=1, title="left")
legend("right", "(x,y)", pch=1, title="right")
legend("bottom", "(x,y)", pch=1, title="bottom")

x<- rnorm(1000, mean=5, sd=1)
hist(x)
hist(x, freq = F)
curve(dnorm(x, mean=5,sd=1),add=T)


rm (list=ls(all=TRUE))
x <- c(2,5,6,5,7,9,11,5,7,9,13,15,17)
y <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
boxplot(x,z)
pie(x)
barplot(x)
hist(x)

library(ggplot2)
test.data <- data.frame(length=c(3,2,5,8), width=c(4,3,6,9), depth=c(5,2,16,80),
                        trt=c("a","b","c","d"))
ggplot(test.data, aes(x=length, y=width))+geom_point(aes(color=trt))
