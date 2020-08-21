if(!requireNamespace("devtools")) install.packages("devtools")

devtools::install_github("dkahle/ggmap")

library(ggmap)
register_google(key='AIzaSyD8EFREGPkSnVEisrBHieWaqtvOf4NymRc')

getwd()
setwd("C:/Rtest/ggmdata")

install.packages("ggplot2")

library(ggplot2)
library(dplyr)

# 서울시 로드맵
map <- get_map(location = "seoul", zoom = 14, maptype="roadmap", source = "google")
g <- ggmap(map)
print(g)

# 서울시 위성사진
map <- get_map(location = "seoul", zoom = 14, maptype = "satellite", source = "google")
g <- ggmap(map)
print(g)

map <- get_map(location = "seoul", zoom = 14, maptype = "toner-lite", source = "google")
g <- ggmap(map)
print(g)

landmarks <- c("nseoul tower, seoul", "city hall, seoul")
lbls <- cbind(geocode(landmarks), text=landmarks)
g <- ggmap(map)
g <- g + geom_point(data=lbls, aes(x=lon, y=lat), size=5, colour="orange")
g <- g + geom_point(data=lbls, aes(x=lon, y=lat), size=3, colour="red")
g <- g + geom_point(data=lbls, aes(x=lon, y=lat, label=text), size=5, colour="blue",
                    hjust=0, vjust=0)
print(g)

setwd("C:/Rtest/datas")

wifi <- read.csv("WIFI.csv", header = T)

View(wifi)
head(wifi)

attach(wifi)

#서울시 지도에 wifi 표시
bmap <- ggmap(get_googlemap(center = c("seoul"), zoom = 11,maptype = "roadmap")) +
  geom_point(data=wifi, aes(x=LON, y=LAT, colour=INSTL_DIV, size=4))
print(bmap)

bmap + facet_wrap(~INSTL_DIV)

ggplot(wifi, aes(x=factor(1))) + geom_bar(aes(fill=INSTL_DIV), width=1) +
  coord_polar(theta = "y") + xlab("") + ylab("")

ggplot(wifi, aes(CATEGORY)) + geom_bar(aes(fill = INSTL_DIV))

ggplot(wifi, aes(INSTL_DIV)) + geom_bar(aes(fill = CATEGORY))
