setwd("C:/Rtest")
install.packages("KoNLP")
install.packages("devtools")
devtools::install_github('haven-jeon/KoNLP')

options(buildtools.check = function(action) TRUE)
devtools::install_github("tidyverse/tidyr")
install.packages("devtools")
install.packages("agricolae")
install.packages("KoNLP")
install.packages("multilinguer")
library(multilinguer)
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
useSejongDic()


txt <- file("bigdata.txt", encoding="UTF-8")
text <- readLines(txt)
close(txt)
text
big <-sapply(text, extractNoun, USE.NAMES = F)
big

head(unlist(big), 30)
f<-unlist(big)

big <- Filter(function(x){nchar(x) >= 3}, f)
big <- gsub("같은경우는", "", big)
big <- gsub("어느정도", "", big)
big <- gsub("찌르는거", "", big)
big <- gsub("생각들어보고싶습니다", "", big)
big <- gsub("그자체로", "", big)
big <- gsub("것들이", "", big)
big <- gsub("관심을받", "", big)
big <- gsub("을", "", big)
big <- gsub("에서", "", big)
big <- gsub("이다", "", big)
big <- gsub("으로", "", big)
big <- gsub("이라는", "", big)

write(unlist(big), "bigdata_2.txt")
re <- read.table("bigdata_2.txt")
nrow(re)
textcount <- table(re)
head(sort(textcount, decreasing = T), 30)
library(wordcloud)
palete <- brewer.pal(9, "Set1")
wordcloud(names(textcount), freq = textcount, scale = c(5,1),
          rot.per=0.25, min.freq = 1, random.order = F, random.color = T,
          colors = palete)
