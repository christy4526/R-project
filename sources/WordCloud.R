install.packages("tm")
library(tm)
getwd()
setwd("C:/Program Files/R/R-4.0.2/library/tm/texts")
getwd()

txt <- system.file("texts", package = "tm")
txt
getReaders()

korea <- Corpus(DirSource(txt, encoding = "UTF-8"), readerControl = list(language="en"))
korea

korea[[1]]
korea <- VCorpus(VectorSource(korea))
korea

korea <-tm_map(korea, stripWhitespace)
korea
korea[[1]]

korea <- tm_map(korea, tolower)
korea[[1]]

korea <- tm_map(korea, removeWords, stopwords("english"))
korea[[1]]

korea <- tm_map(korea, removePunctuation)
korea[[1]]

korea <- tm_map(korea, removeNumbers)
korea[[1]]

install.packages("SnowballC")
library(SnowballC)

korea <- tm_map(korea, stemDocument)
korea[[1]]

korea <- tm_map(korea, PlainTextDocument)
korea[[1]]

kdtm <- DocumentTermMatrix(korea)
kdtm

findFreqTerms(kdtm, 10)



mykdtm <- TermDocumentMatrix(korea, control = list(wordLength=c(1, Inf)))

install.packages("wordcloud")
library(wordcloud)

w <- as.matrix(mykdtm)
wordFreq <- sort(rowSums(w), decreasing = TRUE)
grayLevels <- gray((wordFreq+10)/(max(wordFreq)+10))

wordcloud(words = names(wordFreq), freq=wordFreq, min.freq=3, random.order = F,
          colors = grayLevels)
