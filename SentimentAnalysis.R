library(cld3)
library(tm)
library(SnowballC)
library(dplyr)
library(stringr)
library(SentimentAnalysis)


sessionInfo()

load("./data.corona.RData")
colnames(data.corona)


data.corona$Language<- detect_language(data.corona$text)
data.corona$text <- iconv(data.corona$text, 'UTF-8', 'ASCII')
data.en.noretweet <- data.corona
data.en.noretweet <- data.en.noretweet[which(data.en.noretweet$isRetweet==FALSE&data.en.noretweet$Language=="en"),]



removeURL<- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct<- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
usableText<- function(x) stringr::str_replace_all(x,"[^[:graph:]]", " ")

corpus<- Corpus(DataframeSource(data.en.noretweet))
corpus<- tm_map(corpus, usableText)
corpus<- tm_map(corpus, removeURL)
corpus<- tm_map(corpus, tolower)
corpus<- tm_map(corpus, removePunctuation)
corpus<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, removeNumPunct)
corpus<- tm_map(corpus, removeWords, stopwords("english"))


for(i in 1:length(corpus)){
  data.en.noretweet$text.tm[i] <- strwrap(corpus[[i]], 10000)
}
data.en.noretweet$text.tm[1]

dtm<- DocumentTermMatrix(corpus)
dtm

data.en.noretweet.dtm2<- removeSparseTerms(dtm, sparse= 0.9975)
data.en.noretweet.dtm2

df<- as.data.frame(as.matrix(data.en.noretweet.dtm2))
freq<- sort(colSums(df), decreasing = T)
freq.data<- data.frame(words= names(freq), freq = freq)
freq.data


sentiments.corona<- analyzeSentiment(data.en.noretweet$text.tm)
attach(data.en.noretweet)
attach(sentiments.corona)
sentiments<- data.frame(SentimentLM, SentimentQDAP, Region, WordCount)

library(ggpubr)
compare_means(SentimentLM~Region, sentiments, method = "wilcox.test")