personality<- read.csv("personality_descriptions.csv")
head(personality)

install.packages("tm")
install.packages("SnowballC")
library(dplyr)
library(tm)
library(SnowballC)

removeURL<- function(x)gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

#How many personality attributes are there. The Big 5: OCEAN
personality%>%count(Personality)


## Preprocess
corpus<- Corpus(VectorSource(personality$text))
corpus<- tm_map(corpus, removeURL)
corpus<- tm_map(corpus, removeNumPunct)
corpus<- tm_map(corpus, tolower)
corpus<- tm_map(corpus, removePunctuation)
corps<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, removeWords, stopwords(kind = 'en'))

dtm<- DocumentTermMatrix(corpus)
dtm

dtm0<- removeSparseTerms(dtm, sparse = 0.98)
dtm0

dtm.df<- as.data.frame(as.matrix(dtm0))

## Exploratory Graph Analysis
install.packages("EGAnet")
library(EGAnet)

fit.topics<- EGA.fit(dtm.df, model= 'tmfg',plot.EGA = F)
fit.topics

topics$EntropyFit
topics<- EGA(dtm.df, model= 'tmfg', steps= 5)

## Which topic belongs to which node?
topics$dim.variables

netplot.topics <- plot(topics, label.size = 4, edge.size = 0.9,
                       node.size = colSums(topics$network)*4)
netplot.topics

netscores.topics<- net.scores(data= dtm.df, A= topics)
netscores.topics

scores.topics<- as.data.frame(netscores.topics$scores$std.scores)
colnames(scores.topics)<- paste0("Topics", colnames(scores.topics))

## Which score topic corresponds with a personality
scores.topics$Personality<- factor(personality$Personality)

## Visualize
library(ggpubr)
label<- colnames(scores.topics)[1:6]

## Average topic score per personality trait 
scores.plot<- ggline(scores.topics, x= "Personality", y= label, ylab= "Topic Score", combine= T, add= "mean_ci")+ theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
scores.plot

## Topic3 has a high average score for Neuroticism
## Topic6 has a higher average scores for Conscientiousness and Neuroticism
## In each topic, there are different levels/averages of each personality trait