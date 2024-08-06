---
title: "OCEAN Exploratory Graph Analysis R Notebook"
output: html_notebook
---

# Load necessary packages

```{r}
install.packages('EGAnet')
library(EGAnet)
library(tm)
library(SnowballC)
library(dplyr)
library(ggpubr)
```

# Load and view data

```{r}
personality<- read.csv('personality_descriptions.csv')
head(personality)
attach(personality)
table(Personality)
```

## Preprocess text data

```{r}
removeURL<- function(x)gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

corpus<- Corpus(VectorSource(personality$text))
corpus<- tm_map(corpus, removeURL)
corpus<- tm_map(corpus, removeNumPunct)
corpus<- tm_map(corpus, tolower)
corpus<- tm_map(corpus, removePunctuation)
corpus<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, removeWords, stopwords(kind= 'en'))

dtm<- DocumentTermMatrix(corpus)
dtm 

dtm0<- removeSparseTerms(dtm, sparse= 0.98)
dtm0

dtm.df<- as.data.frame(as.matrix(dtm0))
```

## What does our neural Network look like, and what words belong to which topic?

```{r}
fit.topics<- EGA.fit(dtm.df, model= 'tmfg', plot.EGA = F)
fit.topics

fit.topics$EntropyFit

topics<- EGA(data= dtm.df, model= 'tmfg', steps= 5)

topics$dim.variables

```

# What is the network score per topic?

```{r}
netscores.topics<- net.scores(data= dtm.df, A= topics)
netscores.topics

scores.topics<- as.data.frame(netscores.topics$scores$std.scores)
colnames(scores.topics)<- paste0("Topics", colnames(scores.topics))

scores.topics$Personality<- factor(personality$Personality)
```

Each topic in our neural network has a different average of each of our 5 personality traits contained within it. For example, Topic 3 has the highest Neuroticism average. Topic6 has higher averages for Conscientiousness and Neuroticism. As can be seen below, some topics are more diverse than others.

```{r}
label<- colnames(scores.topics)[1:10]

scores.plot<- ggline(scores.topics, x= "Personality", y= label, ylab= "Topic Score", combine= T, add= "mean_ci")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
scores.plot
```
