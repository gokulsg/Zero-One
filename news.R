library(tidyverse)
library(tidytext)
library(topicmodels) 
library(tm)
library(SnowballC)
library(servr)
library(LDAvis)
library(stringi)
library(topicmodels)
library(dplyr)
library(ggplot2)
library(tm)
library(readr)
library(RSentiment)

ds <- read_csv("C:/Users/Gokul S/Desktop/MadurAI Hackathon/project/data12.csv")
View(ds)
data<-ds[c(1:10),]
View(data)

#data$content <- gsub("[^\x20-\x7E]", "", data$content)

#removing the characters other than unicode 8

phrase_clean <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", data$content)
data$content <- gsub("U00..", "", phrase_clean)

doc_ids <- c(1:10)
df <- data.frame(doc_id = doc_ids, text = data$content, stringsAsFactors = FALSE)
corpus <- Corpus(DataframeSource(df))
inspect(corpus)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus,removeWords,stopwords("english"))
corpus <- tm_map(corpus,removeWords,c("via", "news", "tweet", " ","@\\w+","http.+ |http.+$","amp"))
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus,stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
rowTotals<-apply(dtm,1,sum)
#empty.rows<-dtm[rowTotals==0,]$dimnames[1][[1]]
corpus<-corpus[-as.numeric(empty.rows)]
dtm <- DocumentTermMatrix(corpus)
inspect(dtm[1:5, 1:5])
findFreqTerms(dtm, 100)

dtm.mx <- as.matrix(dtm)
frequency <- colSums(dtm.mx)
frequency <- sort(frequency, decreasing=TRUE)
frequency[0:24] 

findAssocs(dtm, "aleppo", 0.1) 


burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("news1",k,"Topic.csv"))

#Identifying the terms

ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.topics,file=paste("news1",k,"Topic.csv"))
ldaOut.terms[1:6,]

# Generating probabilities

topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
topicProbabilities[1:5,]

#sentiment Analysis

calculate_sentiment(data$content)
