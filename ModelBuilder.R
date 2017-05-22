library(slam)
library(tm)
library(ggplot2)
library(wordcloud)
library(tau)
library(stringr)
library(quanteda)
library(readtext)
#do one at a time and combine
mytf3 <- readtext("c:\\Git\\CourseraCapStone\\Data\\en_US\\*.txt")

myCorpus <-corpus(mytf3)

myMat <- dfm(myCorpus, remove_punct=TRUE, ngrams=3)
trigram <- as.matrix(sort(colSums(myMat), decreasing=TRUE), remove_stopwords("english"), toLower=TRUE)

myMat <- dfm(myCorpus, remove_punct=TRUE, ngrams=2)
bigram <- as.matrix(sort(colSums(myMat), decreasing=TRUE), remove_stopwords("english"), toLower=TRUE)

myMat <- dfm(myCorpus, remove_punct=TRUE, ngrams=1)
unigram <- as.matrix(sort(colSums(myMat), decreasing=TRUE), remove_stopwords("english"), toLower=TRUE)

