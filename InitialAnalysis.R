library(slam)
library(tm)
library(ggplot2)
library(wordcloud)
library(tau)
library(stringr)
setwd("c:\\Personal\\Git\\CourseraCapStone")
filetw <- ".\\Data\\en_US\\en_US.twitter.txt"
filenews <- ".\\Data\\en_US\\en_US.news.txt"
fileblogs <- ".\\Data\\en_US\\en_US.blogs.txt"

twsize <-file.info(filetw)["size"][[1]]/(1024*1000)
newssize <-file.info(filenews)["size"][[1]]/(1024*1000)
blogssize <-file.info(fileblogs)["size"][[1]]/(1024*1000)

numwordstw <- system("c:\\Rtools\\bin\\wc -w c:\\Course\\Capstone\\final\\en_US\\en_US.twitter.txt", intern = TRUE)
numwordstw <- as.numeric(strsplit(numwordstw, " ")[[1]][1])

numwordsnews <- system("c:\\Rtools\\bin\\wc -w c:\\Course\\Capstone\\final\\en_US\\en_US.news.txt", intern = TRUE)
numwordsnews <- as.numeric(strsplit(numwordsnews, " ")[[1]][1])

numwordsblog <- system("c:\\Rtools\\bin\\wc -w c:\\Course\\Capstone\\final\\en_US\\en_US.blogs.txt", intern = TRUE)
numwordsblog <- as.numeric(strsplit(numwordsblog, " ")[[1]][1])

numlinestw <- system("c:\\Rtools\\bin\\wc -l c:\\Course\\Capstone\\final\\en_US\\en_US.twitter.txt", intern = TRUE)
numlinestw <- as.numeric(strsplit(numlinestw, " ")[[1]][1])

numlinesnews <- system("c:\\Rtools\\bin\\wc -l c:\\Course\\Capstone\\final\\en_US\\en_US.news.txt", intern = TRUE)
numlinesnews <- as.numeric(strsplit(numlinesnews, " ")[[1]][1])

numlinesblog <- system("c:\\Rtools\\bin\\wc -l c:\\Course\\Capstone\\final\\en_US\\en_US.blogs.txt", intern = TRUE)
numlinesblog <- as.numeric(strsplit(numlinesblog, " ")[[1]][1])



desc<-c("Twitter","News","Blogs")
words<-c(numwordstw,numwordsnews,numwordsblog)
lines<-c(numlinestw,numlinesnews,numlinesblog)
size_mb<-c(twsize,newssize,blogssize)


data.frame(desc,words,lines,size_mb)

sample.tw <- readLines(filetw, n=10000)
corpus.sample.tw <- Corpus(VectorSource(sample.tw))

inspect(corpus.sample.tw[1:3])

stop.words <- stopwords("english")
swear.words <- read.table(file ='http://www.bannedwordlist.com/lists/swearWords.txt', stringsAsFactors=F)
filter.words<-c(stop.words,swear.words)

data.tw <- TermDocumentMatrix(corpus.sample.tw,
                              control = list(removePunctuation = FALSE,
                                             stopwords = filter.words,
                                             removeNumbers = TRUE, tolower = TRUE,
                                             minWordLength = 1))


data.tw.matrix <- as.matrix(data.tw)
word_freqs <- sort(rowSums(data.tw.matrix), decreasing=TRUE) 
data.tw.freq.unigram <- data.frame(word=names(word_freqs), freq=word_freqs)

set.seed(1234)
wordcloud(data.tw.freq.unigram[,1], data.tw.freq.unigram[,2], min.freq = 100)

ggplot(data = subset(data.tw.freq.unigram,freq>150),aes(x = reorder(word,-freq),y=freq)) +
  geom_bar(stat = "identity", fill="red") + 
  theme(axis.text.x=element_text(angle =90, vjust =0.5))+
  ggtitle('Unigram') + labs(x="words")


bigrams <- textcnt(sample.tw, n = 2, method = "string")
bigrams <- bigrams[order(bigrams, decreasing = TRUE)]

data.tw.freq.bigram <- data.frame(word=names(bigrams), freq=bigrams)
wordcloud(data.tw.freq.bigram[,1],data.tw.freq.bigram[,2],min.freq=100)

ggplot(data = subset(data.tw.freq.bigram,freq>100),aes(x = reorder(word,-freq),y=freq)) +
  geom_bar(stat = "identity", fill="steelblue") + 
  theme(axis.text.x=element_text(angle =90, vjust =0.5))+
  ggtitle('Bigram') + labs(x="words")

trigrams <- textcnt(sample.tw, n = 3, method = "string")
trigrams <- trigrams[order(trigrams, decreasing = TRUE)]


data.tw.freq.trigram <- data.frame(word=names(trigrams), freq=trigrams)
wordcloud(data.tw.freq.trigram[,1],data.tw.freq.trigram[,2],min.freq=20)

ggplot(data = subset(data.tw.freq.trigram,freq>20),aes(x = reorder(word,-freq),y=freq)) +
  geom_bar(stat = "identity", fill="red") + 
  theme(axis.text.x=element_text(angle =90, vjust =0.5))+
  ggtitle('Trigram') + labs(x="words")

quadgrams <- textcnt(sample.tw, n = 4, method = "string")
quadgrams <- quadgrams[order(quadgrams, decreasing = TRUE)]


data.tw.freq.quadgram <- data.frame(word=names(quadgrams), freq=quadgrams)
wordcloud(data.tw.freq.quadgram[,1],data.tw.freq.quadgram[,2],min.freq=5)

ggplot(data = subset(data.tw.freq.quadgram,freq>5),aes(x = reorder(word,-freq),y=freq)) +
  geom_bar(stat = "identity", fill="red") + 
  theme(axis.text.x=element_text(angle =90, vjust =0.5))+
  ggtitle('Quadgram') + labs(x="words")

quingrams <- textcnt(sample.tw, n = 5, method = "string")
quingrams <- quingrams[order(quingrams, decreasing = TRUE)]

data.tw.freq.quingram <- data.frame(word=names(quingrams), freq=quingrams)
wordcloud(data.tw.freq.quingram[,1],data.tw.freq.quingram[,2],min.freq=4)

ggplot(data = subset(data.tw.freq.quingram,freq>2),aes(x = reorder(word,-freq),y=freq)) +
  geom_bar(stat = "identity", fill="red") + 
  theme(axis.text.x=element_text(angle =90, vjust =0.5))+
  ggtitle('Quingram') + labs(x="words")



