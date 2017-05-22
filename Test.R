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

sample.tw <- readLines(filetw, n=100000)
a <- Corpus(VectorSource(sample.tw))
#a <- Corpus(DirSource("c:\\Personal\\Git\\CourseraCapStone\\Data\\en_US\\"))

a <- tm_map(a, removeNumbers)
a <- tm_map(a, removePunctuation)
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, tolower)
a <- tm_map(a, removeWords, stopwords("english")) 
# a <- tm_map(a, stemDocument, language = "english") 
# I also got it to work with stemming, but it takes so long...
adtm <-DocumentTermMatrix(a) 
adtm <- removeSparseTerms(adtm, 0.75)

inspect(adtm) 

findFreqTerms(adtm, lowfreq=10) # find terms with a frequency higher than 10
findAssocs(adtm, "usa",.5) # just looking for some associations  
findAssocs(adtm, "china",.5)

# Trigrams
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm <- TermDocumentMatrix(a, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)
inspect(tdm[1:5,1:5])