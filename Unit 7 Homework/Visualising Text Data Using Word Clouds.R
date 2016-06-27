#Unit 7 Homework - Visualizing Text Data Using Word Clouds

# 1
tweets = read.csv("tweets.csv", stringsAsFactors = F)

library(tm)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))

str(allTweets)
ncol(allTweets)

# 2
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)
?wordcloud

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

# 3

negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

# 4
display.brewer.all()
brewer.pal(name = Set2)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors = brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors = brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors = brewer.pal(9, "Blues")[c(1, 2, 3, 4)])
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors = brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
brewer.pal(9, "Blues")[-1:-4]
brewer.pal(9, "Blues")[5:9]



