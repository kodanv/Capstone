library(tm)
library(stylo)
library(ggplot2)

# setting working directory
setwd("C:\\Users\\akodirov\\Documents\\Capstone")

# setting seed for reproducibility
set.seed(1111)


###########################
### Creating corpus blogs
blogs <- Corpus(DirSource("C:\\Users\\akodirov\\Documents\\Capstone\\blogs", encoding = "UTF-8"), readerControl=list(language="en"))

# unlist blogs corpus into string vectors
blogs.v <- unlist(blogs)

# number of elements (strings) - 899,299
length(blogs.v)

# converting to list by removing non-word chacaters
blogs.spl <- strsplit(blogs.v, "\\W")

# converting into words vector
blogs.v <- unlist(blogs.spl)

#removing the blanks
blogs.v <- blogs.v[which(blogs.v != "")]

# total number of words in the blogs vector - 38,309,378
length(blogs.v)

# number of unique words in the vector - 344,461; unique word rate 0.9%
length(unique((blogs.v)))

############### Training and Testing Set sampling

# assigning a sample rate
smp.rate <- 0.6

# reshuffle for random smapling
shuffle.blogs <- sample(blogs[[1]][[1]], length(blogs[[1]][[1]]))
split.index <- smp.rate * length(shuffle.blogs)
train.blogs <- shuffle.blogs[1: split.index]
test.blogs <- shuffle.blogs[-(1:split.index)]

## write to file
write(train.blogs,"train.blogs.txt")
write(test.blogs,"test.blogs.txt")

## cleaning the environment
rm(list = ls())
set.seed(1111)
##########################
### Creating corpus tweets

tweets <- Corpus(DirSource("C:\\Users\\akodirov\\Documents\\Capstone\\tweets", encoding = "UTF-8"), readerControl=list(language="en"))

# unlist tweets corpus into string vectors
tweets.v <- unlist(tweets)

# number of elements (strings) - 2,360,159
length(tweets.v)

# converting to list by removing non-word chacaters
tweets.spl <- strsplit(tweets.v, "\\W")

# converting into words vector
tweets.v <- unlist(tweets.spl)

#removing the blanks
tweets.v <- tweets.v[which(tweets.v != "")]

# total number of words in the tweets vector - 31,003,099
length(tweets.v)

# number of unique words in the vector - 440,024; unique word rate 1.4%
length(unique((tweets.v)))

############### Training and Testing Set sampling

# assigning a sample rate
smp.rate <- 0.6

# reshuffle for random smapling
shuffle.tweets <- sample(tweets[[1]][[1]], length(tweets[[1]][[1]]))
split.index <- smp.rate * length(shuffle.tweets)
train.tweets <- shuffle.tweets[1: split.index]
test.tweets <- shuffle.tweets[-(1:split.index)]

## write to file
write(train.tweets,"train.tweets.txt")
write(test.tweets,"test.tweets.txt")

## cleaning the environment
rm(list = ls())
set.seed(1111)
##########################
### Creating corpus news
news <- Corpus(DirSource("C:\\Users\\akodirov\\Documents\\Capstone\\news", encoding = "UTF-8"), readerControl=list(language="en"))

# unlist news corpus into string vectors
news.v <- unlist(news)

# number of elements (strings) - 77,270
length(news.v)

# converting to list by removing non-word chacaters
news.spl <- strsplit(news.v, "\\W")

# converting into words vector
news.v <- unlist(news.spl)

#removing the blanks
news.v <- news.v[which(news.v != "")]

# total number of words in the news vector - 2,741,608
length(news.v)

# number of unique words in the vector - 90,748; unique word rate 3.3%
length(unique((news.v)))

############### Training and Testing Set sampling
# assigning a sample rate
smp.rate <- 0.6


shuffle.news <- sample(news[[1]][[1]], length(news[[1]][[1]]))
split.index <- smp.rate * length(shuffle.news)
train.news <- shuffle.news[1: split.index]
test.news <- shuffle.news[-(1:split.index)]

## write to file
write(train.news,"train.news.txt")
write(test.news,"test.news.txt")

## cleaning the environment
rm(list = ls())

################################################
################################################
## reading all three sample documents into corpus
corp <- Corpus(DirSource("C:\\Users\\akodirov\\Documents\\Capstone\\training2", encoding = "UTF-8"), readerControl=list(language="en"))

# converting to lower-case
corp.lower = tm_map(corp, tolower)

# converting to plain text
corpus = tm_map(corp.lower, PlainTextDocument)

# removing punctuation
corpus = tm_map(corpus, removePunctuation)


# removing digits
corpus <- tm_map(corpus, removeNumbers)

# removing stopwords
#corpus <- tm_map(corpus, removeWords, c("a","an","the"))

# removing extra spaces
corpus <- tm_map(corpus, stripWhitespace)

# tokenizing into words
corpus.words<- txt.to.words(corpus)

# corpus words vector
corpus.words.v <- unlist(corpus.words)

# unigram data frames
df.1gram <-data.frame(table(make.ngrams(corpus.words.v, ngram.size = 1)))

# bigram data frames
df.2gram <-data.frame(table(make.ngrams(corpus.words.v, ngram.size = 2)))

# trigram data frames
df.3gram <-data.frame(table(make.ngrams(corpus.words.v, ngram.size = 3)))

# quadragram data frames
df.4gram <-data.frame(table(make.ngrams(corpus.words.v, ngram.size = 4)))

# pentagram data frames
df.5gram <-data.frame(table(make.ngrams(corpus.words.v, ngram.size = 5)))

# sorting by frequency
df.1gram.sorted <- df.1gram[order(df.1gram$Freq, decreasing = TRUE),]
df.2gram.sorted <- df.2gram[order(df.2gram$Freq, decreasing = TRUE),]
df.3gram.sorted <- df.3gram[order(df.3gram$Freq, decreasing = TRUE),]
df.4gram.sorted <- df.4gram[order(df.4gram$Freq, decreasing = TRUE),]
df.5gram.sorted <- df.5gram[order(df.5gram$Freq, decreasing = TRUE),]

#df.3gram.sorted[grep("^case of", df.3gram.sorted$Var1),]



####################################### removing single and two tones

df.1gram.cut<-df.1gram.sorted[which(df.1gram.sorted[,2] > 2),]
df.2gram.cut<-df.2gram.sorted[which(df.2gram.sorted[,2] > 2),]
df.3gram.cut<-df.3gram.sorted[which(df.3gram.sorted[,2] > 2),]
df.4gram.cut<-df.4gram.sorted[which(df.4gram.sorted[,2] > 2),]
df.5gram.cut<-df.5gram.sorted[which(df.5gram.sorted[,2] > 2),]

# df.4_4gram.cut<-df.4gram.sorted[which(df.4gram.sorted[,2] > 4),]
# df.5_4gram.cut<-df.5gram.sorted[which(df.5gram.sorted[,2] > 4),]

string<-c("the end of the")
sam_ple<- c("^", string)
sam_ple<- paste(sam_ple, collapse = "")

nxt_words <- grep(sam_ple,df.5gram.cut[,1], value = TRUE)
nxt_word <- strsplit(nxt_words[1], " ")
word_prob <- df.5gram.cut[which(df.5gram.cut$Var1 == nxt_words[1]),2] 

##### Number of ngrams
count_ngrams <- length(nxt_words)

##### all ngrams count
sum_prob_ngrams = 0
for (i in 1:count_ngrams) {sum_prob_ngrams<- sum_prob_ngrams + df.5gram.cut[which(df.5gram.cut$Var1 == nxt_words[i]),2] }

nxt_word <- paste(unlist(nxt_word))
nxt_word <- nxt_word[length(nxt_word)]


###### 4gram probs

### removing first word in the sample
next_sam_ple_func <- function(string) {
sam_ple<-unlist(strsplit(string, " "))
sam_ple<- sam_ple[2:length(sam_ple)]
sam_ple<- paste(sam_ple, collapse = " ")
sam_ple<- c("^", sam_ple)
sam_ple<- paste(sam_ple, collapse = "")
}
sam_ple <-next_sam_ple_func(string)


nxt_words2 <- grep(sam_ple,df.4gram.cut[,1], value = TRUE)
word_prob2 <- df.4gram.cut[which(df.4gram.cut$Var1 == nxt_words2[1]),2] 
word_prob22 <- df.4gram.cut[which(df.4gram.cut$Var1 == nxt_words2[2]),2] 

##### Number of 4grams
count_ngrams2 <- length(nxt_words2)

##### all ngrams count
sum_prob_ngrams2 = 0
for (i in 1:count_ngrams2) {sum_prob_ngrams2<- sum_prob_ngrams2 + df.4gram.cut[which(df.4gram.cut$Var1 == nxt_words2[i]),2] }


nxt_word2 <- strsplit(nxt_words[1], " ")
nxt_word2 <- paste(unlist(nxt_word2))
nxt_word2 <- nxt_word2[length(nxt_word2)]

nxt_word22 <- strsplit(nxt_words[2], " ")
nxt_word22 <- paste(unlist(nxt_word22))
nxt_word22 <- nxt_word22[length(nxt_word22)]

#################
discount <- 1
if (nxt_word == nxt_word2) { 
  if (word_prob/sum_prob_ngrams >= word_prob22*(count_ngrams*discount/sum_prob_ngrams)/(sum_prob_ngrams2-word_prob2)){
    nxt_word_final <- nxt_word
  } else {nxt_word_final <- nxt_word22}
} else  {if (word_prob/sum_prob_ngrams >= word_prob2*(count_ngrams*discount/sum_prob_ngrams)/(sum_prob_ngrams2-word_prob2))
  {
      nxt_word_final <- nxt_word
  } else {nxt_word_final <- nxt_word2}
}




#####################################################
# subsetting top 20 n-grams
top20.1gram <- df.1gram.sorted[1:20,]
colnames(top20.1gram) <- c("Unigram", "Frequency")

top20.2gram <- df.2gram.sorted[1:20,]
colnames(top20.2gram) <- c("Bigram", "Frequency")

top20.3gram <- df.3gram.sorted[1:20,]
colnames(top20.3gram) <- c("Trigram", "Frequency")



## Plotting top 20 ngrams, n = 1, 2, 3
ggplot(top20.1gram, aes(x = reorder(Unigram, - Frequency), y = Frequency)) + 
  geom_col(fill = "Green") + geom_text(aes(label = Frequency, angle = 25,vjust = -1, hjust = .2)) +
  xlab("Unigrams") +  ylab("Frequency") + theme(axis.text.x = element_text(angle = 25 ,hjust = 1)) + scale_y_continuous(limits = c(0, 220000))

ggplot(top20.2gram, aes(x = reorder(Bigram, - Frequency), y = Frequency)) + 
  geom_col(fill = "Red") + geom_text(aes(label = Frequency, angle = 25,vjust = -1,hjust = .2)) +
  xlab("Bigrams") + ylab("Frequency") + theme(axis.text.x = element_text (angle = 25 ,hjust = 1)) + scale_y_continuous(limits = c(0, 13000))

ggplot(top20.3gram, aes(x = reorder(Trigram, - Frequency), y = Frequency)) + 
  geom_col(fill = "Blue") + geom_text(aes(label = Frequency, angle = 25,vjust = -1,hjust = .2)) +
  xlab("Trigrams") + ylab("Frequency") + theme(axis.text.x = element_text(angle = 25 ,hjust = 1)) + scale_y_continuous(limits = c(0, 1400))


