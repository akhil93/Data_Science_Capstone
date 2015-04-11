#load the datasets
news <- readLines("en_US.news.txt", encoding = "UTF-8")
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8")
blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8")

# # Statistics
# library(stringi)
# library(stringr)
# number.lines=cbind(stri_stats_general(news)["Lines"],stri_stats_general(twitter)["Lines"],stri_stats_general(blogs)["Lines"])
# number.words=c(sum(stri_count_words(news)),sum(stri_count_words(twitter)),sum(stri_count_words(blogs)))
# summary_table=rbind(number.lines,number.words)
# rownames(summary_table)=c("Number of Lines","Number of words")
# colnames(summary_table)=c("Blogs","Twitter","News")
# summary_table
# Sampling
set.seed(48)
news.sample <- sample(news, 1000, replace = FALSE)
twitter.sample <- sample(twitter, 1000, replace = FALSE)
blogs.sample <- sample(blogs, 1000, replace = FALSE)

# #statistics
# number.lines=cbind(stri_stats_general(news.sample)["Lines"],stri_stats_general(twitter.sample)["Lines"],stri_stats_general(blogs.sample)["Lines"])
# number.words=c(sum(stri_count_words(news.sample)),sum(stri_count_words(twitter.sample)),sum(stri_count_words(blogs.sample)))
# summary_table=rbind(number.lines,number.words)
# rownames(summary_table)=c("Number of Lines","Number of words")
# colnames(summary_table)=c("News","Twitter","Blogs")
# 
# summary_table

rm(news,twitter,blogs)

# w.blogs <- stri_count_words(blogs.sample)
# w.news <- stri_count_words(news.sample)
# w.twitter <- stri_count_words(twitter.sample)
# par(mfrow= c(1,3))
# hist(w.twitter[w.twitter<= 50],col="orange",xlab="Twitter",
#      ylab="Frequency",main="Number of words in each line in Twitter")
# hist(w.news[w.news<= 50],col="orange",xlab="News",
#      ylab="Frequency",main="Number of words in each line in News")
# hist(w.blogs[w.blogs<= 150],col="orange",xlab="News",
#      ylab="Frequency",main="Number of words in each line in Blogs")
#Exporting
file.create("C:/DS Capstone/final/en_US/news.sample.txt")
write(news.sample, file = "C:/DS Capstone/final/en_US/news.sample.txt")
file.create("C:/DS Capstone/final/en_US/twitter.sample.txt")
write(twitter.sample, file = "C:/DS Capstone/final/en_US/twitter.sample.txt")
file.create("C:/DS Capstone/final/en_US/blogs.sample.txt")
write(blogs.sample, file = "C:/DS Capstone/final/en_US/blogs.sample.txt")

# Loading Sample Files

news.sample <- readLines("news.sample.txt", encoding = "UTF-8")
blogs.sample <- readLines("blogs.sample.txt", encoding = "UTF-8")
twitter.sample <- readLines("twitter.sample.txt", encoding = "UTF-8")

#Corpus

library(tm)
library(SnowballC)
new.data <- c(news.sample,twitter.sample,blogs.sample)
corpus <- Corpus(VectorSource(new.data))
remove.decimals <- function(x) {gsub("([0-9]*)\\.([0-9]+)", "\\1 \\2", x)}
remove.hashtags <- function(x) { gsub("#[a-zA-z0-9]+", " ", x)}
remove.noneng <- function(x) {gsub("\\W+", " ",x)}
seperate_apostrophe <- function(x){
  gsub("aren't", "are not",x)
  gsub("can't","cannot", x)
  gsub("couldn't","could not",x)
  gsub("didn't","did not",x)
  gsub("doesn't","does not",x)
  gsub("don't","do not",x)
  gsub("hadn't","had not",x)
  gsub("hasn't","has not",x)
  gsub("haven't","have not",x)
  gsub("he'd", "he would",x)
  gsub("he'll", "he will",x)
  gsub("he's", "he is",x)
  gsub("i'd", "i would",x)
  gsub("i'll", "i will",x)
  gsub("i'm", "i am", x)
  gsub("i've", "i have",x)
  gsub("isn't", "is not",x)
  gsub("it's", "it is",x)
  gsub("let's","let us",x)
  gsub("mustn't", "must not",x)
  gsub("shan't", "shall not",x)
  gsub("she'd","she had",x)
  gsub("she'll", "she will",x)
  gsub("she's", "she is",x)
  gsub("shouldn't", "should not",x)
  gsub("that's", "that is",x)
  gsub("there's", "there is",x)
  gsub("they'd", "they would",x)
  gsub("they'll","they will",x)
  gsub("they're", "they are",x)
  gsub("they've", "they have",x)
  gsub("we'd", "we had",x)
  gsub("we're", "we are",x)
  gsub("we've", "we have",x)
  gsub("weren't", "were not",x)
  gsub("what'll", "what will",x)
  gsub("what're", "what are",x)
  gsub("what's", "what is",x)
  gsub("what've", "what have",x)
  gsub("where's", "where is",x)
  gsub("who'd", "who had",x)
  gsub("who'll", "who will",x)
  gsub("who're", "who are",x)
  gsub("who's", "who is", x)
  gsub("who've", "who have",x)
  gsub("won't", "will not",x)
  gsub("wouldn't", "would not",x)
  gsub("you'd", "you had",x)
  gsub("you'll", "you will",x)
  gsub("you're", "you are",x)
  gsub("you've", "you have",x)
}
corpus <- tm_map(corpus, remove.decimals)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, seperate_apostrophe)
corpus <- tm_map(corpus, remove.hashtags)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, remove.noneng)
profanity <- read.csv("Terms-to-block.csv", header = F)
profanity <- rep(profanity$V1)
corpus <- tm_map(corpus, removeWords, profanity)
corpus <- Corpus(VectorSource(corpus))

library(RWeka)
options(mc.cores =1)
Uni.Gram_Tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
Bi.Gram_Tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
Tri.Gram_Tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
Quad.Gram_Tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
Five.Gram_Tokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
TDM_UniGram <- TermDocumentMatrix(corpus, control = list(tokenize = Uni.Gram_Tokenizer))
TDM_BiGram <- TermDocumentMatrix(corpus, control = list(tokenize = Bi.Gram_Tokenizer))
TDM_TriGram <- TermDocumentMatrix(corpus, control = list(tokenize = Tri.Gram_Tokenizer))
TDM_QuadGram <- TermDocumentMatrix(corpus, control = list(tokenize = Quad.Gram_Tokenizer))
TDM_FiveGram <- TermDocumentMatrix(corpus, control = list(tokenize = Five.Gram_Tokenizer))



Ngram_data<-function (x) {
  Ngram_data<-as.data.frame(inspect(x))           # Converting the TDM in to a data frame.
  Ngram_data$Count<-rowSums(Ngram_data)
  Ngram_data<-subset(Ngram_data, Count> 1)          # Selecting words which appear morethan once.
  Ngram_data$Terms<-row.names(Ngram_data)
  Ngram_data<-Ngram_data[order(-Ngram_data$Count),]
  row.names(Ngram_data)<-NULL
  Ngram_data$Probability<-Ngram_data$Count/sum(Ngram_data$Count)
  df_ngram_final<-subset(Ngram_data, select=c("Terms","Count","Probability"))  
}

Ong_gram_data <- Ngram_data(TDM_UniGram)
Two_gram_data <- Ngram_data(TDM_BiGram)
Three_gram_data <- Ngram_data(TDM_TriGram)
Four_gram_data <- Ngram_data(TDM_QuadGram)
Five_gram_data <- Ngram_data(TDM_FiveGram)

one <- read.csv("one_gram_data.csv")
two <- read.csv("two_gram_data.csv")
three <- read.csv("Three_gram_data.csv")
four <- read.csv("Four_gram_data.csv")
five <- read.csv("Five_gram_data.csv")

clean_function <- function(x){
  x <- tolower(x)
  x <- str_replace_all(x, "[[:punct:]]", "")
  x <- str_replace_all(x, "[[:digit:]]", "")
  x <- str_replace_all(x, "\\W+", " ")
  x <- str_replace_all(x, "\\s+", " ")
  return(x)
}

bigram.pred <- function(x){
  elements <- unlist(strsplit(x, " "))
  last.element <- tail(elements,1)
  regeX <- paste("^", last.element, sep = "")
  indices <- grep(regeX, two$Terms)
  lookup <- two[indices,]
  temp.df <- data.frame()
  for(i in 1:nrow(lookup)){
    lookup$Terms <- as.character(lookup$Terms)
    elements1 <- unlist(strsplit(lookup$Terms, " "))
    temp1.df<-matrix(elements1, ncol=2, byrow=TRUE)
    lookup.element<-temp1.df[,2]
    regeX1 <- paste("^", lookup.element[i], sep="")
    one.g_lookup<-one[grep(regeX1, one$terms),]
    one.g_lookup<-data.frame(one.g_lookup)
    prob <- lookup[i,4]/one.g_lookup[,4]
    temp.df<-rbind(temp.df,data.frame("Terms"= one.g_lookup$terms, "Probability"=prob))
  }
  ordered.prob <- temp.df[order(temp.df$Probability),]
  potential.words<-as.character(ordered.prob$Terms)
  return(potential.words)
}


trigram.pred <- function(x){
  elements <- unlist(strsplit("we went to the new york", " "))
  last.element <- tail(elements,2)
  last.element1 <- paste(last.element[1],last.element[2], sep = " ")
  regeX <- paste("^", last.element1, sep = "")
  indices <- grep(regeX, three$Terms)
  lookup <- three[indices,]
  temp.df <- data.frame()
  for(i in 1:nrow(lookup)){
    lookup$Terms <- as.character(lookup$Terms)
    elements1 <- unlist(strsplit(lookup$Terms, " "))
    temp1.df<-matrix(elements1, ncol=3, byrow=TRUE)
    lookup.element<-temp1.df[,3]
    regeX1 <- paste("^", lookup.element[i], sep="")
    one.g_lookup<-one[grep(regeX1, one$terms),]
    one.g_lookup<-data.frame(one.g_lookup)
    prob <- lookup[i,4]/one.g_lookup[,4]
    temp.df<-rbind(temp.df,data.frame("Terms"= one.g_lookup$terms, "Probability"=prob))
    }
  ordered.prob <- temp.df[order(temp.df$Probability),]
  potential.words<-as.character(ordered.prob$Terms)
  potential.words
}


quadgram.pred <- function(x){
  elements <- unlist(strsplit("we went to the new york", " "))
  last.element <- tail(elements,3)
  last.element1 <- paste(last.element[1],last.element[2],last.element[3] sep = " ")
  regeX <- paste("^", last.element1, sep = "")
  indices <- grep(regeX, four$Terms)
  lookup <- four[indices,]
  temp.df <- data.frame()
  for(i in 1:nrow(lookup)){
    lookup$Terms <- as.character(lookup$Terms)
    elements1 <- unlist(strsplit(lookup$Terms, " "))
    temp1.df<-matrix(elements1, ncol=4, byrow=TRUE)
    lookup.element<-temp1.df[,4]
    regeX1 <- paste("^", lookup.element[i], sep="")
    one.g_lookup<-one[grep(regeX1, one$terms),]
    one.g_lookup<-data.frame(one.g_lookup)
    prob <- lookup[i,4]/one.g_lookup[,4]
    temp.df<-rbind(temp.df,data.frame("Terms"= one.g_lookup$terms, "Probability"=prob))
  }
  ordered.prob <- temp.df[order(temp.df$Probability),]
  potential.words<-as.character(ordered.prob$Terms)
  potential.words
}


