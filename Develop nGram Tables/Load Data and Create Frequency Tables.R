# R Script to produce basic 1,2,3 and 4-gram frequency tables from Corpus
# Code splits corpus into 70% training and 30% test datasets
# The training set is then further sub-sampled at 50%
# Four .csv files are produced with the n-gram frequency counts

# Load Libraries
setwd("C:/R Programming Course/Capstone Project/Project Files")
library(tm)
library(stylo)


# Read Corpus Data
text_twitter <- readLines("en_US.twitter.txt")
text_news <- readLines("en_US.news.txt")
text_blogs <- readLines("en_US.blogs.txt")
profane_words <- as.character(read.delim("bad-words.txt", header=FALSE, quote="", stringsAsFactors=FALSE))

totallines_twitter <- length(text_twitter)
totallines_news <- length(text_news)
totallines_blogs <- length(text_blogs)
size_twitter <- object.size(text_twitter)
size_news <- object.size(text_news)
size_blogs <- object.size(text_blogs)

# Creat Training and Test Data sets 70:30 ratio
df_twitter <- data.frame(text = text_twitter)
df_news <- data.frame(text = text_news)
df_blogs <- data.frame(text = text_blogs)
df_total <- rbind(df_twitter,df_news,df_blogs)
sample <- data.frame(num = sample(1:10,nrow(df_total),replace=T))
df_test <- data.frame(text = df_total[sample$num>7,])
df_training <- data.frame(text = df_total[sample$num<=7,])


# Further sub-sample of training data at 50%
perc_sample <- 50   #Change to 20 to sample 20% of the training set etc
#set.seed(42567)
sample1 <- data.frame(num = sample(1:100,nrow(df_training),replace=T))
df_training_sample <- data.frame(text = df_training[sample1$num<=perc_sample,])

# Convert Training data to Corpus and pre-process text
text_training <- DataframeSource(df_training_sample)
docs_training <- Corpus(text_training)
docs_training <-tm_map(docs_training,removePunctuation)
docs_training <-tm_map(docs_training,removeNumbers)
docs_training <-tm_map(docs_training,tolower)
docs_training <-tm_map(docs_training,stripWhitespace)
docs_training <-tm_map(docs_training,removeWords,profane_words)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x) # remove all non-aphanumerics
docs_training <- tm_map(docs_training, removeSpecialChars)

docs_training <- tm_map(docs_training, PlainTextDocument)
df_docs_training <- data.frame(text=unlist(sapply(docs_training,`[`,"content")),stringsAsFactors=F)

# Clean up files
rm("text_twitter","text_news","text_blogs","profane_words","df_twitter","df_news","df_blogs")
rm("df_total","df_test")
rm("df_training","df_training_sample")
rm("sample","sample1","text_training")

# Ngram development

# 1-grams

my.text <- txt.to.words(df_docs_training)
one_grams <- make.ngrams(my.text,ngram.size = 1)
one_grams_df <- data.frame(table(one_grams))
one_grams_df <- one_grams_df[order(one_grams_df$Freq,decreasing=TRUE),]
rownames(one_grams_df) <- c()
write.csv(one_grams_df, file = "one_grams_df_50.csv")
rm("one_grams_df","one_grams")

# 2-grams
my.text <- txt.to.words(df_docs_training$text)
rm("docs_training","df_docs_training")
two_grams <- make.ngrams(my.text,ngram.size = 2)
two_grams_df <- data.frame(table(two_grams))
two_grams_df <- two_grams_df[order(two_grams_df$Freq,decreasing=TRUE),]
rownames(two_grams_df) <- c()
write.csv(two_grams_df, file = "two_grams_df_50.csv")
rm("two_grams_df","two_grams")

# 3-grams
three_grams <- make.ngrams(my.text,ngram.size = 3)
three_grams_df <- data.frame(table(three_grams))
three_grams_df <- three_grams_df[order(three_grams_df$Freq,decreasing=TRUE),]
rownames(three_grams_df) <- c()
write.csv(three_grams_df, file = "three_grams_df_50.csv")

# 4-grams
four_grams <- make.ngrams(my.text,ngram.size = 4)
four_grams_df <- data.frame(table(four_grams))
four_grams_df <- four_grams_df[order(four_grams_df$Freq,decreasing=TRUE),]
rownames(four_grams_df) <- c()
write.csv(four_grams_df, file = "four_grams_df_50.csv")
rm("four_grams_df","four_grams")

# END SCRIPT
