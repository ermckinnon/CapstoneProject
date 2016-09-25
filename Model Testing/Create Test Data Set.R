# Creat Test Set

library(tm)
library(beepr)
library(stylo)

# Load corpus data
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

# Split into training and test sets
# Use same seed as used to produce the training data files
df_twitter <- data.frame(text = text_twitter)
df_news <- data.frame(text = text_news)
df_blogs <- data.frame(text = text_blogs)
df_total <- rbind(df_twitter,df_news,df_blogs)
set.seed(42567)
sample <- data.frame(num = sample(1:10,nrow(df_total),replace=T))
df_test <- data.frame(text = df_total[sample$num>7,])
df_training <- data.frame(text = df_total[sample$num<=7,])

# Clean up the test data in same way as training data
# Sample 50% of test data
perc_sample <- 50
set.seed(42555)
sample1 <- data.frame(num = sample(1:100,nrow(df_test),replace=T))
df_test_sample <- data.frame(text = df_test[sample1$num<=perc_sample,])

text_test <- DataframeSource(df_test_sample)
docs_test <- Corpus(text_test)
docs_test <-tm_map(docs_test,removePunctuation)
docs_test <-tm_map(docs_test,removeNumbers)
docs_test <-tm_map(docs_test,tolower)
docs_test <-tm_map(docs_test,stripWhitespace)
docs_test <-tm_map(docs_test,removeWords,profane_words)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x) # remove all non-aphanumerics
docs_test <- tm_map(docs_test, removeSpecialChars)

docs_test <- tm_map(docs_test, PlainTextDocument)
df_docs_test <- data.frame(text=unlist(sapply(docs_test,`[`,"content")),stringsAsFactors=F)

# Save clean test data for later use


write.csv(df_docs_test, file = "Clean_Test_50.csv")

# Ngram development for test data

ptm <- proc.time()
df_docs_test1 <- read.csv("Clean_Test_50.csv", stringsAsFactors=FALSE)
df_docs_test <- data.frame(df_docs_test1$text)
colnames(df_docs_test) <- c("text")
rm("df_docs_test1")


my.text <- txt.to.words(df_docs_test$text)
one_grams <- make.ngrams(my.text,ngram.size = 1)
one_grams_df <- data.frame(table(one_grams))
one_grams_df <- one_grams_df[order(one_grams_df$Freq,decreasing=TRUE),]
rownames(one_grams_df) <- c()
write.csv(one_grams_df, file = "one_grams_df_50_test.csv")
rm("one_grams_df","one_grams")


two_grams <- make.ngrams(my.text,ngram.size = 2)
two_grams_df <- data.frame(table(two_grams))
two_grams_df <- two_grams_df[order(two_grams_df$Freq,decreasing=TRUE),]
rownames(two_grams_df) <- c()
write.csv(two_grams_df, file = "two_grams_df_50_test.csv")
rm("two_grams_df","two_grams")


three_grams <- make.ngrams(my.text,ngram.size = 3)
three_grams_df <- data.frame(table(three_grams))
three_grams_df <- three_grams_df[order(three_grams_df$Freq,decreasing=TRUE),]
rownames(three_grams_df) <- c()
write.csv(three_grams_df, file = "three_grams_df_50_test.csv")
rm("three_grams_df","three_grams")


four_grams <- make.ngrams(my.text,ngram.size = 4)
four_grams_df <- data.frame(table(four_grams))
four_grams_df <- four_grams_df[order(four_grams_df$Freq,decreasing=TRUE),]
rownames(four_grams_df) <- c()
write.csv(four_grams_df, file = "four_grams_df_50_test.csv")
rm("four_grams_df","four_grams")

