# Test Models Final Model
# Load Libraries
# setwd("C:/R Programming Course/Capstone Project/Project Files")
library(tm)
library(stringi)
library(stringr)
library(dplyr)
library(plyr)

# Load Prediction Tables
load("predictiontables.RData") # All prediction tables saved as workspace file
one_grams_pred <- data.frame(read.csv("C:one_grams_pred_10.csv", stringsAsFactors=FALSE))

# Create smaller test set from clean test data - 4-gram tables
create_testset <- function (size){

test_set <- data.frame(read.csv("C:four_grams_df_50_test.csv", stringsAsFactors=FALSE))
set.seed(42567)
test_set_sample <- sample_n(test_set,size)
test_set_sample <- as.data.frame(test_set_sample)
colnames(test_set_sample) <- c("x","test_text","Freq")
test_set_sample$test_text <- as.character(test_set_sample$test_text)
rm(test_set)

# Split test table into 3gram and the correct result
transform <- do.call(rbind,strsplit(test_set_sample$test_text,split = " "))
transform <- cbind(apply(transform[,1:3],1,function(x) paste (x,collapse = " ")),
                   transform[,4])
test_set_sample <- data.frame(test_text = as.character(test_set_sample$test_text),
                              three_grams = as.character(transform[,1]),                           
                              answer = as.character(transform[,2]))
return(test_set_sample) 
}

# Base model function
Prediction_Probability <- function(x, lambda) {
  returns <- 50
  user_input <- as.character(x)
  df_user_input <- data.frame(user_input = user_input)
  test_text <- DataframeSource(df_user_input)
  
  user_text <- Corpus(test_text)
  user_text  <-tm_map(user_text ,removePunctuation)
  user_text  <-tm_map(user_text ,removeNumbers)
  user_text  <-tm_map(user_text ,tolower)
  user_text  <-tm_map(user_text ,stripWhitespace)
  removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x) # remove all non-aphanumerics
  user_text <- tm_map(user_text, removeSpecialChars)
  user_text<- tm_map(user_text, PlainTextDocument)
  df_user_text <- data.frame(user_text=unlist(sapply(user_text,`[`,"content")),stringsAsFactors=F)
  rownames(df_user_text) <- c()
  
  clean_text <- as.character(df_user_text$user_text)
  word_num <- stri_count(clean_text,regex="\\S+")
  user_unigram <- as.character(word(df_user_text$user_text,-1))
  user_bigram <- as.character(paste(word(df_user_text$user_text,-2),word(df_user_text$user_text,-1)))
  user_trigram <- as.character(paste(word(df_user_text$user_text,-3),word(df_user_text$user_text,-2),word(df_user_text$user_text,-1)))
  rm("user_text","test_text")
  
  if(word_num>=3) {
    # Four Gram model
    four_grams_pred_temp <- four_grams_pred[grep(paste0("^",user_trigram,"$"),as.character(four_grams_pred$three_gram)),]
    four_grams_pred_temp <- four_grams_pred_temp[order(four_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(four_grams_pred_temp),returns)
    prediction_four <- data.frame(four_grams_pred_temp$prediction,four_grams_pred_temp$probability)[1:rows,]
    prediction_four$model <- "FourGram"
    prediction_four$backoff <- prediction_four$four_grams_pred_temp.probability
    colnames(prediction_four) <- c("prediction","probability","model","back_off_prob")
    rownames(prediction_four) <- c()
    
    # Three Gram model
    three_grams_pred_temp <- three_grams_pred[grep(paste0("^",user_bigram,"$"),as.character(three_grams_pred$two_gram)),]
    three_grams_pred_temp <- three_grams_pred_temp[order(three_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(three_grams_pred_temp),returns)
    prediction_three <- data.frame(three_grams_pred_temp$prediction,three_grams_pred_temp$probability)[1:rows,]
    prediction_three$model <- "ThreeGram"
    prediction_three$backoff <- lambda * prediction_three$three_grams_pred_temp.probability
    colnames(prediction_three) <- c("prediction","probability","model","back_off_prob")
    rownames(prediction_three) <- c()
    
    # Two Gram model
    two_grams_pred_temp <- two_grams_pred[grep(paste0("^",user_unigram,"$"),as.character(two_grams_pred$one_gram)),]
    two_grams_pred_temp <- two_grams_pred_temp[order(two_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_two <- data.frame(two_grams_pred_temp$prediction,two_grams_pred_temp$probability)[1:rows,]
    prediction_two$model <- "TwoGram"
    prediction_two$backoff <- lambda * lambda * prediction_two$two_grams_pred_temp.probability
    colnames(prediction_two) <- c("prediction","probability","model","back_off_prob")
    rownames(prediction_two) <- c()
    
    #Full Prediction Table
    Total_Prediction <- data.frame(rbind(prediction_four,prediction_three,prediction_two))
    #     merge on unigram probability
    #     one_gram_temp <- one_grams_pred[,c(2,4)]
    #     colnames(one_gram_temp) <- c("prediction","prediction_probability")
    #     Total_Prediction <- merge(Total_Prediction,one_gram_temp,by="prediction")
    Output <- Total_Prediction[order(Total_Prediction$back_off_prob,decreasing=TRUE),]
    Output <- Output[!duplicated(Output$prediction), ]
    rownames(Output) <- c()
    
  } else if(word_num==2) {
    
    
    # Three Gram model
    three_grams_pred_temp <- three_grams_pred[grep(paste0("^",user_bigram,"$"),as.character(three_grams_pred$two_gram)),]
    three_grams_pred_temp <- three_grams_pred_temp[order(three_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(three_grams_pred_temp),returns)
    prediction_three <- data.frame(three_grams_pred_temp$prediction,three_grams_pred_temp$probability)[1:rows,]
    prediction_three$model <- "ThreeGram"
    prediction_three$backoff <- lambda * prediction_three$three_grams_pred_temp.probability
    colnames(prediction_three) <- c("prediction","probability","model","back_off_prob")
    rownames(prediction_three) <- c()
    
    # Two Gram model
    two_grams_pred_temp <- two_grams_pred[grep(paste0("^",user_unigram,"$"),as.character(two_grams_pred$one_gram)),]
    two_grams_pred_temp <- two_grams_pred_temp[order(two_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_two <- data.frame(two_grams_pred_temp$prediction,two_grams_pred_temp$probability)[1:rows,]
    prediction_two$model <- "TwoGram"
    prediction_two$backoff <- lambda * lambda * prediction_two$two_grams_pred_temp.probability
    colnames(prediction_two) <- c("prediction","probability","model","back_off_prob")
    rownames(prediction_two) <- c()
    
    #Full Prediction Table
    Total_Prediction <- data.frame(rbind(prediction_three,prediction_two))
    Output <- Total_Prediction[order(Total_Prediction$back_off_prob,decreasing=TRUE),]
    Output <- Output[!duplicated(Output$prediction), ]
    rownames(Output) <- c()
    
  } else {
    
    # Two Gram model
    two_grams_pred_temp <- two_grams_pred[grep(paste0("^",user_unigram,"$"),as.character(two_grams_pred$one_gram)),]
    two_grams_pred_temp <- two_grams_pred_temp[order(two_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_two <- data.frame(two_grams_pred_temp$prediction,two_grams_pred_temp$probability)[1:rows,]
    prediction_two$model <- "TwoGram"
    prediction_two$backoff <- lambda * lambda * prediction_two$two_grams_pred_temp.probability
    colnames(prediction_two) <- c("prediction","probability","model","back_off_prob")
    rownames(prediction_two) <- c()
    
    #Full Prediction Table
    Total_Prediction <- data.frame(prediction_two)
    Output <- Total_Prediction[order(Total_Prediction$back_off_prob,decreasing=TRUE),]
    Output <- Output[!duplicated(Output$prediction), ]
    rownames(Output) <- c()
    
    
  }
  
  return(Output)
  
  
}

# Mercer Smoothed Probability Function
Prediction_Mercer <- function(x,lam1,lam2,lam3,lam4) {
  
  returns <- 100
  user_input <- as.character(x)
  df_user_input <- data.frame(user_input = user_input)
  test_text <- DataframeSource(df_user_input)
  
  user_text <- Corpus(test_text)
  user_text  <-tm_map(user_text ,removePunctuation)
  user_text  <-tm_map(user_text ,removeNumbers)
  user_text  <-tm_map(user_text ,tolower)
  user_text  <-tm_map(user_text ,stripWhitespace)
  removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x) # remove all non-aphanumerics
  user_text <- tm_map(user_text, removeSpecialChars)
  user_text<- tm_map(user_text, PlainTextDocument)
  df_user_text <- data.frame(user_text=unlist(sapply(user_text,`[`,"content")),stringsAsFactors=F)
  rownames(df_user_text) <- c()
  
  clean_text <- as.character(df_user_text$user_text)
  word_num <- stri_count(clean_text,regex="\\S+")
  user_unigram <- as.character(word(df_user_text$user_text,-1))
  user_bigram <- as.character(paste(word(df_user_text$user_text,-2),word(df_user_text$user_text,-1)))
  user_trigram <- as.character(paste(word(df_user_text$user_text,-3),word(df_user_text$user_text,-2),word(df_user_text$user_text,-1)))
  rm("user_text","test_text")
  
  if(word_num>=3) {
    # Four Gram model
    four_grams_pred_temp <- four_grams_pred[grep(paste0("^",user_trigram,"$"),as.character(four_grams_pred$three_gram)),]
    four_grams_pred_temp <- four_grams_pred_temp[order(four_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(four_grams_pred_temp),returns)
    prediction_four <- data.frame(four_grams_pred_temp$prediction,four_grams_pred_temp$probability)[1:rows,]
    prediction_four$model <- "FourGram"
    prediction_four$backoff <- lam1 *prediction_four$four_grams_pred_temp.probability
    colnames(prediction_four) <- c("prediction","probability4","model4","back_off_prob4")
    rownames(prediction_four) <- c()
    
    # Three Gram model
    three_grams_pred_temp <- three_grams_pred[grep(paste0("^",user_bigram,"$"),as.character(three_grams_pred$two_gram)),]
    three_grams_pred_temp <- three_grams_pred_temp[order(three_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(three_grams_pred_temp),returns)
    prediction_three <- data.frame(three_grams_pred_temp$prediction,three_grams_pred_temp$probability)[1:rows,]
    prediction_three$model <- "ThreeGram"
    prediction_three$backoff <- lam2 * prediction_three$three_grams_pred_temp.probability
    colnames(prediction_three) <- c("prediction","probability3","model3","back_off_prob3")
    rownames(prediction_three) <- c()
    
    # Two Gram model
    two_grams_pred_temp <- two_grams_pred[grep(paste0("^",user_unigram,"$"),as.character(two_grams_pred$one_gram)),]
    two_grams_pred_temp <- two_grams_pred_temp[order(two_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_two <- data.frame(two_grams_pred_temp$prediction,two_grams_pred_temp$probability)[1:rows,]
    prediction_two$model <- "TwoGram"
    prediction_two$backoff <- lam3 * prediction_two$two_grams_pred_temp.probability
    colnames(prediction_two) <- c("prediction","probability2","model2","back_off_prob2")
    rownames(prediction_two) <- c()
    
    # One Gram model
    one_grams_pred_temp <- one_grams_pred
    one_grams_pred_temp <- one_grams_pred_temp[order(one_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_one <- data.frame(one_grams_pred_temp$full_gram,one_grams_pred_temp$probability)[1:rows,]
    prediction_one$model <- "OneGram"
    prediction_one$backoff <- lam4 * prediction_one$one_grams_pred_temp.probability
    colnames(prediction_one) <- c("prediction","probability1","model1","back_off_prob1")
    rownames(prediction_one) <- c()   
    
    #Full Prediction Table
    Total_prediction <- merge(prediction_one,prediction_two,by="prediction",all.x=TRUE)
    Total_prediction <- merge(Total_prediction,prediction_three,by="prediction",all.x=TRUE)
    Total_prediction <- merge(Total_prediction,prediction_four,by="prediction",all.x=TRUE)    
    Total_prediction[is.na(Total_prediction)] <- 0  
    Total_prediction$total.probability <- Total_prediction$back_off_prob1 + Total_prediction$back_off_prob2 + Total_prediction$back_off_prob3 + Total_prediction$back_off_prob4 
    Total_prediction$model <- paste(Total_prediction$model1,Total_prediction$model2,Total_prediction$model3,Total_prediction$model4)
    Output <- Total_prediction[order(Total_prediction$total.probability,decreasing=TRUE),]
    Output <- data.frame(prediction=Output$prediction, model =Output$model,back_off_prob=Output$total.probability)
    
  } else if(word_num==2) {
    
    # Three Gram model
    three_grams_pred_temp <- three_grams_pred[grep(paste0("^",user_bigram,"$"),as.character(three_grams_pred$two_gram)),]
    three_grams_pred_temp <- three_grams_pred_temp[order(three_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(three_grams_pred_temp),returns)
    prediction_three <- data.frame(three_grams_pred_temp$prediction,three_grams_pred_temp$probability)[1:rows,]
    prediction_three$model <- "ThreeGram"
    prediction_three$backoff <- 2 * lam2 * prediction_three$three_grams_pred_temp.probability
    colnames(prediction_three) <- c("prediction","probability3","model3","back_off_prob3")
    rownames(prediction_three) <- c()
    
    # Two Gram model
    two_grams_pred_temp <- two_grams_pred[grep(paste0("^",user_unigram,"$"),as.character(two_grams_pred$one_gram)),]
    two_grams_pred_temp <- two_grams_pred_temp[order(two_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_two <- data.frame(two_grams_pred_temp$prediction,two_grams_pred_temp$probability)[1:rows,]
    prediction_two$model <- "TwoGram"
    prediction_two$backoff <- 2 * lam3 * prediction_two$two_grams_pred_temp.probability
    colnames(prediction_two) <- c("prediction","probability2","model2","back_off_prob2")
    rownames(prediction_two) <- c()
    
    # One Gram model
    one_grams_pred_temp <- one_grams_pred
    one_grams_pred_temp <- one_grams_pred_temp[order(one_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_one <- data.frame(one_grams_pred_temp$full_gram,one_grams_pred_temp$probability)[1:rows,]
    prediction_one$model <- "OneGram"
    prediction_one$backoff <- 2 * lam4 * prediction_one$one_grams_pred_temp.probability
    colnames(prediction_one) <- c("prediction","probability1","model1","back_off_prob1")
    rownames(prediction_one) <- c()   
    
    #Full Prediction Table
    Total_prediction <- merge(prediction_one,prediction_two,by="prediction",all.x=TRUE)
    Total_prediction <- merge(Total_prediction,prediction_three,by="prediction",all.x=TRUE)
    Total_prediction[is.na(Total_prediction)] <- 0  
    Total_prediction$total.probability <- Total_prediction$back_off_prob1 + Total_prediction$back_off_prob2 + Total_prediction$back_off_prob3 
    Total_prediction$model <- paste(Total_prediction$model1,Total_prediction$model2,Total_prediction$model3)
    Output <- Total_prediction[order(Total_prediction$total.probability,decreasing=TRUE),]
    Output <- data.frame(prediction=Output$prediction, model =Output$model,back_off_prob=Output$total.probability)    
    
    
  } else {
    
    # Two Gram model
    two_grams_pred_temp <- two_grams_pred[grep(paste0("^",user_unigram,"$"),as.character(two_grams_pred$one_gram)),]
    two_grams_pred_temp <- two_grams_pred_temp[order(two_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_two <- data.frame(two_grams_pred_temp$prediction,two_grams_pred_temp$probability)[1:rows,]
    prediction_two$model <- "TwoGram"
    prediction_two$backoff <- 4 * lam3 * prediction_two$two_grams_pred_temp.probability
    colnames(prediction_two) <- c("prediction","probability2","model2","back_off_prob2")
    rownames(prediction_two) <- c()
    
    # One Gram model
    one_grams_pred_temp <- one_grams_pred
    one_grams_pred_temp <- one_grams_pred_temp[order(one_grams_pred_temp$probability,decreasing=TRUE),]
    rows = min(nrow(two_grams_pred_temp),returns)
    prediction_one <- data.frame(one_grams_pred_temp$full_gram,one_grams_pred_temp$probability)[1:rows,]
    prediction_one$model <- "OneGram"
    prediction_one$backoff <- 4 * lam4 * prediction_one$one_grams_pred_temp.probability
    colnames(prediction_one) <- c("prediction","probability1","model1","back_off_prob1")
    rownames(prediction_one) <- c()   
    
    #Full Prediction Table
    Total_prediction <- merge(prediction_one,prediction_two,by="prediction",all.x=TRUE)
    Total_prediction[is.na(Total_prediction)] <- 0  
    Total_prediction$total.probability <- Total_prediction$back_off_prob1 + Total_prediction$back_off_prob2  
    Total_prediction$model <- paste(Total_prediction$model1,Total_prediction$model2)
    Output <- Total_prediction[order(Total_prediction$total.probability,decreasing=TRUE),]
    Output <- data.frame(prediction=Output$prediction, model =Output$model,back_off_prob=Output$total.probability)    
    
    
  }
  
  return(Output)
  
  
}

# Test model function
test_model <- function (sample_size){

# Create Empty test results dataframe
results <- data.frame()
lambda <- 0.4
for (i in 1:sample_size) {
  
  # Populate columns with test vector
  results[i,1] <- as.character(test_set_sample[i,1])
  results[i,2] <- as.character(test_set_sample[i,2])
  results[i,3] <- as.character(test_set_sample[i,3])
  
  # Select model to test below and comment out the other
  predictions <- data.frame(Prediction_Probability(results[i,2],lambda)) 
  #predictions <- data.frame(Prediction_Mercer(results[i,2],0.5,0.3,0.15,0.05))
  
  # Post- process predcitions to pick top 5 plus top 5 non-stopwords
  predictions$prediction <- as.character(predictions$prediction)
  predictions_name <- data.frame(prediction = predictions[,1])
  predictions_names <- DataframeSource(predictions_name)
  predictions_names <- Corpus(predictions_names)
  minus_stopwords <- tm_map(predictions_names, removeWords, stopwords("english"))
  minus_stopword <- tm_map(minus_stopwords, PlainTextDocument)
  minus_stopword <- data.frame(text=unlist(sapply(minus_stopword,`[`,"content")),stringsAsFactors=F)
  minus_stopword <- data.frame(text = minus_stopword[minus_stopword$text !="",])
  minus_stopword$text <- as.character(minus_stopword$text)
  common <- data.frame(text = intersect(predictions$prediction[1:5],minus_stopword$text[1:20]))
  common$text <- as.character(common$text)
  if (nrow(common)==0) {
    
    minus_stopword1 <- data.frame(text = minus_stopword$text )
    
  } else {
    
    minus_stopword1 <- data.frame(text = minus_stopword[minus_stopword$text != common$text,])
    
  }
  minus_stopword1$text <- as.character(minus_stopword1$text)
  final_predictions <- c(predictions$prediction[1:5],minus_stopword1$text[1:5])
  
  # Use unigrams if predictions are missing
  for (y in 1:10) {
  
  if (is.na(final_predictions[y])) {final_predictions[y]=one_grams_pred[y,2]}
  
  }
  
  # Compare Prediction with correct answer and 
  results[i,4] <- paste(final_predictions[1],final_predictions[2],final_predictions[3],final_predictions[4],final_predictions[5],final_predictions[6],
                        final_predictions[7],final_predictions[8],final_predictions[9],final_predictions[10],sep = " ")
  results[i,5] <- sum(grepl(paste("^",results[i,3],"$", sep=""),final_predictions))
  accuracy <- sum(results[,5])/i
  print(accuracy)
  print(i)
}
colnames(results) <- c("test_text","three_grams","answer","predictions","Success")
return(results)
}

# Run a test
test_set_sample_total <- create_testset(10000)

test_results <- data.frame()
# Dummy data to start with
test_results[1,1] <- 1
test_results[1,2] <-.50
test_results[1,3] <-100
test_results[1,4] <-1000
colnames(test_results) <- c("run","accuracy","time","file-size")

# Code to reduce starting data size to delete ngrams with frequency less than 20
two_grams_pred <- two_grams_pred[two_grams_pred$full_gram_count >20,]
three_grams_pred <- three_grams_pred[three_grams_pred$full_gram_count >20,]
four_grams_pred <- four_grams_pred[four_grams_pred$full_gram_count >20,]

# Run 100 draws of 200 random sentences and calculate accuracy
x <- 1
for (run in 0:99) {
print(paste("run ",run))  
lower <- (run * 100) + 1
upper <- (run + 1) * 100

test_set_sample <- test_set_sample_total[lower:upper,]

ptm <- proc.time()
results <- test_model(100) 
time <- proc.time() - ptm 
test_results[x,1] <- x
test_results[x,2] <- sum(results$Success) / nrow(results)
test_results[x,3] <- time[3]
test_results[x,4] <- (object.size(one_grams_pred) + object.size(two_grams_pred) + object.size(three_grams_pred) + object.size(four_grams_pred)) / 1000000
x <- x + 1
}
# Write accuracy results to file
write.csv(test_results, file = "base_model_100_runs_level20_new1grams.csv")
