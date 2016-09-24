# This is the script for a base back-off algorithm with discount factor lamda

# Load Libraries
#setwd("C:/R Programming Course/Capstone Project/Project Files")
library(tm)
library(stringi)
library(stringr)
library(dplyr)
library(plyr)

# Load Prediction Tables
load("predictiontables.RData")  #this is an image file of the 4 prediction files prodiced by the previous code
one_grams_pred <- data.frame(read.csv("C:one_grams_pred_10.csv", stringsAsFactors=FALSE)) #A table of the top ten unigrams


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