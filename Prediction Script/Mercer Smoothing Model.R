# This is the script for a Mercer back-off algorithm with discount factors lamda1, 2, 3 and 4

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


# Mercer Smoothing Function
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