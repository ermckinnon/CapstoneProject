
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tm)
library(stringi)
library(stringr)
library(dplyr)
library(plyr)

#Load Prediction Tables
load("finalpredictiontables.RData")
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
lambda <- 0.4


shinyServer(function(input, output) {
  
  output$text1 <- renderText({
    
    x <- stri_trim(input$text)
    predictions <- data.frame(Prediction_Probability(x,lambda))
    
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
    
    for (y in 1:10) {
      
      if (is.na(final_predictions[y])) {final_predictions[y]=one_grams_pred[y,2]}
      
    }
    out<- paste("1.",final_predictions[1]," 2.",final_predictions[2]," 3.",final_predictions[3]," 4.",final_predictions[4]," 5.",final_predictions[5],sep = " ")
    
    paste("", out)
    
  })
  
  output$text2 <- renderText({
    
    x <- stri_trim(input$text)
    predictions <- data.frame(Prediction_Probability(x,lambda))
    
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
    
    for (y in 1:10) {
      
      if (is.na(final_predictions[y])) {final_predictions[y]=one_grams_pred[y,2]}
      
    }
    out<- paste("6.",final_predictions[6]," 7.",final_predictions[7]," 8.",final_predictions[8]," 9.",final_predictions[9]," 10.",final_predictions[10],sep = " ")
    
    paste("",out)
    
  })

})
