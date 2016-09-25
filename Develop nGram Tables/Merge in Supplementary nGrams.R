# Load supplementary corpus from http://corpus.byu.edu/full-text/

# Load files
four_grams_download <- read.delim("C:/R Programming Course/Capstone Project/Project Files/Downloade Corpus/w4_.txt", header=FALSE, stringsAsFactors=FALSE)
three_grams_download <- read.delim("C:/R Programming Course/Capstone Project/Project Files/Downloade Corpus/w3_.txt", header=FALSE, stringsAsFactors=FALSE)
two_grams_download  <- read.delim("C:/R Programming Course/Capstone Project/Project Files/Downloade Corpus/w2_.txt", header=FALSE, stringsAsFactors=FALSE)

# Load derived files from Project Corpus
#two_grams_pred <- data.frame(read.csv("C:two_grams_pred_50_1.csv", stringsAsFactors=FALSE))
three_grams_pred <- data.frame(read.csv("C:three_grams_pred_50_1.csv", stringsAsFactors=FALSE))
four_grams_pred <- data.frame(read.csv("C:four_grams_pred_50_1.csv", stringsAsFactors=FALSE))


# Build supplementary tables for 4 grams from mix of the two corpus
three_gram <- data.frame(three_gram = paste(four_grams_download$V2,four_grams_download$V3,four_grams_download$V4,sep=" "))
full_gram <- data.frame(full_gram  = paste(four_grams_download$V2,four_grams_download$V3,four_grams_download$V4,four_grams_download$V5,sep=" "))
full_gram_count <- data.frame(full_gram_count = four_grams_download$V1)
prediction <- data.frame(prediction = four_grams_download$V5)
four_grams_pred_supp <- data.frame(cbind(three_gram,full_gram,full_gram_count,prediction))
rm("three_gram","full_gram","full_gram_count","prediction")


three_gram <- data.frame(three_gram = paste(three_grams_download$V2,three_grams_download$V3,three_grams_download$V4,sep=" "))
three_gram_count <- data.frame(three_gram_count = three_grams_download$V1)
three_grams_supp <- data.frame(cbind(three_gram,three_gram_count))
rm("three_gram","three_gram_count")

four_grams_pred_supp1 <- merge(three_grams_supp,four_grams_pred_supp,by="three_gram")
four_grams_pred_supp1$probability <- four_grams_pred_supp1$full_gram_count / four_grams_pred_supp1$three_gram_count
four_grams_pred_supp1$X <- 1:nrow(four_grams_pred_supp1)

four_grams_pred_supp <- four_grams_pred_supp1[c(7,1,3,4,5,2,6)]
rm("four_grams_pred_supp1","three_grams_supp")                                    
                
# Merge Corpus
four_grams_pred_supp1 <- rbind(four_grams_pred_supp,four_grams_pred)                                   
four_grams_pred_supp <- four_grams_pred_supp1[!duplicated(four_grams_pred_supp1$full_gram), ]                                    
lapply(four_grams_pred_supp, class)
four_grams_pred_supp$three_gram <- as.character(four_grams_pred_supp$three_gram)
four_grams_pred_supp$prediction <- as.character(four_grams_pred_supp$prediction)
four_grams_pred_supp$full_gram <- as.character(four_grams_pred_supp$full_gram)
write.csv(four_grams_pred_supp, file = "four_grams_pred_supp1.csv")
rm("four_grams_pred_supp","four_grams_pred_supp1","four_grams_download","four_grams_pred")

# Build supplementary tables for 3 grams from mix of the two corpus
two_gram <- data.frame(two_gram = paste(three_grams_download$V2,three_grams_download$V3,sep=" "))
full_gram <- data.frame(full_gram  = paste(three_grams_download$V2,three_grams_download$V3,three_grams_download$V4,sep=" "))
full_gram_count <- data.frame(full_gram_count = three_grams_download$V1)
prediction <- data.frame(prediction = three_grams_download$V4)
three_grams_pred_supp <- data.frame(cbind(two_gram,full_gram,full_gram_count,prediction))
rm("two_gram","full_gram","full_gram_count","prediction")


two_gram <- data.frame(two_gram = paste(two_grams_download$V2,two_grams_download$V3,sep=" "))
two_gram_count <- data.frame(two_gram_count = two_grams_download$V1)
two_grams_supp <- data.frame(cbind(two_gram,two_gram_count))
rm("two_gram","two_gram_count")

three_grams_pred_supp1 <- merge(two_grams_supp,three_grams_pred_supp,by="two_gram")
three_grams_pred_supp1$probability <- three_grams_pred_supp1$full_gram_count / three_grams_pred_supp1$two_gram_count
three_grams_pred_supp1$X <- 1:nrow(three_grams_pred_supp1)
three_grams_pred_supp1$X.1 <- 1:nrow(three_grams_pred_supp1)

three_grams_pred_supp <- three_grams_pred_supp1[c(8,7,1,3,4,5,2,6)]
rm("three_grams_pred_supp1","two_grams_supp") 

# Merge Corpus

three_grams_pred_supp1 <- rbind(three_grams_pred_supp,three_grams_pred)                                   
three_grams_pred_supp <- three_grams_pred_supp1[!duplicated(three_grams_pred_supp1$full_gram), ]                                    
lapply(three_grams_pred_supp, class)
three_grams_pred_supp$two_gram <- as.character(three_grams_pred_supp$two_gram)
three_grams_pred_supp$prediction <- as.character(three_grams_pred_supp$prediction)
three_grams_pred_supp$full_gram <- as.character(three_grams_pred_supp$full_gram)
write.csv(three_grams_pred_supp, file = "three_grams_pred_supp1.csv")
rm("three_grams_pred_supp","three_grams_pred_supp1","three_grams_download","three_grams_pred")


