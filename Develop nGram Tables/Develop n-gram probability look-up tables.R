# This R script takes in the 4 raw frequency tables produced by preceding 'Load and create frequency tables' script
# New tables are produced which store conditional probabilities in them for
# The 4-new tables are saved as prediction files for the algorithms

# This scipt is memory intensive and 24Gbytes of RAM is recommended
memory.limit(size = 24000)

#setwd("C:/R Programming Course/Capstone Project/Project Files")

# Load previous n-gram tables for 1 and 2-grams
one_grams <- data.frame(read.csv("C:one_grams_df_50.csv", stringsAsFactors=FALSE))
two_grams <- data.frame(read.csv("C:two_grams_df_50.csv", stringsAsFactors=FALSE))


# Build Prediction Tables

# One Gram predictions table
total_count <- sum(one_grams$Freq)
one_grams_pred <- data.frame(full_gram = as.character(one_grams$one_grams),
                             count = one_grams$Freq,
                             probability = one_grams$Freq / total_count)
write.csv(one_grams_pred, file = "one_grams_pred_50.csv")

# bigram predictions table
transform <- do.call(rbind,strsplit(two_grams$two_grams,split = " "))
two_grams_pred <- data.frame(full_gram = as.character(two_grams$two_grams),
                             count = as.numeric(two_grams$Freq),
                             one_grams = as.character(transform[,1]),                           
                             prediction = as.character(transform[,2]))
two_grams_pred <- merge(two_grams_pred,one_grams,by="one_grams")
probability <- data.frame(probability = (two_grams_pred$count / two_grams_pred$Freq))
two_grams_pred <- cbind(two_grams_pred,probability)

two_grams_pred <- two_grams_pred[,c(1:4,6:7)]
two_grams_pred$one_grams <- as.character(two_grams_pred$one_grams)
two_grams_pred$full_gram <- as.character(two_grams_pred$full_gram)
two_grams_pred$prediction <- as.character(two_grams_pred$prediction)
two_grams_pred$count<- as.numeric(two_grams_pred$count)
two_grams_pred$Freq <- as.numeric(two_grams_pred$Freq)
two_grams_pred$probability <- as.numeric(two_grams_pred$probability )

colnames(two_grams_pred) <- c("one_gram","full_gram","full_gram_count","prediction","one_gram_count","probability")
two_grams_pred <- two_grams_pred[order(two_grams_pred$probability,decreasing=TRUE),]
rownames(two_grams_pred) <- c()

write.csv(two_grams_pred, file = "two_grams_pred_50.csv")
rm("one_grams","one_grams_pred","two_grams_pred","probability","transform")


# trigram predictions table
three_grams <- data.frame(read.csv("C:three_grams_df_50.csv", stringsAsFactors=FALSE))
ptm <- proc.time()
transform <- do.call(rbind,strsplit(three_grams$three_grams,split = " "))
transform <- cbind(apply(transform[,1:2],1,function(x) paste (x,collapse = " ")),
                   transform[,3])
three_grams_pred <- data.frame(full_gram = as.character(three_grams$three_grams),
                             count = as.numeric(three_grams$Freq),
                             two_grams = as.character(transform[,1]),                           
                             prediction = as.character(transform[,2]))
rm("transform")
three_grams_pred <- merge(three_grams_pred,two_grams,by="two_grams")
probability <- data.frame(probability = (three_grams_pred$count / three_grams_pred$Freq))
three_grams_pred <- cbind(three_grams_pred,probability)
three_grams_pred <- three_grams_pred[,c(1:4,6:7)]

three_grams_pred$two_grams <- as.character(three_grams_pred$two_grams)
three_grams_pred$full_gram <- as.character(three_grams_pred$full_gram)
three_grams_pred$prediction <- as.character(three_grams_pred$prediction)
three_grams_pred$count<- as.numeric(three_grams_pred$count)
three_grams_pred$Freq <- as.numeric(three_grams_pred$Freq)
three_grams_pred$probability <- as.numeric(three_grams_pred$probability )
colnames(three_grams_pred) <- c("two_gram","full_gram","full_gram_count","prediction","two_gram_count","probability")

three_grams_pred <- three_grams_pred[order(three_grams_pred$probability,decreasing=TRUE),]
rownames(three_grams_pred) <- c()

write.csv(three_grams_pred, file = "three_grams_pred_50.csv")
rm("two_grams","three_grams_pred","probability","transform")


#quadgram predictions table
ptm <- proc.time()
three_grams <- data.frame(read.csv("C:three_grams_df_50.csv", stringsAsFactors=FALSE))
four_grams <- data.frame(read.csv("C:four_grams_df_50.csv", stringsAsFactors=FALSE))
four_grams <- four_grams[four_grams$Freq >1,]
ptm <- proc.time()
transform <- do.call(rbind,strsplit(four_grams$four_grams,split = " "))
transform <- cbind(apply(transform[,1:3],1,function(x) paste (x,collapse = " ")),
                   transform[,4])
four_grams_pred <- data.frame(full_gram = as.character(four_grams$four_grams),
                               count = as.numeric(four_grams$Freq),
                               three_grams = as.character(transform[,1]),                           
                               prediction = as.character(transform[,2]))
rm("transform")
four_grams_pred <- merge(four_grams_pred,three_grams,by="three_grams")
probability <- data.frame(probability = (four_grams_pred$count / four_grams_pred$Freq))
four_grams_pred <- cbind(four_grams_pred,probability)
four_grams_pred <- four_grams_pred[,c(1:4,6:7)]

four_grams_pred$three_grams <- as.character(four_grams_pred$three_grams)
four_grams_pred$full_gram <- as.character(four_grams_pred$full_gram)
four_grams_pred$prediction <- as.character(four_grams_pred$prediction)
four_grams_pred$count<- as.numeric(four_grams_pred$count)
four_grams_pred$Freq <- as.numeric(four_grams_pred$Freq)
four_grams_pred$probability <- as.numeric(four_grams_pred$probability )
colnames(four_grams_pred) <- c("three_gram","full_gram","full_gram_count","prediction","three_gram_count","probability")

four_grams_pred <- four_grams_pred[order(four_grams_pred$probability,decreasing=TRUE),]
rownames(four_grams_pred) <- c()

write.csv(four_grams_pred, file = "four_grams_pred_50.csv")

# End Script
