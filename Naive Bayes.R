# Load Libraries
library(readxl)
library(e1071)
library(writexl)
library(caret)
library(caTools)
library(ggplot2)
library(gridExtra)
library(reshape2)






# Read excel file and remove blank rows and columns
df <- read_xlsx("Complete_Final.xlsx")
df <- df[, -5]
df <- df[rowSums(is.na(df[ , 7])) == 0, ]

# write_xlsx(df,"Complete_Final.xlsx")                        


# Change class types
df$`Overall grade by the peers` <- as.factor(df$`Overall grade by the peers`)
df$`Final grade` <- as.factor(df$`Final grade`)
df$`Assigment grade` <- as.factor(df$`Assigment grade`)
df$`Grade Level` <- as.factor(df$`Grade Level`)

df$`Detailed label` <- as.factor(df$`Detailed label`)
df$`Lie factor` <- as.factor(df$`Lie factor`)
df$`Data/color ink ratio` <- as.factor(df$`Data/color ink ratio`)
df$`Chart junk` <- as.factor(df$`Chart junk`)

df$`Count # of words` <- as.integer(df$`Count # of words`)
df$Nouns <- as.integer(df$Nouns)
df$Verb <- as.integer(df$Verb)
df$adv <- as.integer(df$adv)
df$Adj <- integer(df$Adj)


# 10 bin
df$CommentDensity <- cut(df$`Count # of words`, breaks = c(0, 10, 20, 30, 40, 50, 60 , 70, 80, 90, 100), include.lowest = T)
# 5 Bin
df$CommentDensity <- cut(df$`Count # of words`, breaks = c(0, 20, 40, 60, 90, 100), include.lowest = T)
# 4 Bin
df$CommentDensity <- cut(df$`Count # of words`, breaks = c(0, 25, 50, 75, 100), include.lowest = T)
# 2 Bin
df$CommentDensity <- cut(df$`Count # of words`, breaks = c(0, 50,100), include.lowest = T)



fill_down_and_repeat <- function(df) {
  # create a copy of the input dataframe
  df_filled <- df
  
  # loop over each row in the dataframe
  for (i in 1:nrow(df_filled)) {
    # loop over each column in the dataframe
    for (j in 1:ncol(df_filled)) {
      # check if the current cell is missing data and the previous row and column have data
      if (is.na(df_filled[i, j]) && !is.na(df_filled[i-1, j]) && !is.na(df_filled[i, j-1])) {
        # fill the current cell with the data from the previous row with data in the current column
        df_filled[i, j] <- df_filled[i-1, j]
      }
    }
  }
  
  # return the filled dataframe
  return(df_filled)
}

df <- fill_down_and_repeat(df)



# Set Seed
set.seed(22)

# Create train and test df
x = sort(sample(nrow(df), nrow(df) * .7))

df_train <- df[x,]
df_test <- df[-x,]



# Grade Level Classifier
model_GL <- naiveBayes(`Grade Level`~., data = df_train )
pred_GL <- predict(model_GL, df_test)


cm_GL <- table(df_test$`Grade Level`, pred_GL)
confusionMatrix(cm_GL)
# 93.5%





# Overall Grade Classifier
model_OG <- naiveBayes(`Overall grade by the peers`~., df_train)
pred_OG <- predict(model_OG, df_test)


cm_OG <- table(df_test$`Overall grade by the peers`, pred_OG)
confusionMatrix(cm_OG)
# 75.1%



# Assignment Grade Classifier
model_AG <- naiveBayes(`Assigment grade`~., data = df_train)
pred_AG <- predict(model_AG, df_test)

cm_AG <- table(df_test$`Assigment grade`, pred_AG)
confusionMatrix(cm_AG)
# 79.7%



# Final Grade Classifier
model_FG <- naiveBayes(`Final grade`~., data = df_train)
pred_FG <- predict(model_FG, df_test)

cm_FG <- table(df_test$`Final grade`, pred_FG)
confusionMatrix(cm_FG)
# 81.7%




# Lie Factor Classifier
model_Lie <- naiveBayes(`Lie factor`~., data = df_train)
pred_Lie <- predict(model_Lie, df_test)


cm_Lie <- table(df_test$`Lie factor`, pred_Lie)
confusionMatrix(cm_Lie)
# 93.8%




# Detail Label Classifier
model_DL <- naiveBayes(`Detailed label`~., data = df_train)
pred_DL <- predict(model_DL, df_test)

cm_DL <- table(df_test$`Detailed label`, pred_DL)
confusionMatrix(cm_DL)
# 93.9%










# Final grade classifier with other grades
model_2 <- naiveBayes(`Final grade`~ `Assigment grade` +
                        `Peer Comment` + `Grade Level`, data = df_train)
pred_2 <- predict(model_2, df_test)

cm_2 <- table(df_test$`Final grade`, pred_2)
confusionMatrix(cm_2, postive = "A+")
# 90%

cm_2 <- tableGrob(cm_2)

q<- ggplot(subset(df_test, !is.na(`Final grade`)), aes(x = `Final grade`, fill = `Final grade`)) + geom_bar()

grid.arrange(q, cm_2, ncol = 2)


plot(model_2, df_test$`Final grade`)






# Overall Grade minus other 2 grades
df_trainX <- df_train[,-2:-3]
df_testX <- df_test[, -2:-3]

model_X <- naiveBayes(`Overall grade by the peers`~., data = df_testX)
pred_X <- predict(model_X, df_testX)

cm_X <- table(df_testX$`Overall grade by the peers`, pred_X)
confusionMatrix(cm_X)









# Overall Grade Classifier with specific supports
model_1 <- naiveBayes(`Overall grade by the peers`~ CommentDensity + `Peer Comment` + 
                        `Assigment grade` + `Chart junk` +, data = df_train)
pred_1 <- predict(model_1, df_test)

cm_1 <- table(df_test$`Overall grade by the peers`, pred_1)
confusionMatrix(cm_1)



# Focus on Word Count 4 bin

model_4bin <- naiveBayes(CommentDensity ~., data = df_train)
pred_4bin <- predict(model_4bin, df_test)

cm_4bin<- table(df_test$CommentDensity, pred_4bin)
confusionMatrix(cm_4bin)
# 96.5%



# Word count 10 bins (0, 10, 20, 30, 40, 50, 60 , 70, 80, 90, 100)
model_10bin <- naiveBayes(CommentDensity~., data = df_train)
pred_10bin <- (predict(model_10bin, df_test))

cm_10bin <- table(df_test$CommentDensity, pred_10bin)
confusionMatrix(cm_10bin)
# 91.1%


# 5 bins
model_5bin <- naiveBayes(CommentDensity~., data = df_train)
pred_5bin <- (predict(model_5bin, df_test))

cm_5bin <- table(df_test$CommentDensity, pred_5bin)
confusionMatrix(cm_5bin)
# 96.5%


cm_5bin
mcnemar.test(table(df_test$CommentDensity, pred_5bin))



# 2 bins
model_2bin <- naiveBayes(CommentDensity~., data = df_train)
pred_2bin <- (predict(model_2bin, df_test))

cm_2bin <- table(df_test$CommentDensity, pred_2bin)
confusionMatrix(cm_2bin)
# 96.5%


cm_2bin
mcnemar.test(table(df_test$CommentDensity, pred_2bin))




# y <- as.data.frame(c(x[,1], x[,2]))
# Freq <- x[,3]
# Freq <- c(Freq, Freq)
# Freq
# y$Freq <- Freq
# 
# act <- c("Actual")
# act <- rep(act, each = 16)
# pre <- "Predicted"
# pre <- rep(pre, each = 16)
# 
# Type <- c(act, pre)
# y$Type <- Type
# colnames(y)[1] <- "CommentDensity"
# 
# ggplot(y, aes(x = CommentDensity, fill = Type, y = Freq)) + geom_col(position = "dodge", stat = "identity")













# https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/
# https://towardsdatascience.com/understanding-confusion-matrix-a9ad42dcfd62?gi=267787abc771
# https://www.programmingr.com/statistics/how-to-generate-a-confusion-matrix-in-r/



# class_prediction <-  ifelse(probability_prediction > 0.50, "positive_class", "negative_class"  )





# Create a confusion matrix
cm <- confusionMatrix(predictions, actuals)

# Calculate the accuracy
accuracy <- cm$overall['Accuracy']

# Calculate the precision
precision <- cm$byClass['Positive']['Precision']

# Calculate the recall
recall <- cm$byClass['Positive']['Recall']

# Calculate the F1 score
f1 <- 2 * (precision * recall) / (precision + recall)

# Print the results
cat('Accuracy:', accuracy, '\n')
cat('Precision:', precision, '\n')
cat('Recall:', recall, '\n')
cat('F1 score:', f1, '\n')

