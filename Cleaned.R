# Load Libraries
library(readxl)
library(e1071)
library(writexl)
library(caret)
library(caTools)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(corrplot)



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


# 2 Bin
df$CommentDensity <- cut(df$`Count # of words`, breaks = c(0, 50,100), include.lowest = T)



# Fill in the blanks in the dataframe
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




# 2 bins naive bayes test
model_2bin <- naiveBayes(CommentDensity~., data = df_train)
pred_2bin <- (predict(model_2bin, df_test))

# frequency Table
cm_2bin <- table(df_test$CommentDensity, pred_2bin)
barplot(cm_2bin)

# Probility Table
probo_2bin <- (cm_2bin/ sum(cm_2bin))
probo_2bin

confusionMatrix(cm_2bin)
# 96.9%


# Naive Bayes with contigency table from the df_train and test datasets
con_table_Train <- data.table::data.table(df_train)
con_table_Test <- data.table::data.table(df_test)

con_B <- naiveBayes(CommentDensity~., data = con_table_Train)
con_pred <- predict(con_B, con_table_Test)

cm_con <- table(con_table_Test$CommentDensity, con_pred)
confusionMatrix(cm_con)


# Correlation Matrix (Can only is numerical data)
cor <- cor(df[, 11:15])
cor
corrplot(cor, method = "color", tl.srt = 45)
corrplot(cor, method = "number", tl.srt = 45)


# Mcnemars Test
cm_2bin
mcnemar.test(table(df_test$CommentDensity, pred_2bin))


