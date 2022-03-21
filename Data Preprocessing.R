# DATA PREPROCESSING 

setwd("D:/Credit card Assignment")


##################   Load the dataset   ###################################################
library(caret) # -> for createDataPartition
library(doParallel)

client_data <- readRDS('client_data_afterPreprocess_ver4.csv')


##################  Label Encoding For Categorical Varaibles  #############################
# Data conversion
# Function of encode-ordinal 
encode_ordinal <- function(x) {
  
  x <- as.numeric(as.factor(x))
  Y <- as.integer(x)
  Y
}

client_data$LIMIT_BAL<- encode_ordinal(client_data$LIMIT_BAL)
client_data$SEX<-encode_ordinal(client_data$SEX)
client_data$EDUCATION<-encode_ordinal(client_data$EDUCATION)
client_data$MARRIAGE<-encode_ordinal(client_data$MARRIAGE)
client_data$AGE<-encode_ordinal(client_data$AGE)
client_data$PAY_0<-encode_ordinal(client_data$PAY_0)
client_data$PAY_2<-encode_ordinal(client_data$PAY_2)
client_data$PAY_3<-encode_ordinal(client_data$PAY_3)
client_data$PAY_4<-encode_ordinal(client_data$PAY_4)
client_data$PAY_5<-encode_ordinal(client_data$PAY_5)
client_data$PAY_6<-encode_ordinal(client_data$PAY_6)

client_data$DEFAULT <- as.numeric(client_data$DEFAULT) -1


#####################   Data Partitioning      ################################################

# Separate into training data and testing data

set.seed(999)
train <- createDataPartition(client_data$DEFAULT, p=0.70,list=FALSE)
train_data <- client_data[train, ]
test_data <- client_data[-train, ]

dim(train_data)    # 21000 * 24
dim(test_data)     # 9000  * 24

# Imbalaced Datasets for training (0:1 = 1:3.52)
#     0     1 
# 16381  4619
table(train_data$DEFAULT)

# Check for testing datasets 
table(test_data$DEFAULT)       # 0 -> 6983,  1 -> 2017

#parallel processing
registerDoParallel()

# Export the train and test data 
library(readr)
# write_rds(train_data,"train_client_data_ver2.csv")
# write_rds(test_data,"test_client_data_ver2.csv")

summary(client_data)
summary(train_data)


####################  Load Training and Testing Dataset   #############################################
# Load training data
train_data <- read_rds("train_client_data_ver2.csv") 

str(train_data)
summary(train_data)
dim(train_data)         


# Check for output balancing
#      0     1(yes) 
#  16381  4619 
table(train_data$DEFAULT)

# Load testing data
test_data <- read_rds("test_client_data_ver2.csv") 
str(test_data)

#      0    1 
#   6983 2017 
table(test_data$DEFAULT) 


#########################   Z-scores Normalization     #############################

con_var <- c("BILL_AMT1", "BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6",
             "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")

zscores_Norm <- preProcess(train_data[,con_var], method=c("center", "scale"))

train_data[,con_var] <- predict(zscores_Norm, train_data[,con_var])
test_data[,con_var] <- predict(zscores_Norm, test_data[,con_var])
dim(train_data)

str(train_data)
str(test_data)


#write_rds(train_data,"train_afterPreprocess_client_ver5i.csv")
#write_rds(test_data,"test_afterPreprocess_client_ver5i.csv")

#write.csv(train_data,"train_afterPreprocess_client_ver5.csv",row.names = F)
#write.csv(test_data,"test_afterPreprocess_client_ver5.csv",row.names = F)