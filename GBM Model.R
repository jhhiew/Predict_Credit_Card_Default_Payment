# Gradient Boosting Method (GBM)
# Credit Card Assignments 
# SVM using z-scores 

setwd("D:/Credit card Assignment")

library(readr)

# Load the data
train_data_gbm <- read.csv("train_afterPreprocess_client_ver5.csv",header=T, sep=",") 
test_data_gbm <- read.csv("test_afterPreprocess_client_ver5.csv",header=T, sep=",")

#     0    1 
# 16381 4619 
table(train_data_gbm$DEFAULT)
#    0    1 
# 6983 2017 
table(test_data_gbm$DEFAULT)

# Convert all the outcome to factors 
train_data_gbm$DEFAULT <- ifelse(train_data_gbm$DEFAULT == 1, "Yes", "No")
train_data_gbm$DEFAULT <- as.factor(train_data_gbm$DEFAULT)

test_data_gbm$DEFAULT <- ifelse(test_data_gbm$DEFAULT == 1, "Yes", "No")
test_data_gbm$DEFAULT <- as.factor(test_data_gbm$DEFAULT)


##################      GBM       #################################################
###################################################################################
library(caret)

set.seed(333)
ctrl <- trainControl (method = "cv",
                      number = 10,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)

weightage <- ifelse(train_data_gbm$DEFAULT == "No",1,3.55)
table(weightage)

set.seed(333)

#Train GBM
model_gbm <- train(form = DEFAULT~.,
                   data = train_data_gbm,
                   method = "gbm",
                   metric = "ROC",
                   trControl = ctrl,
                   weights= weightage,
                   tuneLength = 10)


model_gbm

# save the model to disk
# saveRDS(model_gbm,"model_gbm.rds")

# load the model
model_gbm <- readRDS("model_gbm.rds")


result.predicted <- predict(model_svm, newdata = test_data_gbm , type="prob" ) # Prediction
result.predicted 

# Default threshold -> 0.5
cm_all_svm <- confusionMatrix (predict(model_gbm, newdata = test_data_gbm), test_data_gbm$DEFAULT,positive = 'Yes')
cm_all_svm

library(pROC)
library(ROCR)

result.roc_gbm <- roc(test_data_gbm$DEFAULT, result.predicted$Yes, type="prob")
# plot(result.roc_gbm, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_gbm
plot(result.roc_gbm, col="red", legacy.axes = TRUE,xlab="1-Specificity",
     main="ROC Curve of GBM Model")

