# Credit Cards Assignment
# Deep Learning Model (MLP) 
# With Weightage 
# With Z-scores Normalization 

setwd("D:/Credit card Assignment")

library(keras)
library(tensorflow)
library(mlbench)
library(pROC)
library(dplyr)

tensorflow::tf$random$set_seed(104)

# Load the data
train_data <- read.csv("train_afterPreprocess_client_ver5.csv",header=T, sep=",") 
test_data <- read.csv("test_afterPreprocess_client_ver5.csv",header=T, sep=",")

head(train_data$DEFAULT)

dim(train_data)         # 21000 * 24
dim(test_data)          # 9000 * 24

str(train_data)
str(test_data)


# change the data to matrix
train_data_mat_all <- as.matrix(train_data)
test_data_mat_all <- as.matrix(test_data)


# Data partition into input variables and target 
X_train_all = train_data_mat_all[,-24]
X_test_all = test_data_mat_all[,-24]

Y_train_all = train_data_mat_all[,24]
Y_test_all = test_data_mat_all[,24]

# Label - Hot Label Encoder
Y_train_label_all <- to_categorical(Y_train_all)
Y_test_label_all <- to_categorical(Y_test_all)



################ Model Training Deep Learning #####################################################

## Model Buiding

model <- keras_model_sequential()

model %>%
  layer_dense(units=23, activation = 'relu', input_shape = ncol(X_train_all)) %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=64, activation = 'relu')  %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=32, activation = 'relu')  %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=16, activation = 'relu')  %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  #layer_dense(units=16, activation = 'relu')  %>%
  #layer_dropout(rate = 0.2, set.seed(123)) %>%
  #layer_dense(units=8, activation = 'relu')  %>%
  #layer_dropout(rate = 0.2, set.seed(123)) %>%
  #layer_dense(units=4, activation = 'relu')  %>%
  #layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=2, activation = 'sigmoid')

#optimizer
adagrad= optimizer_adagrad(lr = 0.001)

# Compile the model 
model %>% 
  compile(loss='binary_crossentropy',
          optimizer = adagrad,
          metrics = 'accuracy')

summary(model)


# Fit the model 
model %>%
  fit(X_train_all,
      Y_train_label_all,
      epoch = 100, 
      batch_size= 10,
      validation_split = 0.2,
      #class_weight = list("0"=0.642, "1"=2.261))
      class_weight = list("0"=1, "1"=3.55))

model %>% 
  evaluate (X_test_all, Y_test_label_all)

# Model prediction
all_prob <- model %>%
  predict_proba(X_test_all)

prediction <- model%>%
  predict_classes(X_test_all)


## Plot roc 
library(pROC)
library(ROCR)

all_roc <- roc(as.vector(Y_test_label_all), as.vector(all_prob), positive = 0, type ="prob") 
all_roc
plot(all_roc, col="blue", legacy.axes = TRUE,xlab="1-Specificity",
     main="ROC Curve of MLP Model")


prediction_vector <- as.factor(prediction)
Y_test <- as.factor(Y_test_all)

cm <- confusionMatrix(data = prediction_vector, reference= Y_test, positive = '1')
cm




