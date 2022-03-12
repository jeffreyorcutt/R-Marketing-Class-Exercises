# Naive Bayes Classifiers Assignment
# 
# Author: Jeff Orcutt
# Date: 3 November 2021

library("naivebayes")

setwd("~/R")

# bring in dataset 
dataset <- read.csv("dataset_bank_term_deposit.csv")

set.seed(1000)

proportion <- 0.75
indexes_train <- sample(nrow(dataset), proportion * nrow(dataset))
data_train <- dataset[indexes_train,]
data_test <- dataset[-indexes_train,]

#create naive bayes classification model based on five factors
nbc_model_1 <- naive_bayes(y ~ age + duration + housing + marital + previous 
                           , data_train)

#making prediction using nbc_model_2
data_test$class_predicted_1 <- predict(nbc_model_1, data_test[, c("age", 
                             "duration", "housing", "marital", "previous" )])


nbc_model_1$prior
nbc_model_1$tables

(confusion_matrix_1 <- table(data_test$y, data_test$class_predicted_1))
(predictive_accuracy_1 <- sum(diag(confusion_matrix_1)/sum(confusion_matrix_1)))



#create naive bayes classification model based on seven factors
nbc_model_2 <- naive_bayes(y ~  age+ duration + housing + marital + previous 
                           + education + job, data_train)



#making prediction using nbc_model_3
data_test$class_predicted_2 <- predict(nbc_model_2, data_test[, c( "age",
                               "duration", "housing", "marital", "previous",
                               "education", "job")])


nbc_model_2$prior
nbc_model_2$tables

(confusion_matrix_2 <- table(data_test$y, data_test$class_predicted_2))
(predictive_accuracy_2 <- sum(diag(confusion_matrix_2)/sum(confusion_matrix_2)))

#create naive bayes classification model based on six factors
nbc_model_3 <- naive_bayes(y ~ duration + housing + marital + previous 
                           + education + job, data_train)

#making prediction using nbc_model_3
data_test$class_predicted_3 <- predict(nbc_model_3, data_test[, c(
                               "duration", "housing", "marital", "previous",
                               "education", "job")])


nbc_model_3$prior
nbc_model_3$tables

(confusion_matrix_3 <- table(data_test$y, data_test$class_predicted_3))
(predictive_accuracy_3 <- sum(diag(confusion_matrix_3)/sum(confusion_matrix_3)))


#needed these to answer question 12 
(confusion_matrix_2)
(confusion_matrix_3)

(predictive_accuracy_2)
(predictive_accuracy_3)




#answering question 13
((443/(443+724))-(405/(405+762)))



#question 14 asks us to up the training sample size, which should
#increase (even slightly) fidelty of the prediction of the test dataset
set.seed(1000)

proportion <- 0.9
indexes_train <- sample(nrow(dataset), proportion * nrow(dataset))
data_train_2 <- dataset[indexes_train,]
data_test_2 <- dataset[-indexes_train,]

#create naive bayes classification model based on seven factors
nbc_model_4 <- naive_bayes(y ~  age+ duration + housing + marital + previous 
                           + education + job, data_train_2)



#making prediction using nbc_model_4
data_test_2$class_predicted_4 <- predict(nbc_model_4, data_test_2[, c( "age",
                                 "duration", "housing", "marital", "previous",
                                 "education", "job")])

#nbc_model_4$prior
#nbc_model_4$tables

(confusion_matrix_4 <- table(data_test_2$y, data_test_2$class_predicted_4))
(predictive_accuracy_4 <- sum(diag(confusion_matrix_4)/sum(confusion_matrix_4)))

