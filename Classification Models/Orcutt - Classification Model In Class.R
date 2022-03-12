# Naive Bayes Classifiers Assisted Classwork
# 
# Author: Jeff Orcutt
# Date: 28 October 2021

library("naivebayes")

setwd("~/Desktop/R Data and Projects/data/")

# bring in dataset 
dataset <- read.csv("dataset_bank_term_deposit.csv")

set.seed(1000)

proportion <- 0.8
indexes_train <- sample(nrow(dataset), proportion * nrow(dataset))
data_train <- dataset[indexes_train,]
data_test <- dataset[-indexes_train,]

#create naive bayes classification model based on six factors
nbc_model_1 <- naive_bayes(y ~ age + duration + housing + marital + previous 
                           + job, data_train)

#making prediction using nbc_model_1
data_test$class_predicted_1 <- predict(nbc_model_1, data_test[, c("age", 
                        "duration", "housing", "marital", "previous", "job")])


nbc_model_1$prior
nbc_model_1$table$marital
#likelihood that someone is divorced provided they provided a "no" response .1133
#(PNo|x) = P(No)P(x|No) / P(x) 
nbc_model_1$table$age

#need to review this...again
#P(C2|X6) = P?(C2) * P(X6|C2)   /    P(x6)
#posterior posibility = apriori probability of c2 * likelihood of data / probability x

#compute predictive accuracy of nbc_model_1
(confusion_matrix_1 <- table(data_test$y, data_test$class_predicted_1))
(predictive_accuracy_1 <- sum(diag(confusion_matrix_1)/sum(confusion_matrix_1)))

#question 1: answer A
#question 2: answer a b c
#question 3: answer c


#create naive bayes classification model based on five factors
nbc_model_2 <- naive_bayes(y ~ age  + housing + marital + previous + job, data_train)

#making prediction using nbc_model_2
data_test$class_predicted_2 <- predict(nbc_model_2, data_test[, c("age", 
                                   "housing", "marital", "previous", "job")])

#compute predictive accuracy of nbc_model_2
(confusion_matrix_2 <- table(data_test$y, data_test$class_predicted_2))
(predictive_accuracy_2 <- sum(diag(confusion_matrix_2)/sum(confusion_matrix_2)))

nbc_model_2$prior


#create new datasets, 75% split
set.seed(1000)
proportion_2 <- 0.75
indexes_train_2 <- sample(nrow(dataset), proportion_2 * nrow(dataset))
data_train_2 <- dataset[indexes_train_2,]
data_test_2 <- dataset[-indexes_train_2,]

#create naive bayes classification model based on six factors
nbc_model_3 <- naive_bayes(y ~ age + duration + housing + marital + previous 
                           + job, data_train_2)

#making prediction using nbc_model_3
data_test_2$class_predicted_3 <- predict(nbc_model_3, data_test_2[, c("age", 
                        "duration", "housing", "marital", "previous", "job")])
#examine likelihood of all categorical predictors
(nbc_model_3$tables)

#compute predictive accuracy of nbc_model_3
(confusion_matrix_3 <- table(data_test_2$y, data_test_2$class_predicted_3))
(predictive_accuracy_3 <- sum(diag(confusion_matrix_3)/sum(confusion_matrix_3)))

#to find improvement in yes
(445/(722+445))
(0.381396-0.3799)/0.3799

#assuming age is normally distributed is not necessarily a good thing - it could be distributed otherwise 
# should be kernel density 

