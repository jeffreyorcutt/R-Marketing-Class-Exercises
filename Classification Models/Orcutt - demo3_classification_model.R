# Demonstration of Naive Bayes Classifiers
#
# author: Jeff Orcutt

# bring in libraries
library("caTools")
library("naivebayes")
# set working directory
setwd("~/R/")

# bring in dataset
dataset <- read.csv("dataset_investor_type.csv")

#split data
train <- dataset[1:900,]
test <- dataset[901:1000,]

#a better way to split data
#split dataset into two datasets: train and test
#set.seed(123)
#sample = sample.split(dataset[,1], SplitRatio = .9)
#train <- subset(dataset, sample == T)
#test <- subset(dataset, sample == F)

#build a NBC model using education group and age as indicators
nbc_model_1 <- naive_bayes(investor_type ~ educ_group + age, train)

#examine a-priori probabilities and likelihoods
nbc_model_1$prior
nbc_model_1$tables$educ_group
table(train$educ_group, train$investor_type)

#likelihood for a continuous variable
#best fitting normal distribution by class given
nbc_model_1$tables$age

#histograms of distributions of age
hist(train$age[train$investor_type == "active_advisory"], 50, xlab = "age", main="Age Distribution within Active Advisory Class")
hist(train$age[train$investor_type == "active_self"], 50, xlab = "age", main="Age Distribution within Active Self Class")
hist(train$age[train$investor_type == "passive"], 50, xlab = "age", main="Age Distribution within Passive Class")

#NBC model 1 as an equation
#p(C_k|educ_group, age) = P(C_k) * (P(educ_group|c_k) * P(age|c_k)) / p(educ_group, age)

#p(C_1|educ_group, age) = P(C_1) * (P(educ_group|c_1) * P(age|c_1)) / p(educ_group, age)
#p(C_2|educ_group, age) = P(C_2) * (P(educ_group|c_2) * P(age|c_2)) / p(educ_group, age)
#p(C_3|educ_group, age) = P(C_3) * (P(educ_group|c_3) * P(age|c_3)) / p(educ_group, age)

#new customer age 45, educ group3 probability of active_advisory
#0.18 * .0822 * dnorm(45, mean = 39.8417, 6.419)
#(apriori) * (educ_group) * dnorm((age), mean in prob, sd in prob)

#predictions for NBC model 1 in validation sample
test$class_predicted_1 <- predict(nbc_model_1, test[, c("educ_group", "age")])

#get posterior probabilities from nbc_model_1
post_probabilities <- predict(nbc_model_1, test[, c("educ_group", "age")], type="prob")

#if you wanted the post_probs of each column to own variable
test$C1_post_prob_1 <- post_probabilities[,1]
test$C2_post_prob_1 <- post_probabilities[,2]
test$C3_post_prob_1 <- post_probabilities[,3]



#build an nbc model using all available information as predictors
nbc_model_2 <- naive_bayes(investor_type ~ income_group + educ_group + age +
                             homeowner + num_children + multiBrandUser, train)
#predictions for NBC model 2 in validation sample
test$class_predicted_2 <- predict(nbc_model_2, test[, c("income_group", 
                              "educ_group", "age", "homeowner", "num_children", 
                              "multiBrandUser")])

#get posterior probabilities from nbc_model_2
post_probabilities_2 <- predict(nbc_model_2, test[, c("income_group", 
                              "educ_group", "age", "homeowner", "num_children", 
                              "multiBrandUser")], type="prob")
#if you wanted the post_probs of each column to own variable
test$C1_post_prob_2 <- post_probabilities_2[,1]
test$C2_post_prob_2 <- post_probabilities_2[,2]
test$C3_post_prob_2 <- post_probabilities_2[,3]


#compute predictive accuracy of nbc_model_2
(confusion_matrix_2 <- table(test$investor_type, test$class_predicted_2))
(predictive_accuracy_2 <- sum(diag(confusion_matrix_2)/sum(confusion_matrix_2)))




#build an nbc model using all available information as predictors, with smoothing
nbc_model_3 <- naive_bayes(investor_type ~ income_group + educ_group + age +
                             homeowner + num_children + multiBrandUser, 
                           laplace = 1, train)
#predictions for NBC model 3 in validation sample
test$class_predicted_3 <- predict(nbc_model_3, test[, c("income_group", 
                                                        "educ_group", "age", "homeowner", "num_children", 
                                                        "multiBrandUser")])

#get posterior probabilities from nbc_model_3
post_probabilities_3 <- predict(nbc_model_3, test[, c("income_group", 
                                                      "educ_group", "age", "homeowner", "num_children", 
                                                      "multiBrandUser")], type="prob")
#if you wanted the post_probs of each column to own variable
test$C1_post_prob_3 <- post_probabilities_3[,1]
test$C2_post_prob_3 <- post_probabilities_3[,2]
test$C3_post_prob_3 <- post_probabilities_3[,3]




#build an nbc model using all available information as predictors except age, with smoothing
nbc_model_4 <- naive_bayes(investor_type ~ income_group + educ_group +
                             homeowner + num_children + multiBrandUser, 
                           laplace = 1, train)
#predictions for NBC model 4 in validation sample
test$class_predicted_4 <- predict(nbc_model_4, test[, c("income_group", 
                                                        "educ_group", "homeowner", "num_children", 
                                                        "multiBrandUser")])

#get posterior probabilities from nbc_model_4
post_probabilities_4 <- predict(nbc_model_4, test[, c("income_group", 
                                                      "educ_group", "homeowner", "num_children", 
                                                      "multiBrandUser")], type="prob")
#if you wanted the post_probs of each column to own variable
test$C1_post_prob_4 <- post_probabilities_4[,1]
test$C2_post_prob_4 <- post_probabilities_4[,2]
test$C3_post_prob_4 <- post_probabilities_4[,3]
#compute predictive accuracy of nbc_model_3
(confusion_matrix_4 <- table(test$investor_type, test$class_predicted_4))
(predictive_accuracy_4 <- sum(diag(confusion_matrix_4)/sum(confusion_matrix_4)))





#build an nbc model using all available information as predictors except multiBrandUser, with smoothing
nbc_model_5 <- naive_bayes(investor_type ~ income_group + educ_group + age+ 
                             homeowner + num_children , 
                           laplace = 1, train)
#predictions for NBC model 5 in validation sample
test$class_predicted_5 <- predict(nbc_model_5, test[, c("income_group", 
                                                        "educ_group", "homeowner", "num_children", 
                                                       "age")])

#get posterior probabilities from nbc_model_5
post_probabilities_5 <- predict(nbc_model_5, test[, c("income_group", 
                                                      "educ_group", "homeowner", "num_children", 
                                                      "age")], type="prob")
#if you wanted the post_probs of each column to own variable
test$C1_post_prob_5 <- post_probabilities_5[,1]
test$C2_post_prob_5 <- post_probabilities_5[,2]
test$C3_post_prob_5 <- post_probabilities_5[,3]
#compute predictive accuracy of nbc_model_5
(confusion_matrix_5 <- table(test$investor_type, test$class_predicted_5))
(predictive_accuracy_5 <- sum(diag(confusion_matrix_5)/sum(confusion_matrix_5)))

