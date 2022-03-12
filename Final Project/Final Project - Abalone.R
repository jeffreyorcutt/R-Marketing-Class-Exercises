# Final Project
# Authors: Mark Babcock and Jeff Orcutt
# Date: 27 November 2021

# Description: apply data analysis techniques learned in MKTG 5566 to a 
# dataset and gleam any insights 

# Dataset chosen: Abalone Data Set 
# https://archive.ics.uci.edu/ml/datasets/Abalone
# 
# Problem Statement: To what extent is it possible to reliably estimate the 
# number of rings, and thus the age, of the endangered species Abalone without 
# using destructive means?
# Our dataset contains the following variables
# Name		Data Type	Meas.	Description
# ----		---------	-----	-----------
#  Sex		nominal			M, F, and I (infant)
# Length		continuous	mm	Longest shell measurement
# Diameter	continuous	mm	perpendicular to length
# Height		continuous	mm	with meat in shell
# Whole weight	continuous	grams	whole abalone
# Shucked weight	continuous	grams	weight of meat
# Viscera weight	continuous	grams	gut weight (after bleeding)
# Shell weight	continuous	grams	after being dried
# Rings		integer			+1.5 gives the age in years
# The non-destructive variables are: Sex, Length, Diameter, Height, and Whole Weight
# The destructive variables are: Shucked Weigh, Viscera Weight, Shell Weight and Rings

# Rings are the scientific calculator of age = rings+1.5

library("naivebayes")
library("corrplot")
library("nFactors")
library("ggplot2")

setwd("~/Desktop/R Data and Projects/data")
data <-read.csv("abalone.data")

corrplot(cor(data[,-1]))
summary(prcomp(data[,-1]))

#create a Factor Analysis solution for the related variables, 
# which is all of them except sex
#

data_scaled_dim <- scale(data[,2:8])

#eigen values
eigen_values <-eigen(cor(data_scaled_dim))
eigen_values$values

#create a scree plot to examine amount of information from factors
plot(eigen_values$values, type = "o", col= "blue", main="Eigen Value Scree Chart", ylab = "Eigen Value")

fa_solution_dim <- factanal(data_scaled_dim, factors = 1, scores = "Bartlett")
fa_solution_dim$loadings
data$factor_dim <- fa_solution_dim$scores[,"Factor1"]

#created a column to group Abalone by reproductive age groups, also serves to
#provide target variables for classification models


#2 age group bins
data$Age_Group <- "infant"
data$Age_Group[data$Rings > 7] <- "adult"
hist(data$Rings, main="Distribution of Rings", xlab = "Count of Rings", ylab = "Frequency")
ggplot(data, aes(x=Age_Group)) + geom_bar()

#4 age group bins
data$Age_Group_4bin <- "infant"
data$Age_Group_4bin[data$Rings > 5] <- "juvenile"
data$Age_Group_4bin[data$Rings > 7] <- "adult"
data$Age_Group_4bin[data$Rings > 14] <- "senior"
ggplot(data, aes(x=Age_Group_4bin)) + geom_bar()
  
  #make two datasets training and test to establish reliability of results
smp_size <- floor(0.80 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


# created a Naive Bayes Classification Model based on Factor Analysis and Sex
# 87.08% accurate

nbc_model_wfactor <- naive_bayes(Age_Group ~ Sex + factor_dim, train)
test$nbc_predicted_wfactor <- predict(nbc_model_wfactor, test[,cbind("Sex","factor_dim")])
(confusion_matrix_nbc_wfactor <- table(test$Age_Group, test$nbc_predicted_wfactor))
(predictive_accuracy_nbc_wfactor<- 
    sum(diag(confusion_matrix_nbc_wfactor)/sum(confusion_matrix_nbc_wfactor)))

# created a Naive Bayes Classification Model based on Factor Analysis and Sex for 4 age group bins
# 76.43% accurate

nbc_model_wfactor_4bin <- naive_bayes(Age_Group_4bin ~ Sex + factor_dim, train)
test$nbc_predicted_wfactor_4bin <- predict(nbc_model_wfactor_4bin, test[,cbind("Sex","factor_dim")])

(confusion_matrix_nbc_wfactor_4bin <- 
    table(test$Age_Group_4bin, test$nbc_predicted_wfactor_4bin))

(predictive_accuracy_nbc_wfactor_4bin<- 
    sum(diag(confusion_matrix_nbc_wfactor_4bin)/sum(confusion_matrix_nbc_wfactor_4bin)))


# create a general linear regression Model based on factor analysis and sex for 2 age group bins 
# greater and less than 7 years
# 84.09 accurate
lm_model_wfactor<- lm(Rings ~ Sex + factor_dim, data = train )
test$lm_predicted_lm_wfactor <- predict(lm_model_wfactor, test[,cbind("Sex", "factor_dim")])
test$Age_Group_lm_wfactor[test$lm_predicted_lm_wfactor <= 7] <- "infant"
test$Age_Group_lm_wfactor[test$lm_predicted_lm_wfactor > 7] <- "adult"
(confusion_matrix_lm_wfactor <- table(test$Age_Group, test$Age_Group_lm_wfactor))
(predictive_accuracy_lm_wfactor<- 
    sum(diag(confusion_matrix_lm_wfactor)/sum(confusion_matrix_lm_wfactor)))

# create a general linear regression Model based on factor analysis and age for four bins
#
# 72.96% accurate
lm_model_fctr_4bin <- lm(Rings ~ Sex + factor_dim, data = train )
test$lm_predicted_lm_fctr_4bin <- predict(lm_model_fctr_4bin, test[,cbind("Sex","factor_dim")])
test$Age_Group_lm_fctr_4bin[test$lm_predicted_lm_fctr_4bin <= 5] <- "infant"
test$Age_Group_lm_fctr_4bin[test$lm_predicted_lm_fctr_4bin > 5] <- "juvenile"
test$Age_Group_lm_fctr_4bin[test$lm_predicted_lm_fctr_4bin > 7] <- "adult"
test$Age_Group_lm_fctr_4bin[test$lm_predicted_lm_fctr_4bin> 14] <- "senior"
(confusion_matrix_lm_fctr_4bin <- table(test$Age_Group_4bin, test$Age_Group_lm_fctr_4bin))
(predictive_accuracy_lm_fctr_4bin<- 
    sum(diag(confusion_matrix_lm_fctr_4bin)/sum(confusion_matrix_lm_fctr_4bin)))


