# demo 2 
# for MKTG 5566
# 05 SEP 2021
#
# Jeffrey Orcutt

library(psych)

setwd("~/R")

dataset <- read.csv("dataset_online_store.csv")

#summarize and describe the dataset
summary(dataset)
table(dataset$isMultiBrandUser)
table(dataset$promotionType)

#display a table of proportions for categorical type data
frequency_repeat_buyer <- table(dataset$isRepeatBuyer)
proportion_repeat_buyer <- frequency_repeat_buyer/nrow(dataset)
proportion_repeat_buyer <- frequency_repeat_buyer/length(dataset$isRepeatBuyer)

#display a bar chart of the frequency/proportion table
barplot(frequency_repeat_buyer)
barplot(proportion_repeat_buyer, xlab = "Repeat Purchase Choice", ylab = "Proportion Value", col = c("brown", "green"), horiz=T)

#create indicator variables for categorical variables in the dataset
dataset$isRepeatBuyer_yes_i <- 0
dataset$isRepeatBuyer_yes_i[dataset$isRepeatBuyer == "yes"] <- 1

dataset$isMultiBrandUser_yes_i <- 0
dataset$isMultiBrandUser_yes_i[dataset$isMultiBrandUser == "yes"] <- 1

dataset$hasUserFriend_yes_i <-0 
dataset$hasUserFriend_yes_i[dataset$hasUserFriend == "yes"] <- 1

dataset$promotion_free_i<-0 
dataset$promotion_free_i[dataset$promotionType == "free"] <- 1

dataset$promotion_discount_i<-0 
dataset$promotion_discount_i[dataset$promotionType == "discount"] <- 1

#estimate a logit model of repeat purchase as a function of isMultiBrandUser
#log(p/(1-p)) = b0 + b1 * isMultiBrandUser_yes_i

logit_model_1 <- glm(isRepeatBuyer_yes_i ~ isMultiBrandUser_yes_i, family = binomial(link="logit"), data = dataset)
  summary(logit_model_1) #-1.07 
  
#marginal and multiplicative effects
print(marginal_effects_1 <-coef(logit_model_1))
print(multi_effects_1 <- exp(marginal_effects_1)) 
#compared to a single_brand user, .3406593 as likely to repeat buy
print(multi_effects_percent_1 <- (multi_effects_1-1)*100)
#produces a percentage (-65.93407% as likely to buy again)

#create other models
logit_model_2 <- glm(isRepeatBuyer_yes_i ~ isMultiBrandUser_yes_i+ hasUserFriend_yes_i + cookingRating + recipeRating, data=dataset, family=binomial(link ="logit"))
summary(logit_model_2)
#marginal and multiplicative effects
print(marginal_effects_2 <-coef(logit_model_2))
print(multi_effects_2 <- exp(marginal_effects_2)) 
print(multi_effects_percent_2 <- (multi_effects_2-1)*100)
#compared to a user without friends who isn't a user, there's a 69% increase in repeat buying...
cbind(marginal_effects_2, multi_effects_2, multi_effects_percent_2)
#this is called base odds when all variables are set to zero
#for every unit of increase in cooking rating, logit of the probability increases.678
#for every unit of increase in cooking rating, odds increase 1.97 times 
#for every unit of increase in cooking rating, there's a 97% increase in odds of repeat purchase

#making predictions to compute predictive accuracy
dataset$probability_predicted_2 <- predict(logit_model_2, dataset[ , c("isMultiBrandUser_yes_i",
                                                                       "hasUserFriend_yes_i", 
                                                                       "cookingRating", 
                                                                       "recipeRating")],
                                                              type = "response")
#create a variable to state whether predict yes (1) or no(0) if probability > 0.5
dataset$choice_predicted_2 <- 0
dataset$choice_predicted_2[dataset$probability_predicted_2 >0.5] <- 1

#create confusion matrix
confusion_matrix_2 <- table(dataset$isRepeatBuyer, dataset$choice_predicted_2)
confusion_matrix_2

accuracy_2 <- sum(diag(confusion_matrix_2))/sum(confusion_matrix_2)
accuracy_2

#create third logit model 
logit_model_3<- glm(isRepeatBuyer_yes_i ~ isMultiBrandUser_yes_i+ 
                      hasUserFriend_yes_i + cookingRating + recipeRating + 
                      promotion_discount_i + promotion_free_i, 
                    data=dataset, family=binomial(link ="logit"))
summary(logit_model_3)
#marginal and multiplicative effects
print(marginal_effects_3 <-coef(logit_model_3))
print(multi_effects_3 <- exp(marginal_effects_3)) 
print(multi_effects_percent_3 <- (multi_effects_3-1)*100)
#making predictions to compute predictive accuracy
dataset$probability_predicted_3 <- predict(logit_model_3, dataset[ , c("isMultiBrandUser_yes_i",
                                                                       "hasUserFriend_yes_i", 
                                                                       "cookingRating", 
                                                                       "recipeRating", 
                                                                       "promotion_discount_i",
                                                                       "promotion_free_i")],
                                           type = "response")

#create a variable to state whether predict yes (1) or no(0) if probability > 0.5
dataset$choice_predicted_3 <- 0
dataset$choice_predicted_3[dataset$probability_predicted_3 >0.5] <- 1

#create confusion matrix
confusion_matrix_3 <- table(dataset$isRepeatBuyer, dataset$choice_predicted_3)
confusion_matrix_3

accuracy_3 <- sum(diag(confusion_matrix_3))/sum(confusion_matrix_3)
accuracy_3

#making a prediction for a new customer (multibrand, no friend, received discount)
new_customer <- data.frame(isMultiBrandUser_yes_i = 1, hasUserFriend_yes_i = 0, 
                           cookingRating = mean(dataset$cookingRating),
                           recipeRating = mean(dataset$recipeRating),
                           promotion_discount_i= 1, promotion_free_i = 0)
new_customer
predict(logit_model_3, new_customer, type = "response")

dataset[1:10, c("isRepeatBuyer", "probability_predicted_2", "choice_predicted_2",
                "probability_predicted_3", "choice_predicted_3"
                )]