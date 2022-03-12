# Discrete Choice Models Assignment
#
# Jeffrey Orcutt
# October 09, 2021

library(psych)

setwd("~/R/")

#bring in dataset
dataset <- read.csv("dataset_payment_choice.csv")

#summarize the dataset
summary(dataset)

#setup indicator variables
dataset$genre_sim_i <- 0 
dataset$genre_sim_i[dataset$genre == "simulation"] <- 1
dataset$genre_act_i <-0
dataset$genre_act_i[dataset$genre == "action"] <- 1

dataset$appeal_charity_i <- 0
dataset$appeal_charity_i[dataset$appeal_type == "charity"] <- 1 

dataset$previous_pay_yes_i <-0
dataset$previous_pay_yes_i[dataset$previous_payer == "yes"] <- 1

dataset$previous_pay_no_i <- 0
dataset$previous_pay_no_i[dataset$previous_payer == "no"] <-1

dataset$payment_choice_i <- 0
dataset$payment_choice_i[dataset$payment_choice == "yes"] <-1


#answer question 1
table(dataset$payment_choice)
print(sum(dataset$payment_choice == "no")/ nrow(dataset))
#34.2% of dataset did not make a payment

#answer question 2
summary(dataset)
table(dataset$previous_payer, useNA = "ifany")

#answer question 3
#create generalized logit model of voluntary payment as a function of
#   dev_rating, num_stream, game genres 
logit_model_1 <- glm(payment_choice_i ~ dev_rating + num_streams + genre_sim_i + 
                       genre_act_i, family = binomial(link="logit"), data=dataset)
summary(logit_model_1)
print(exp(-.00269))
# a is false, the multiplicative effect comes out to .997
# b is false because the measure of significance is not enough to make conclusions from
# c is true because the logit is positive and signifcantly different from zero

#answer question 4
summary(logit_model_1)

#answer question 5
print(exp(1.75488))

#answer question 6
#create a confusion matrix and compute how accurate the logit_model_1 is
dataset$predict_1 <- 0
dataset$predict_1 <- predict(logit_model_1, dataset[ , c("dev_rating", 
                                                         "genre_sim_i",
                                                         "genre_act_i",
                                                         "num_streams")],
                             type = "response")
dataset$choice_predicted_1 <- 0
dataset$choice_predicted_1[dataset$predict_1 > 0.5 ] <- 1
(confusion_matrix_1 <- table(dataset$payment_choice, dataset$choice_predicted_1))

accuracy_1 <- sum(diag(confusion_matrix_1))/sum(confusion_matrix_1)

accuracy_1

#answer question 7, 8
logit_model_2 <- glm(payment_choice_i ~ dev_rating + num_streams + 
                       previous_pay_yes_i + previous_pay_no_i + genre_sim_i + 
                       genre_act_i + appeal_charity_i, 
                     family = binomial(link="logit"), data=dataset)
summary(logit_model_2)

#answer question 9, 10 
print(marginal_effects_2 <-coef(logit_model_2))
print(multi_effects_2 <- exp(marginal_effects_2)) 
print(multi_effects_percent_2 <- (multi_effects_2-1)*100)
#the multi effective of streams is -.31 percent not 31 percent, so answer was none of the above
# in the multi_effects_2, the values reported are the multiplicative effects 2.689

#answer question 11
dataset$predict_2 <- 0
dataset$predict_2 <- predict(logit_model_2, dataset[ , c("dev_rating", 
                                                         "genre_sim_i",
                                                         "genre_act_i",
                                                         "previous_pay_yes_i",
                                                         "previous_pay_no_i",
                                                         "appeal_charity_i",
                                                         "num_streams")],
                             type = "response")
dataset$choice_predicted_2 <- 0
dataset$choice_predicted_2[dataset$predict_2 > 0.5 ] <- 1
(confusion_matrix_2 <- table(dataset$payment_choice, dataset$choice_predicted_2))

#looking for incorrect which is FP or FN in the matrix (FN no,1 or FP yes,0)
#71 FP

#answer question 12
accuracy_2 <- sum(diag(confusion_matrix_2))/sum(confusion_matrix_2)

accuracy_2
#78% accurate
#better predicts payers than non payers (100/171 vs 290/329)

#answer question 13
new_customer <- data.frame(dev_rating = 2.5, genre_sim_i = 0, genre_act_i = 1,
                           num_streams = 1000, previous_pay_yes_i = 0, 
                           previous_pay_no_i = 0, appeal_charity_i = 0)
predict(logit_model_2, new_customer, type="response")
#prediction is yes, 60.2% likely

new_customer_with_charity <- data.frame(dev_rating = mean(dataset$dev_rating), 
                             genre_sim_i = 0, genre_act_i = 1,
                             num_streams = mean(dataset$num_streams),
                             previous_pay_yes_i = 0, 
                             previous_pay_no_i = 0, appeal_charity_i = 1)
new_customer_without_charity <- data.frame(dev_rating = mean(dataset$dev_rating), 
                                        genre_sim_i = 0, genre_act_i = 1,
                                        num_streams = mean(dataset$num_streams),
                                        previous_pay_yes_i = 0, 
                                        previous_pay_no_i = 0, appeal_charity_i = 0)

predict(logit_model_2, new_customer_with_charity, type="response")
predict(logit_model_2, new_customer_without_charity, type="response")

