# Market Segmentation 
# 
# author: Jeff Orcutt
# 
# date: 17 November 2021

#include necessary libraries
library(cluster)


#set working directory and bring in dataset

setwd("~/R")
data_raw <- read.csv("dataset_cardio_equipment_preferences.csv")

summary(data_raw)
data_raw$type <- as.factor(data_raw$type)
data_raw$mode <- as.factor(data_raw$mode)

dataset <- data_raw[data_raw$mode =="electric", -5]
#View(dataset)

#-------------hierachial clustering (agglomerative) method ----#

#step1: determining cluster variables
data_hclust <- dataset[,-5]

#step2: determine dissimiliarity (distance) between all customs
distances <- daisy(data_hclust)
#to compute number of values
#number of obs * number of obs -1   / 2

#cast distances into a matrix, not necessary
distance_matrix <- as.matrix(distances)
distance_matrix[1:5, 1:5]

#step 3: create cluster solutions using the selected method
seg_hclust <- hclust(distances, method = "complete")

#plot the dendogram (complete hierachy of agglomerations)
#plot(seg_hclust)
plot(seg_hclust, labels= F)

#create a 3 customer solution
#creates an array with the cluster number for each element 
seg_hclust_3 <- cutree(seg_hclust, k=3)

#assign the cluster numbers to the original dataset
dataset$hclust_3 <- seg_hclust_3


#create a 4 customer solution
#creates an array with the cluster number for each element 
seg_hclust_4 <- cutree(seg_hclust, k=4)

#assign the cluster numbers to the original dataset
dataset$hclust_4 <- seg_hclust_4

#create a 5 customer solution
#creates an array with the cluster number for each element 
seg_hclust_5 <- cutree(seg_hclust, k=5)

#assign the cluster numbers to the original dataset
dataset$hclust_5 <- seg_hclust_5

#show a graphical representation of the cluster solutions
plot(seg_hclust, labels=F)
rect.hclust(seg_hclust, k=3, border="blue")

#show a graphical representation of the cluster solutions
plot(seg_hclust, labels=F)
rect.hclust(seg_hclust, k=4, border="purple")

#show a graphical representation of the cluster solutions
plot(seg_hclust, labels=F)
rect.hclust(seg_hclust, k=5, border="red")

#oooor you can overlay all the solutions on each other
plot(seg_hclust, labels=F)
rect.hclust(seg_hclust, k=3, border="blue")
rect.hclust(seg_hclust, k=4, border="purple")
rect.hclust(seg_hclust, k=5, border="red")

#steps 4&5 : select from feasible solutions and 
# describe segmentation plan
# business constraint wants 10% or more customers in each segment
table(dataset$hclust_3) # feasible
table(dataset$hclust_4) # feasible
table(dataset$hclust_5) # not-feasible, but close

#summarize individual segments in a 4 cluster solution
summary(dataset[dataset$hclust_4 == 1,1:5])
summary(dataset[dataset$hclust_4 == 2,1:5])
summary(dataset[dataset$hclust_4 == 3,1:5])
summary(dataset[dataset$hclust_4 == 4,1:5])

#when looking at data, compactness is basically same
#warranty services in segment 4 do not place a lot of importance 
#interactive equipment is less important to 1 and 2 relative to 3 and 4
#only segment 1 prefers recumbent
#only segment 3 prefers upright
#segement 2 and 4 prefer dual action
#dual action interactive, less warranty focused 

#K-means clustering

data_kmeans <- data_hclust

#convert categorical into numerical
data_kmeans$type_recumbent_i <- ifelse(data_kmeans$type == "recumbent", 1, 0)
data_kmeans$type_upright_i <- ifelse(data_kmeans$type == "upright", 1, 0)
data_kmeans$type <- NULL

#verify that all values are numeric
#str(data_kmeans)

#create candidate cluster solutions 
#k = (3, 4, 5)

set.seed(1000)
seg_kmeans_3 <- kmeans(data_kmeans, centers = 3)
dataset$kmeans_3 <- seg_kmeans_3$cluster

set.seed(1000)
seg_kmeans_4 <- kmeans(data_kmeans, centers = 4)
dataset$kmeans_4 <- seg_kmeans_4$cluster

set.seed(1000)
seg_kmeans_5 <- kmeans(data_kmeans, centers = 5)
dataset$kmeans_5 <- seg_kmeans_5$cluster

#describe the segment solutions
#getting different values for breakdown
table(dataset$kmeans_3) #feasible
table(dataset$kmeans_4) #feasible
table(dataset$kmeans_5) #feasible

#summarize individual segments in a 4 cluster solution
summary(dataset[dataset$kmeans_4 == 1,1:5])
summary(dataset[dataset$kmeans_4 == 2,1:5])
summary(dataset[dataset$kmeans_4 == 3,1:5])
summary(dataset[dataset$kmeans_4 == 4,1:5])

#compare distribtuion of cluster variables among alternative solutions
table(dataset$hclust_3)
table(dataset$kmeans_3)

hist(dataset$interactive[dataset$hclust_3 == 2])
hist(dataset$interactive[dataset$kmeans_3 == 3]) # used clstr 3 in this 
# histogram because I'm getting different values than you
