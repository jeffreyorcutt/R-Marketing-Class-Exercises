# Market Segementation Assisted Classwork
# 
# Author: Jeff Orcutt
# Date: 18 November 2021
#

library(cluster)

setwd("~/Desktop/R Data and Projects/data")

#read in data and move customer_type to specific factors
dataset<- read.csv("dataset_online_calculator_customers.csv")
dataset$customer_type<- as.factor(dataset$customer_type)
#dataset$test_factor <- as.factor(dataset$customer_type, levels = c("Individual","Small Business", "Large Business"), labels = c(1,2,3))
#dataset$customer_type[dataset$customer_type == "Individual"] <-1
#dataset$customer_type[dataset$customer_type == "Small Business"] <-2
#dataset$customer_type[dataset$customer_type == "Large Business"] <-3
#dataset$customer_type <- as.integer(dataset$customer_type)


#summarize data
summary(dataset)

#create a copy of the dataset to work with
data_hclust <- dataset[,2:5]

#create and view distance matrix
distances <- daisy(data_hclust)
distance_matrix <- as.matrix(distances)
distance_matrix[1:5,1:5]

#step 3: create cluster solutions using the selected method
seg_hclust <- hclust(distances, method = "complete")

plot(seg_hclust, labels= F)
#create a 3 cluster solution
#creates an array with the cluster number for each element 
seg_hclust_3 <- cutree(seg_hclust, k=3)

#assign the cluster numbers to the original dataset
dataset$hclust_3 <- seg_hclust_3
plot(seg_hclust, labels=F)
rect.hclust(seg_hclust, k=3, border="purple")

table(dataset$hclust_3)


#summarize individual segments in a 3 cluster solution
summary(dataset[dataset$hclust_3 == 1,1:5])
summary(dataset[dataset$hclust_3 == 2,1:5])
summary(dataset[dataset$hclust_3 == 3,1:5])

#create a 4 cluster solution
#creates an array with the cluster number for each element 
seg_hclust_4 <- cutree(seg_hclust, k=4)

#assign the cluster numbers to the original dataset
dataset$hclust_4 <- seg_hclust_4
plot(seg_hclust, labels=F)
rect.hclust(seg_hclust, k=4, border="green")

table(dataset$hclust_4)


#summarize individual segments in a 4 cluster solution
summary(dataset[dataset$hclust_4 == 1,1:5])
summary(dataset[dataset$hclust_4 == 2,1:5])
summary(dataset[dataset$hclust_4 == 3,1:5])
summary(dataset[dataset$hclust_4 == 4,1:5])

#create a 5 cluster solution
#creates an array with the cluster number for each element 
seg_hclust_5 <- cutree(seg_hclust, k=5)

#assign the cluster numbers to the original dataset
dataset$hclust_5 <- seg_hclust_5
plot(seg_hclust, labels=F)
rect.hclust(seg_hclust, k=5, border="red")

table(dataset$hclust_5)


#summarize individual segments in a 5 cluster solution
summary(dataset[dataset$hclust_5 == 1,1:5])
summary(dataset[dataset$hclust_5 == 2,1:5])
summary(dataset[dataset$hclust_5 == 3,1:5])
summary(dataset[dataset$hclust_5 == 4,1:5])
summary(dataset[dataset$hclust_5 == 5,1:5])

#-----K-Means clustering

data_kmeans <- data_hclust
data_kmeans$size[dataset$customer_type == "Individual"] <-1
data_kmeans$size[dataset$customer_type == "Small Business"] <-2
data_kmeans$size[dataset$customer_type == "Large Business"] <- 3
data_kmeans$customer_type <- NULL

#create candidate cluster solutions 
#k = 3

set.seed(1000)
seg_kmeans_3 <- kmeans(data_kmeans, centers = 3)
dataset$kmeans_3 <- seg_kmeans_3$cluster

#describe 3 cluster K-mean solution
table(dataset$kmeans_3)
summary(dataset[dataset$kmeans_3 == 1, 1:5])
summary(dataset[dataset$kmeans_3 == 2, 1:5])
summary(dataset[dataset$kmeans_3 == 3, 1:5])

#k = 4

set.seed(1000)
seg_kmeans_4 <- kmeans(data_kmeans, centers = 4)
dataset$kmeans_4 <- seg_kmeans_4$cluster
#describe 4 cluster K-mean solution
table(dataset$kmeans_4)
summary(dataset[dataset$kmeans_4 == 1, 1:5])
summary(dataset[dataset$kmeans_4 == 2, 1:5])
summary(dataset[dataset$kmeans_4 == 3, 1:5])
summary(dataset[dataset$kmeans_4 == 4, 1:5])

#k = 5

set.seed(1000)
seg_kmeans_5 <- kmeans(data_kmeans, centers = 5)
dataset$kmeans_5 <- seg_kmeans_5$cluster
#describe 5 cluster K-mean solution
table(dataset$kmeans_5)
summary(dataset[dataset$kmeans_5 == 1, 1:5])
summary(dataset[dataset$kmeans_5 == 2, 1:5])
summary(dataset[dataset$kmeans_5 == 3, 1:5])
summary(dataset[dataset$kmeans_5 == 4, 1:5])
summary(dataset[dataset$kmeans_5 == 5, 1:5])

table(dataset$customer_type)
(17/80)
(6/195)

