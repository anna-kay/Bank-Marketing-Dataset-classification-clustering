rm(list=ls())

bank<-read.csv("bank-additional-full.csv", sep=";")
bank$y<-relevel(bank$y, "yes")

# Define train_sample & test_sample
data<- bank

set.seed(81)

bank_sample = data[sample(nrow(data),3500),]
bank_sample_× = bank_sample[ , -ncol(bank_sample)]
bank_sample_Y = bank_sample[ , col(bank_sample)]

#library(caret)

#inTrain <- createDataPartition(y=bank_sample$y, p=0.70, list=FALSE)

#bank_sample_train = bank_sample[inTrain, ]
#bank_sample_train_X = bank_sample_train[ , -ncol(bank_sample_train)] 

#bank_sample_test = bank_sample[-inTrain, ]
#bank_sample_test_X = bank_sample_test[ , -ncol(bank_sample_test)]

#-----------------------------------------------------------------------------#
# DBSCAN Clustering
#-----------------------------------------------------------------------------#
library(dbscan)

dissimilarity_matrix<-as.matrix(daisy(bank_sample_×, metric = "gower")) # enallaktika allo metric

# Optimize the parameter eps of the DBSCAN algorithm, knn distance
knndist = kNNdist(dissimilarity_matrix, k = 20)

# Get the distances of the 10 nearest neighbours for each point
kdist = knndist[ ,20]

# Plot distances
plot(sort(kdist), type = 'l', xlab = "Points sorted by distance",
     ylab = "20-NN distance")

db = dbscan(dissimilarity_matrix, eps = 3.5, minPts = 20)

print(db)
str(db$cluster)
summary(db$cluster)
plot(dissimilarity_matrix, col=db$cluster, pch=15)

#-----------------------------------------------------------------------------#
# DBSCAN Clustering - Internal Evaluation - Silhouette
#-----------------------------------------------------------------------------#

model_silhouette = silhouette(db$cluster, dissimilarity_matrix)
model_silhouette
mean_sil=mean(model_silhouette[,3])
mean_sil

#-----------------------------------------------------------------------------#
