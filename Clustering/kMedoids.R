rm(list=ls())

bank<-read.csv("bank-additional-full.csv", sep=";")
bank$y<-relevel(bank$y, "yes")

#-----------------------------------------------------------------------------#

data <-bank 

#-----------------------------------------------------------------------------#
set.seed(81)

bank_sample = data[sample(nrow(data), 3500) , ]
#bank_sample = data

bank_sample_X = bank_sample[ , -ncol(bank_sample)]

#library(caret)

# Dimiourgia partition (statified)
#inTrain <- createDataPartition(y=bank_sample$y, p=0.70, list=FALSE)

# = bank_sample[inTrain, ]
#bank_sample_train_X = bank_sample_train[ , -ncol(bank_sample_train)] 

#bank_sample_test = bank_sample[-inTrain, ]
#bank_sample_test_X = bank_sample_test[ , -ncol(bank_sample_test)]

#-----------------------------------------------------------------------------#
library(cluster)

dissimilarity_matrix<-as.matrix(daisy(bank_sample_X , metric = "gower")) # enallaktika allo metric

medoids_model<- pam(dissimilarity_matrix, 2)

medoids_model$medoids
clusters=medoids_model$clustering

#-----------------------------------------------------------------------------#
# kMedoids Clustering - Internal Evaluation - Silhouette
#-----------------------------------------------------------------------------#
model_silhouette = silhouette(medoids_model$clustering, dissimilarity_matrix)#den to bgazei gia megalo pinaka 
model_silhouette

mean_sil=mean(model_silhouette[,3])
mean_sil

#plot(dissimilarity_matrix, col=clusters, pch=15)
#------------------------------------------------------------------------------------------------#

clusters<-medoids_model$clustering

#------------------------------------------------------------------------------------------------#
# kMedoids Clustering - Evaluation based on classes
#------------------------------------------------------------------------------------------------#

predicted_cluster<-replace(clusters, clusters==1, "no")
predicted_cluster<-replace(predicted_cluster, predicted_cluster==2, "yes")
predicted_cluster<-as.factor(predicted_cluster)
predicted_cluster<-relevel(predicted_cluster, "yes")
summary(predicted_cluster)

Y_pred<-predicted_cluster
Y_true<-bank_sample[ , ncol(bank_sample)]

#------------------------------------------------------------------------------------------------#

(cm=ConfusionMatrix(Y_pred, Y_true))

cat(paste("Accuracy: \t", format(Accuracy(Y_pred, Y_true ), digits=2), "\n", sep=" "))
cat(paste("Precision: \t", format(Precision(Y_true, Y_pred), digits=2), "\n", sep=" "))
cat(paste("Recall: \t", format(Recall(Y_true, Y_pred), digits=2), "\n", sep=" "))
cat(paste("F-Score: \t", format(F1_Score(Y_true, Y_pred), digits=2), "\n", sep=" "))

#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# Training Naive Bayes on clustering
#------------------------------------------------------------------------------------------------#

bank_sample_X$predicted_cluster<-predicted_cluster


library(e1071)
library(MLmetrics)


model<-naiveBayes(predicted_cluster~., data=bank_sample_X)

new_random_sample = data[sample(nrow(data), 1000),]
new_random_sample_Y= new_random_sample[ , ncol(new_random_sample)]

Y_pred=predict(model, new_random_sample, type="class")
Y_true=new_random_sample_Y

#------------------------------------------------------------------------------------------------#
# Metrics after clustering and re-training
#------------------------------------------------------------------------------------------------#

(cm=ConfusionMatrix(Y_pred, Y_true))

cat(paste("Accuracy: \t", format(Accuracy(Y_pred, Y_true ), digits=2), "\n", sep=" "))
cat(paste("Precision: \t", format(Precision(Y_true, Y_pred), digits=2), "\n", sep=" "))
cat(paste("Recall: \t", format(Recall(Y_true, Y_pred), digits=2), "\n", sep=" "))
cat(paste("F-Score: \t", format(F1_Score(Y_true, Y_pred), digits=2), "\n", sep=" "))