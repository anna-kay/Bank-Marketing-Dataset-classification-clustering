rm(list=ls())

bank<-read.csv("bank-additional-full.csv", sep=";")
bank$y<-relevel(bank$y, "yes")

#------------------------------------------------------------------------------------------------#
# Epilogi ari8mitikwn metavlitwn kai kanonikopoiisi
#------------------------------------------------------------------------------------------------#

job<-as.integer(as.factor(bank$job))
marital<-as.integer(as.factor(bank$marital))
education<-as.integer(as.factor(bank$education))
default<-as.integer(as.factor(bank$default))
housing<-as.integer(as.factor(bank$housing))
loan<-as.integer(as.factor(bank$loan))
contact<-as.integer(as.factor(bank$contact))
month<-as.integer(as.factor(bank$month))
day_of_week<-as.integer(as.factor(bank$day_of_week))
poutcome<-as.integer(as.factor(bank$poutcome))

#------------------------------------------------------------------------------------------------#
# Epilogi metavlitwn pou 8a xrisimopoi8oun ws features
#------------------------------------------------------------------------------------------------#

bank_hc_complete <- bank

bank_hc_complete$job<-job
bank_hc_complete$marital<-marital
bank_hc_complete$education<-education
bank_hc_complete$default<-default
bank_hc_complete$housing<-housing
bank_hc_complete$loan<-loan
bank_hc_complete$contact<-contact
bank_hc_complete$month<-month
bank_hc_complete$day_of_week<-day_of_week
bank_hc_complete$poutcome<-poutcome


#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# Orismos train_sample kai test_sample
#------------------------------------------------------------------------------------------------#
data<- bank_hc_complete

set.seed(100)
bank_sample = data[sample(nrow(data),3500),]
#bank_sample = data

# Afairesi stilis klasis
bank_sample_X = bank_sample[ , -ncol(bank_sample)]

#library(caret)
#inTrain <- createDataPartition(y=bank_sample$y, p=0.70, list=FALSE)

#bank_sample_train = bank_sample[inTrain, ]
#bank_sample_train_X = bank_sample_train[ , -ncol(bank_sample_train)]
#bank_sample_train_Y = bank_sample_train[ , ncol(bank_sample_train)] 

#bank_sample_test = bank_sample[-inTrain, ]
#bank_sample_test_X = bank_sample_test[ , -ncol(bank_sample_test)]
#bank_sample_test_Y = bank_sample_test[ , ncol(bank_sample_test)]

#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Ekpaideusi montelou
#------------------------------------------------------------------------------------------------#

# Pinakas apostasewn (distance matrix)
d=dist(scale(bank_sample_X))

hc_complete = hclust(d, method="complete")
#lot(hc_complete)

# k=2 gia 2 clusters
clusters = cutree(hc_complete, k=2)
#range(clusters)

#plot(data, col=clusters+2, pch=15, main="Complete Linkage")
#rect.hclust(hc_complete, k=2)

#------------------------------------------------------------------------------------------------#
# Hierarchical Clustering - Internal Evaluation
#------------------------------------------------------------------------------------------------#
library(cluster)
model_silhouette = silhouette(clusters, d)
model_silhouette

mean_sil=mean(silhouette(clusters, d)[ ,3])
mean_sil

#------------------------------------------------------------------------------------------------#
# Hierarchical Clustering - Evaluation based on classes
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