rm(list=ls())

bank<-read.csv("bank-additional-full.csv", sep=";")
bank$y<-relevel(bank$y, "yes")

#----------------------------------------------------------------------------#
# Epilogi ari8mitikwn metavlitwn kai kanonikopoiisi
#----------------------------------------------------------------------------#
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

"age<-as.data.frame(lapply(bank$age, normalize))
duration<-as.data.frame(lapply(bank$duration, normalize))
campaign<-as.data.frame(lapply(bank$campaign, normalize))
pdays<-as.data.frame(lapply(bank$pdays, normalize))
previous<-as.data.frame(lapply(bank$previous, normalize))
emp.var.rate<-as.data.frame(lapply(bank$emp.var.rate, normalize))
cons.price.idx<-as.data.frame(lapply(bank$cons.price.idx, normalize))
cons.conf.idx <-as.data.frame(lapply(bank$cons.conf.idx, normalize))
euribor3m <-as.data.frame(lapply(bank$euribor3m, normalize))
nr.employed<-as.data.frame(lapply(bank$nr.employed, normalize))"

#----------------------------------------------------------------------------#

"job<-as.numeric(as.factor(bank$job))
marital<-as.numeric(as.factor(bank$marital))
education<-as.numeric(as.factor(bank$education))
default<-as.numeric(as.factor(bank$default))
housing<-as.numeric(as.factor(bank$housing))
loan<-as.numeric(as.factor(bank$loan))
contact<-as.numeric(as.factor(bank$contact))
month<-as.numeric(as.factor(bank$month))
day_of_week<-as.numeric(as.factor(bank$day_of_week))
poutcome<-as.numeric(as.factor(bank$poutcome))"

y<-as.factor(bank$y)


#----------------------------------------------------------------------------#
# Epilogi metavlitwn pou 8a xrisimopoi8oun ws features
#----------------------------------------------------------------------------#

bank_hc_single <- cbind.data.frame(bank$age, bank$duration, bank$pdays, bank$y)

#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Orismos train_sample kai test_sample
#----------------------------------------------------------------------------#
data<- bank_hc_single

bank_sample = data[sample(nrow(data),1000),]
#bank_sample = data

library(caret)

# Dimiourgia partition (statified)
inTrain <- createDataPartition(y=bank_sample$y, p=0.70, list=FALSE)

bank_sample_train = bank_sample[inTrain, ]
bank_sample_train_X = bank_sample_train[ , -ncol(bank_sample_train)] 

bank_sample_test = bank_sample[-inTrain, ]
bank_sample_test_X = bank_sample_test[ , -ncol(bank_sample_test)]

#---------------------------------------------------------------------------#
# Pinakas apostasewn (distance matrix)

d=dist("bank_numeric")

hc_single=hclust(d, method="single")
plot(hc_single)

# k=2 gia 2 clusters
clusters=cutree(hc_single, k=2)
#range(clusters)

plot(data, col=clusters+2, pch=15, main="Single Linkage")

-----------------------------------------------------------------------------#
# Evaluation - Model Metrics
#-----------------------------------------------------------------------------#
#cohesion=model$tot.withinss 
#cohesion
  
#separation=model$betweenss
#separation
  
silhouette = silhouette(clusters, dist(bank_sample_train_X))#den to bgazei gia megalo pinaka 
silhouette

mean_sil=mean(silhouette[ ,3])
mean_sil

#------------------------------------------------------------------------------#
# Vadlidation (on train set) metrics
#------------------------------------------------------------------------------#
bank_sample_train$predicted_cluster = clusters
str(bank_sample_train)

library(MLmetrics)

(ConfusionMatrix(bank_sample_train$predicted_cluster, as.integer(bank_sample_train$y)))
(Accuracy(bank_sample_train$predicted_cluster, as.integer(bank_sample_train$y)))
(Precision(as.integer(bank_sample_train$y), bank_sample_train$predicted_cluster))
(Recall(as.integer(bank_sample_train$y), bank_sample_train$predicted_cluster))
(F1_Score(as.integer(bank_sample_train$y), bank_sample_train$predicted_cluster))

#------------------------------------------------------------------------------#
# Vadlidation (on train set) metrics
#------------------------------------------------------------------------------#