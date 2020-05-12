rm(list=ls())

bank<-read.csv("bank-additional-full.csv", sep=";")
bank$y<-relevel(bank$y, "yes")

bank$job<-as.numeric(as.factor(bank$job))
bank$marital<-as.numeric(as.factor(bank$marital))
bank$education<-as.numeric(as.factor(bank$education))
bank$default<-as.numeric(as.factor(bank$default))
bank$housing<-as.numeric(as.factor(bank$housing))
bank$loan<-as.numeric(as.factor(bank$loan))
bank$contact<-as.numeric(as.factor(bank$contact))
bank$month<-as.numeric(as.factor(bank$month))
bank$day_of_week<-as.numeric(as.factor(bank$day_of_week))
bank$previous<-as.numeric(as.factor(bank$previous))
bank$poutcome<-as.numeric(as.factor(bank$poutcome))

#-------------------------------------------------------------------#

#bank_sample = data[sample(nrow(data),20000) ,]
bank_sample = bank[ ,-11]

library(caret)

set.seed(81)

# Dimiourgia partition (statified)
inTrain <- createDataPartition(y=bank_sample$y, p=0.70, list=FALSE)
bank_sample_train = bank_sample[inTrain, ]
bank_sample_train_X = bank_sample_train[ , -ncol(bank_sample_train)]

#--------------------------------------------------------------------#
# bank_sample_train_X = as.data.frame(scale(bank_sample_train_X))
#--------------------------------------------------------------------#

#--------------------------------------------------------------------#
bank_sample_test = bank_sample[-inTrain, ]
bank_sample_test_X = bank_sample_test[ , -ncol(bank_sample_test)]
bank_sample_test_Y = bank_sample_test[ , ncol(bank_sample_test)]
#--------------------------------------------------------------------#

#--------------------------------------------------------------------#
# bank_sample_test_X = as.data.frame(scale(bank_sample_test_X))
#--------------------------------------------------------------------#


library(e1071)
library(MLmetrics)

svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = bank_sample_train)

Y_pred = predict(svm_model, bank_sample_test_X)

#--------------------------------------------------------------------#

(cm=ConfusionMatrix(Y_pred, bank_sample_test_Y))

cat(paste("Accuracy: \t", format(Accuracy(Y_pred, bank_sample_test_Y ), digits=2), "\n", sep=" "))
cat(paste("Precision: \t", format(Precision(bank_sample_test_Y, Y_pred), digits=2), "\n", sep=" "))
cat(paste("Recall: \t", format(Recall(bank_sample_test_Y, Y_pred), digits=2), "\n", sep=" "))
cat(paste("F-Score: \t", format(F1_Score(bank_sample_test_Y, Y_pred), digits=2), "\n", sep=" "))



