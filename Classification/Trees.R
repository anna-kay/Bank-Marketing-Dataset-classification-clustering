rm(list=ls())

bank<-read.csv("bank-additional-full.csv", sep=";")
bank$y<-relevel(bank$y, "yes")

data<-bank[ ,-11]

#bank_sample = data[sample(nrow(data),5000) ,]
bank_sample = data

library(caret)
set.seed(81)

# Dimiourgia partition (statified)
inTrain <- createDataPartition(y=bank_sample$y, p=0.70, list=FALSE)

bank_sample_train = bank_sample[inTrain, ]
bank_sample_train_X = bank_sample_train[ , -ncol(bank_sample_train)] 

bank_sample_test = bank_sample[-inTrain, ]
bank_sample_test_X = bank_sample_test[ , -ncol(bank_sample_test)]
bank_sample_test_Y = bank_sample_test[ , ncol(bank_sample_test)]

#--------------------------------------------------------------------------#
library(rpart)
library(rpart.plot)
library(rattle)
library(MLmetrics)

model<-rpart(y~., data=bank_sample_train, method="class", parms=list(split="gini"), control=rpart.control(minsplit=200, minbucket=200, maxdepth=10, cp=0.016))

plot(model)
fancyRpartPlot(model)
text(model)

#printcp(model) # display the results 
#plotcp(model) # visualize cross-validation results 
#summary(model) # detailed summary of splits

#--------------------------------------------------------------------------#

Y_pred=predict(model, bank_sample_test_X, type="class")

#--------------------------------------------------------------------------#

(cm=ConfusionMatrix(Y_pred, bank_sample_test_Y))

cat(paste("Accuracy: \t", format(Accuracy(Y_pred, bank_sample_test_Y ), digits=2), "\n", sep=" "))
cat(paste("Precision: \t", format(Precision(bank_sample_test_Y, Y_pred), digits=2), "\n", sep=" "))
cat(paste("Recall: \t", format(Recall(bank_sample_test_Y, Y_pred), digits=2), "\n", sep=" "))
cat(paste("F-Score: \t", format(F1_Score(bank_sample_test_Y, Y_pred), digits=2), "\n", sep=" "))
