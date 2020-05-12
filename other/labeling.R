#------------------------------------------------------------------------------#
# Vadlidation (on train set) metrics
#------------------------------------------------------------------------------#
bank_sample_train$predicted_cluster = db$cluster
str(bank_sample_train)
#------------------------------------------------------------------------------#

# Afrairw ta outliers
# which (db$cluster==0)

#------------------------------------------------------------------------------#
library(MLmetrics)

(ConfusionMatrix(bank_sample_train$predicted_cluster, as.integer(bank_sample_train$y)))
(Accuracy(bank_sample_train$predicted_cluster, as.integer(bank_sample_train$y)))

(Precision(as.integer(bank_sample_train$y), bank_sample_train$predicted_cluster))
(Recall(as.integer(bank_sample_train$y), bank_sample_train$predicted_cluster))
(F1_Score(as.integer(bank_sample_train$y), bank_sample_train$predicted_cluster))



#------------------------------------------------------------------------------------------------#
# New labels assignment according to clustering
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Train a tree on the bank_sample_train_clust 
#------------------------------------------------------------------------------------------------#

library(rpart)
library(rpart.plot)
library(rattle)

model<-rpart(predicted_cluster~., data=bank_sample_train_clust, method="class")

plot(model)
fancyRpartPlot(model)
text(model)

Y_pred=predict(model, bank_sample_test_clust_X, type="class")

#------------------------------------------------------------------------------------------------#
# Metrics after clustering and re-training
#------------------------------------------------------------------------------------------------#

(cm=ConfusionMatrix(Y_pred, as.numeric(bank_sample_test_clust_Y)))

cat(paste("Accuracy: \t", format(Accuracy(Y_pred, bank_sample_test_clust_Y ), digits=2), "\n", sep=" "))
cat(paste("Precision: \t", format(Precision(bank_sample_test_clust_Y, Y_pred), digits=2), "\n", sep=" "))
cat(paste("Recall: \t", format(Recall(bank_sample_test_clust_Y, Y_pred), digits=2), "\n", sep=" "))
cat(paste("F-Score: \t", format(F1_Score(bank_sample_test_clust_Y, Y_pred), digits=2), "\n", sep=" "))
