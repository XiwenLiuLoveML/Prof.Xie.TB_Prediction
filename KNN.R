#KNN
#Advantage:it doesn't make any assumption about
#the underlying distribution of the data.
#disadvantage:
#In imbalanced datasets, KNN becomes biased towards 
#the majority instances of the training space.

#model tuning
set.seed(100)
KNN_tune <- train(
  Death ~ ., 
  data = trainingSet, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3,5,7))
)
#After finding the best value of “K”, we will train the 
#KNN classification model with a scaled training dataset.
KNN<- knn3(
  Death ~ .,
  data = trainingSet,
  k = KNN_tune$bestTune$k
)

# Calculate confusion matrix
KNNPred <- predict(KNN, testSet,type = "class")
KNNPred <- ordered(KNNPred, levels = c("dead", "alive"))
testSet$Death <- ordered(testSet$Death, levels = c("dead", "alive"))
cm <- confusionMatrix(KNNPred, testSet$Death,mode = "everything")
cm
#ROC

KNNPredObj <- prediction(as.numeric(KNNPred),as.numeric(testSet$Death))
KNNPerfObj <- performance(KNNPredObj, "tpr","fpr")
# plotting ROC curve
plot(KNNPerfObj,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
# area under curve
aucLR <- performance(KNNPredObj, measure = "auc")
aucLR <- aucLR@y.values[[1]]
aucLR
#0.9069343

