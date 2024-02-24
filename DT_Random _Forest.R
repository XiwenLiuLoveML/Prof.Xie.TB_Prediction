###3.Decision tree(Random Forest)

#information for Prof. Xie to read:
#Advantage:1.Decision trees do not require feature scaling or normalization, 
#as they are invariant to monotonic transformations. 
#2. you can do feature selection:The importance of a feature can be determined 
#based on how early it appears in the tree and how often it is used for splitting.
#Disadvantage: 1.overfitting.2.less accurate.3.sensitive to small change.
#4.Decision trees can be biased towards the majority class 
#in imbalanced datasets. Your data is the case.

#I propose Random Forest:
#a tree-based ensemble method that leverage the strengths 
#of decision trees while addressing some of their limitations.
#Mechanism:You build a number of decision trees on bootstrapped training samples. 
#each time a split in a tree is considered, a random sample of m predictors 
#is chosen as split candidates from the full set of predictors. 

###please run data_prep.R before the following codes

# we will do gridsearch Cross Validation error.
grid_rf <- expand.grid( mtry=c(1:20) )
grid_rf
#set the training control 
train_ctrl <- trainControl(method="cv", # type of resampling in this case Cross-Validated
                           number=5, # number of folds
                           search = "grid", # we are performing a "grid-search"
                           classProbs = TRUE)
set.seed(400)
RF <- train(Death ~ .,
                       data = trainingSet,
                       method = "rf",
                       metric = "Accuracy", 
                       trControl = train_ctrl, 
                       tuneGrid = grid_rf,
                       # options to be passed to randomForest
                       ntree = 741,
                       keep.forest=TRUE,
                       importance=TRUE) 
RF
plot(RF)
#feature selection if needed
varImp(RF)
# check the optimal model
RF$finalModel

# predicting the test set observations
RFPred <- predict(RF,testSet, type = "raw")
# ordering the levels
RFPred <- ordered(RFPred, levels = c("dead", "alive"))
testSet$Death <- ordered(testSet$Death, levels = c("dead", "alive"))

# confusion matrix
cm <- table(Predicted = RFPred, Actual = testSet$Death)
cm
confusionMatrix(cm,mode = "everything")

#ROC

RFPredObj <- prediction(as.numeric(RFPred),as.numeric(testSet$Death))
RFPerfObj <- performance(RFPredObj, "tpr","fpr")
# plotting ROC curve
plot(RFPerfObj,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
# area under curve
aucLR <- performance(RFPredObj, measure = "auc")
aucLR <- aucLR@y.values[[1]]
aucLR
#0.9963504

