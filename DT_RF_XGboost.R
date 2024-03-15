

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

#caret library

# we will do gridsearch Cross Validation error.
grid_rf <- expand.grid( mtry=1:20 )
grid_rf

#control basic (general) parameters
RFControl <- trainControl(method = "cv",
                           summaryFunction = twoClassSummary, 
                          # deal with the imbalance:upsampling
                           sampling = "up",
                           classProbs = TRUE,
                           savePredictions = TRUE)
levels(trainingSet$Death) <- c("alive","dead")
levels(testSet$Death) <- c("alive","dead")
# model building using caret package
# tune model
set.seed(766)
RF <- train(Death ~ .,
                 data = trainingSet,
                 method = 'rf',
                 trControl = RFControl,
                 metric = "ROC",
                 tuneGrid = grid_rf,
                 ntree = 1000,
                 keep.forest=TRUE,
                 importance=TRUE #help feature selection
                 )
RF
plot(RF)
#feature selection if needed
varImp(RF) #Course.of.Antituberculosis.treatment
# check the optimal model
RF$finalModel
#ROC-AUC
ROC_AUC(RF,testSet)
#0.8277372mjk,.            

