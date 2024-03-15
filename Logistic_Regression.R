#2,imbalanced output data, use Kappa,F1 score,AUC... better than accuracy
# data imbalance damage the classifiers. Bias towards majority.
# Solutions:
# undersampling majority (loss of information)
# oversampling minority (over-fitting)
#3,numeric predictors mostly have distribution with 
# longer tail on the right side
# solutions: box-cox or YeoJohnson transformation.
# The errors after modeling should be normal to 
# draw a valid conclusion by hypothesis testing.
#4.no. of outliers is too high. Likely due to data imbalance.
#solution: transforming the data (log, square root, and inverse)
#using robust modeling techniques (decision tree based model)
# using ensemble methods (boosting, bagging)
# using regularization (lasso, ridge)
#5. Correlation:there are 3 variables highly (>0.75) correlated to others.
#solution: Greedy elimination. combine score, CPA, ridge...


# 1. predictors correlation can dilute true associations and lead to large 
# standard errors with wide and imprecise confidence intervals, or,
# conversely, identify spurious associations. 
# 2.unbalanced data heavily affects the prediction capabilities 
# of a logistic regression model. 
# 3. It is often observed outliers have a considerable influence on the 
# analysis results, which may lead the study to the wrong conclusions.


### Preprocessing
#remove outliers with high MCD (minimum covariance determinant)
#library(MASS)
#library(MASS)


# correlation solution: greedy elimilation
#removing descriptors with absolute correlations above 0.75.
#find index where numeric variable starts
grep("Course.of.disease", colnames(trainingSet)) #24
length(trainingSet) #49
NumericData <- trainingSet[24:49]
numColN <- colnames(NumericData)
descrCor <-  cor(NumericData)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
numColN[highlyCorDescr]
filteredDescr <- NumericData[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])
# remove the data with cor higher than 0.75
colnames(trainingSet)[highlyCorDescr+23]
c_trainingSet <-trainingSet[,-(highlyCorDescr+23)]
c_testSet <-testSet[,-(highlyCorDescr+23)]


# skewness: scaling, transforming
scaling <- preProcess(c_trainingSet, method = c("center", "scale"))
LRtrainingSet <- predict(scaling, newdata = c_trainingSet)
LRtestSet <- predict(scaling, newdata = c_testSet)


### fit the model
#control basic (general) parameters
LRControl <- trainControl(method = "cv", 
                           # deal with the imbalance:upsampling
                           sampling = "up",
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE,
                           savePredictions = TRUE)
levels(LRtrainingSet$Death) <- c("alive","dead")
levels(LRtestSet$Death) <- c("alive","dead")
###4.Logistic Regression
# model building using caret package
set.seed(766)
LR <- train(Death ~ .,
            data = LRtrainingSet,
            method = 'glm',
            trControl = LRControl,
            metric = "ROC")
summary(LR)
pchisq(1089.2 , 1234) #0.001248656 # Good!
LR$result
# ROC and AUC
#library(ROCR)
ROC_AUC(LR, LRtestSet)
#0.7951338

