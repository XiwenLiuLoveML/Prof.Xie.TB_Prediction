#set working directory
setwd("/Users/liuxiwen/Documents/谢老师项目")

#install the packages needed
#install.packages("readxl")
#install.packages("cowplot")
#install.packages("caret", dependencies = TRUE)

#loading library needed:
library("readxl") #for reading data
library(ggplot2) #for plotting
library(ggpubr) # for plotting
library(purrr) # for plotting
library(caret) # for preprocess data and fit model, predict...
library(caTools) # for splitting data into training and test set
library(dplyr) #for tuning hyperparameters like cutoff prob for logit
library(ROCR) #for plotting ROC and calculate AUC

#defining some functions for general evaluation
# functions to print confusion matrices for diffrent cut-off levels of probability
CmFn <- function(cutoff) {
  
  # predicting the test set results
  # model_fit is a trained model object from caret.
  Pred <- predict(model_fit, testSet, type = "prob")
  C1 <- ifelse(Pred[,2] > cutoff, "dead", "alive")
  C2 <- testSet$Death
  predY   <- as.factor(C1)
  actualY <- as.factor(C2)
  
  predY <- ordered(predY, levels = c("dead", "alive"))
  actualY <- ordered(actualY, levels = c("dead", "alive"))
  
  # use the confusionMatrix from the caret package
  cm1 <-confusionMatrix(table(predY,actualY))
  # extracting accuracy
  Accuracy <- cm1$overall[1]
  # extracting sensitivity
  Sensitivity <- cm1$byClass[1]
  # extracting specificity
  Specificity <- cm1$byClass[2]
  # extracting value of kappa
  Kappa <- cm1$overall[2]
  # extracting value of F1 score
  F1 <- cm1$byClass[7]
  # calculate AUC
  LRPredObj <- prediction(as.numeric(predY),as.numeric(actualY))
  AUC <- performance(LRPredObj, measure = "auc")
  AUC <- AUC@y.values[[1]]
  
  # combined table
  tab <- cbind(Accuracy,Sensitivity,Specificity,Kappa,F1,AUC)
  return(tab)}

cutoff_evaluation <- function(model_fit){
  model_fit <- model_fit
  # making sequence of cut-off probabilities       
  cutoff1 <- seq( .1, .9, by = .05 )
  # loop using "lapply"
  tab2    <- lapply(cutoff1, CmFn)
  # extra coding for saving table as desired format
  tab3 <- rbind(tab2[[1]],tab2[[2]],tab2[[3]],tab2[[4]],tab2[[5]],tab2[[6]],tab2[[7]],
                tab2[[8]],tab2[[9]],tab2[[10]],tab2[[11]],tab2[[12]],tab2[[13]],tab2[[14]],
                tab2[[15]],tab2[[16]],tab2[[17]])
  # printing the table
  tab4 <- as.data.frame(tab3)
  tab5 <- cbind(cutoff1,tab4$Accuracy,tab4$Sensitivity,tab4$Specificity,tab4$Kappa,tab4$F1,tab4$AUC)
  tab6 <- as.data.frame(tab5)
  tab7 <- rename(tab6,cutoff =  cutoff1, Accuracy = V2 , 
                 Senstivity = V3 ,Specificity =  V4 ,kappa = V5, F1 = V6, AUC = V7)
  return(tab7)
}


#read the data
#library("readxl")
data <- read_excel("原始数据.xls", sheet = 1)
data <- as.data.frame(data) #caret models takes df
data_info <- read_excel("原始数据.xls",sheet = 2)

###1.data analytics
str(data)
print(data_info, n = 47)
print(data)

#fix the data type
cols.num <- c(5:10,12:21)
data[cols.num] <- lapply(data[cols.num],as.logical)
cols.num <- c(1:4,11)
data[cols.num] <- lapply(data[cols.num],as.factor)
sapply(data, class)
#levels(data$Death) <- c("alive","dead")
#because logistic Regression takes only factor or numeric output variable.

# visualization
#library(ggplot2)
#library(ggpubr)
#library(purrr)
#library(cowplot)
plot_f <- function(i){
  ### define a function to plot different predictors according to their data type
  col_N <- colnames(data)
  #x value equals to the name of the column
  if (i<22 & i > 1) { 
    ggplot(data, aes(x =!!sym(col_N[i]), colour = Death, label = Death)) + geom_bar()
  } else if (i>21 & i <48) {
    ggplot(data, aes(x =!!sym(col_N[i]), colour = Death, label = Death)) + geom_boxplot()
  } else if (i==1) {
    ggplot(data, aes(x =!!sym(col_N[i]))) + geom_bar()
  }else {
    print("i index number for variable should be within range [1,47]")
  }
}
# make a plot list of every varible
figurelist <- list()
for (i in 0:2){
  plot_list <- map(12*i+(1:12),plot_f)
  figurelist[[i+1]]<-ggarrange(plotlist = plot_list,nrow = 3, ncol = 4)
}
plot_list <- map(37:47,plot_f)
figurelist[[4]]<-ggarrange(plotlist = plot_list,nrow = 3, ncol = 4)
figurelist[[1]]
#output variable looks imbalanced.
#predictors look imbalanced:Gender,Medical.insurance,Educational.attainment
#Drinking,Cavity,Pleural.effusion,Hypertension.Classification
figurelist[[2]]
#predictors look imbalanced:Heart.failure,Hyperlipidemia,Stroke,CHD
#CKD,COPD,Antihypertensive.medications,Lipid_lowering.drugs
#predictors with a distribution too skewed or full of outliers:
#Course.of.disease
ggplot(data, aes(x = !!sym(col_N[22]), colour = Death, label = Death)) + geom_density()
figurelist[[3]]
#predictors with a distribution too skewed or full of outliers:
#Duration.of.diabetes
#predictors looks near zero variance: Cr,AST
ggplot(data, aes(x = !!sym(col_N[36]), colour = Death, label = Death)) + geom_density()
figurelist[[4]]
#predictors looks near zero variance: ALT,TBIL,DBIL,CK
ggplot(data, aes(x = !!sym(col_N[37]), colour = Death, label = Death)) + geom_density()

# check zero or near zero variance Predictors
#library(caret)
nzv <- nearZeroVar(data, saveMetrics= TRUE)
nzv
#conclusion: There is no near zero variance predictor.

# Identifying Correlated Predictors
NumericData <- data[22:47]
numColN <- colnames(NumericData)
descrCor <-  cor(NumericData)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
highCorr # conclusion: there is no highly correlated predictor
#removing descriptors with absolute correlations above 0.75.
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
numColN <- colnames(NumericData)
numColN[highlyCorDescr]
filteredDescr <- NumericData[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])
highlyCorRemovedData <-data[,-(highlyCorDescr+21)]

#conclusion from data analytics:
#1,imbalanced output data, use F1 score,AUC... better than accuracy
# data imbalance damage the classifiers. Bias towards majority.
# Solutions:
# undersampling majority (loss of information)
# oversampling minority (overfitting)
#2,numeric predictors mostly have distribution with 
# longer tail on the right side, box-cox or YeoJohnson
# transformation is appropriate.
#3.factor variables numeric values will influence model. 
# model may think some levels the higher the better.
# solution:one hot encoding.



###2.pre-processing
# scaling and centering
scaling <- preProcess(highlyCorRemovedData, method = c("center", "scale"))#,"YeoJohnson"))
#non-numeric variables will be ignored.
scaling
transformed <- predict(scaling, newdata = highlyCorRemovedData)
head(transformed)
# one-hot-encoding
dummies <- dummyVars(~ ., data = transformed[-1], fullRank = T)
dmfpredictors <- predict(dummies, newdata = transformed[-1])
T_D_data <- data.frame(transformed[1],dmfpredictors)
summary(T_D_data)



###3.split the data
#library(caTools)
# fixing the observations in training set and test set
set.seed(123)
# splitting the data set into ratio 0.70:0.30
split <- sample.split(T_D_data$Death, SplitRatio = 0.70)
# creating training dataset
trainingSet <- subset(T_D_data, split == TRUE)
summary(trainingSet$Death)
# creating test data set
testSet <- subset(T_D_data, split == FALSE)
summary(testSet$Death)



###4. manage imbalance. (you shouldn't upsample before you split the data!)
BtrainingSet <- upSample(trainingSet[-1],trainingSet$Death,yname = "Death")
summary(BtrainingSet$Death)
summary(BtrainingSet) # Death changed the column location.


###5. model fitting

#5.1 Logistic Regression
set.seed(1)
# control parameters
objControl <- trainControl(method = "cv", 
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE)
levels(BtrainingSet$Death) <- c("alive","dead")
levels(testSet$Death) <- c("alive","dead")
LR <- train(Death ~ .,
            data = BtrainingSet,
            method = 'glm',
            trControl = objControl,
            metric = "ROC")
summary(LR)
#prediction
LRPred <- predict(LR, testSet,type = "prob")
LRPredraw <- predict(LR, testSet,type = "raw")
LRPredObj <- prediction(LRPred[,2],testSet$Death)
LRPerfObj <- performance(LRPredObj, "tpr","fpr")
# plotting ROC curve
plot(LRPerfObj,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
# area under curve
aucLR <- performance(LRPredObj, measure = "auc")
aucLR <- aucLR@y.values[[1]]
aucLR
#0.7984185


#5.2 Decision Tree, Random Forest, and XGBoost
#Advantage:1.Decision trees do not require feature scaling or normalization, 
#as they are invariant to monotonic transformations. 
#2. you can do feature selection:The importance of a feature can be determined 
#based on how early it appears in the tree and how often it is used for splitting.
#Disadvantage: 1.overfitting.2.less accurate.3.sensitive to small change.
#4.Decision trees can be biased towards the majority class 
#in imbalanced datasets. Your data is the case.
# boosting tree 
# disadvantage:Boosting models are vulnerable to outliers. 
#Because each model attempts to correct the faults of its predecessor,
#outliers can skew results significantly. 

# random forest: bagging tree
# control parameters
objControl <- trainControl(method = "cv", 
                           number = 10,
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE,
                           allowParallel = TRUE)
# grid search for tuning hyperparameters
tune_grid <- expand.grid(mtry=1:20)
set.seed(2)
rf_tune <- train(Death ~ .,
                  data = BtrainingSet,
                  method = "rf",
                  tuneGrid = tune_grid)

#boosted tree
# control parameters
objControl <- trainControl(method = "cv", 
                           number = 10,
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE,
                           allowParallel = TRUE)

# grid search for tuning hyperparameters
tune_grid <- expand.grid(
  nrounds = 100,
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(3, 5, 10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(2)
xgb_tune <- train(Death ~ .,
                  data = BtrainingSet,
                  method = "xgbTree",
                  tuneGrid = tune_grid
                  )
GBM
#Plotting the Resampling Profile
trellis.par.set(caretTheme())
plot(GBM)
plot(GBM, metric = "ROC")


# Multi-Layer Perceptron
# control parameters
objControl <- trainControl(method = "cv", 
                           number = 10,
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE)
tune_grid <- expand.grid(
  layer1 = c( 25, 30, 35),
  layer2 = c( 10, 20, 30),
  layer3 = c( 5, 10, 15)
)

set.seed(4)
MLP <- train(Death ~ .,
             data = BtrainingSet,
             method = "mlpML",
             tuneGrid = tune_grid)
trellis.par.set(caretTheme())
plot(MLP)
MLP$finalModel


#prediction
MLPPred <- predict(MLP, testSet,type = "prob")
MLPPredObj <- prediction(MLPPred[,2],testSet$Death)
MLPPerfObj <- performance(MLPPredObj, "tpr","fpr")
# plotting ROC curve
plot(MLPPerfObj,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
# area under curve
aucMLP <- performance(MLPPredObj, measure = "auc")
aucMLP <- aucMLP@y.values[[1]]
aucMLP
# 0.75882
MLP$results
