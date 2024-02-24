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


#read the data
#library("readxl")
data <- read_excel("原始数据.xls", sheet = 1)
data <- as.data.frame(data) #caret models takes fd
#data_info <- read_excel("原始数据.xls",sheet = 2)

###1.data analytics
str(data)
print(data_info, n = 47)
print(data,width = Inf)
#fix the data type
cols.num <- c(5:10,12:21)
data[cols.num] <- lapply(data[cols.num],as.logical)
cols.num <- c(1:4,11)
data[cols.num] <- lapply(data[cols.num],as.factor)
sapply(data, class)
levels(data$Death) <- c("alive","dead")
#because logistic Regression takes only factor or numeric output variable.

# data analytics
summary(data) 

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
figurelist[[3]]
#predictors with a distribution too skewed or full of outliers:
#Duration.of.diabetes
#predictors looks near zero variance: Cr,AST
figurelist[[4]]
#predictors looks near zero variance: ALT,TBIL,DBIL,CK

# check zero or near zero variance Predictors
#library(caret)
nzv <- nearZeroVar(data, saveMetrics= TRUE)
nzv
#conclusion: There is no near zero variance predictor.

# Identifying Correlated Predictors
descrCor <-  cor(data[22:47])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
highCorr
# conclusion: there is no highly correlated predictor

###conclusion from data analytics:
#1,imbalanced output data, use F1 score,AUC... better than accuracy
# data imbalance damage the classifiers. Bias towards majority.
# Solutions:
# undersampling majority (loss of information)
# oversampling minority (overfitting)
#2,numeric predictors mostly have distribution with 
# longer tail on the right side, box-cox or YeoJohnson
# transformation is appropriate.

###2.pre-processing
# manage imbalance
balanced_data <- upSample(data[,2:47],data$Death,yname = "Death")
summary(balanced_data)
data <- balanced_data
# scaling
scaling <- preProcess(data, method = c("center", "scale"))#,"YeoJohnson"))
#non-numeric variables will be ignored.
scaling
transformed <- predict(scaling, newdata = data)
head(transformed)

#split the data
#library(caTools)
# fixing the observations in training set and test set
set.seed(123)
# splitting the data set into ratio 0.70:0.30
split <- sample.split(transformed$Death, SplitRatio = 0.70)
# creating training dataset
trainingSet <- subset(transformed, split == TRUE)
# creating test data set
testSet <- subset(transformed, split == FALSE)

summary(trainingSet$Death)
summary(testSet$Death)
# control parameters
objControl <- trainControl(method = "boot", 
                           number = 2, 
                           returnResamp = 'none', 
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE,
                           savePredictions = TRUE)

###3.Logistic Regression
# model building using caret package
set.seed(766)
LR <- train(Death ~ .,
            data = trainingSet,
            method = 'glm',
            trControl = objControl,
            metric = "ROC")
summary(LR)
# predicting the test set observations
LRPred <- predict(LR, testSet, type = "prob")
LRPred <- LRPred[,2]
# plot of probabilities
plot(LRPred, 
     main = "Scatterplot of Probabilities of Death (test data)", ylab = "Predicted Probability of Default")
# setting the cut-off probablity
classify50 <- ifelse(LRPred > 0.5,"dead","alive")
# ordering the levels
classify50 <- ordered(classify50, levels = c("dead", "alive"))
testSet$Death <- ordered(testSet$Death, levels = c("dead", "alive"))

# confusion matrix
cm <- table(Predicted = classify50, Actual = testSet$Death)
cm
confusionMatrix(cm,mode = "everything")
#library(caret)
#library(dplyr)
# function to print confusion matrices for diffrent cut-off levels of probability
CmFn <- function(cutoff) {
  
  # predicting the test set results
  LRPred <- predict(LR, testSet, type = "prob")
  C1 <- ifelse(LRPred[,2] > cutoff, "dead", "alive")
  C2 <- testSet$Death
  predY   <- as.factor(C1)
  actualY <- as.factor(C2)
  
  predY <- ordered(predY, levels = c("dead", "alive"))
  actualY <- ordered(actualY, levels = c("dead", "alive"))
  
  # use the confusionMatrix from the caret package
  cm1 <-confusionMatrix(table(predY,actualY),mode = "everything")
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
  
  # combined table
  tab <- cbind(Accuracy,Sensitivity,Specificity,Kappa,F1)
  return(tab)}
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
tab5 <- cbind(cutoff1,tab4$Accuracy,tab4$Sensitivity,tab4$Specificity,tab4$Kappa,tab4$F1)
tab6 <- as.data.frame(tab5)
tab7 <- rename(tab6,cutoff =  cutoff1, Accuracy = V2 , 
               Senstivity = V3 ,Specificity =  V4 ,kappa = V5, F1 = V6)
tab7
#cutoff 0.45 with the best kappa and F1 score

# ROC and AUC
#library(ROCR)

lgPredObj <- prediction((1-LRPred),testSet$Death)
lgPerfObj <- performance(lgPredObj, "tpr","fpr")
# plotting ROC curve
plot(lgPerfObj,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
# area under curve
aucLR <- performance(lgPredObj, measure = "auc")
aucLR <- aucLR@y.values[[1]]
aucLR
#0.8423464


