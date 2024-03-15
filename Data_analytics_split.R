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
library(MASS) # for removing outliers with high MCD.
library(caret) # for preprocess data and fit model, predict...
library(caTools) # for splitting data into training and test set
library(dplyr) #for tuning hyperparameters like cutoff prob for logit
library(ROCR) #for plotting ROC and calculate AUC

# function to plot ROC and calculate AUC
ROC_AUC <- function(model,testSet){
  #prediction
  Pred <- predict(model, testSet,type = "prob")
  PredObj <- prediction(Pred[,2],testSet$Death)
  PerfObj <- performance(PredObj, "tpr","fpr")
  # plotting ROC curve
  plot(PerfObj,main = paste (model$method, "ROC Curve"),col = 2,lwd = 2)
  abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
  # area under curve
  auc <- performance(PredObj, measure = "auc")
  auc <- auc@y.values[[1]]
  return(auc)
}

# function to make one plot according to the datatype
plot_f <- function(data,i){
  ### define a function to plot different predictors 
  #according to their data type
  col_N <- colnames(data)
  #x value equals to the name of the column
  if (is.factor(data[,i]) | is.logical(data[,i])) { 
    # if the variable is factor or logical, plot barplot
    ggplot(data, aes(x =!!sym(col_N[i]), colour = Death, label = Death)) + geom_bar()
  } else {
    # if the variable is numeric, plot density
    ggplot(data, aes(x =!!sym(col_N[i]), colour = Death, label = Death)) + geom_density()
  }
}

# function to make a plot list of every variable
plot_all <- function(data){
  N <- length(data)
  z <- N%/%12 # put 12 plots in one figure, z+1 is the total figure no.
  figurelist <- list()
  for (i in 0:(z-1)){
    # put 12 plots in one figure
    plot_list <- pmap(list(list(data), (12*i+(1:12))),plot_f)
    figurelist[[i+1]]<-ggarrange(plotlist = plot_list,nrow = 3, ncol = 4)
  }
  plot_list <- pmap(list( list(data), (12*z+1):N),plot_f)
  figurelist[[z+1]]<-ggarrange(plotlist = plot_list,nrow = 3, ncol = 4)
  return(figurelist)
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

plot_f <- function(data,i){
  ### define a function to plot different predictors 
  #according to their data type
  col_N <- colnames(data)
  #x value equals to the name of the column
  if (is.factor(data[,i]) | is.logical(data[,i])) { 
    # if the variable is factor or logical, plot barplot
    ggplot(data, aes(x =!!sym(col_N[i]), colour = Death, label = Death)) + geom_bar()
  } else {
    # if the variable is numeric, plot density
    ggplot(data, aes(x =!!sym(col_N[i]), colour = Death, label = Death)) + geom_density()
  }
}

# make a plot list of every varible
plot_all <- function(data){
  N <- length(data)
  z <- N%/%12 # put 12 plots in one figure, z+1 is the total figure no.
  figurelist <- list()
  for (i in 0:(z-1)){
    # put 12 plots in one figure
    plot_list <- pmap(list(list(data), (12*i+(1:12))),plot_f)
    figurelist[[i+1]]<-ggarrange(plotlist = plot_list,nrow = 3, ncol = 4)
  }
  plot_list <- pmap(list( list(data), (12*z+1):N),plot_f)
  figurelist[[z+1]]<-ggarrange(plotlist = plot_list,nrow = 3, ncol = 4)
  return(figurelist)
}

figurelist <- plot_all(data)
figurelist[[1]]
#output variable looks imbalanced.
#predictors imbalance:Gender,Medical.insurance,Educational.attainment
#Drinking,Cavity,Pleural.effusion,Hypertension.Classification
figurelist[[2]]
#predictors look imbalanced:Heart.failure,Hyperlipidemia,Stroke,CHD
#CKD,COPD,Antihypertensive.medications,Lipid_lowering.drugs
#predictors with a distribution too skewed or full of outliers:
#Course.of.disease
plot_f(data,22)
length(boxplot(data[22])$out)
#outlier: remove? transform?
#去除outlier要split data之前
figurelist[[3]]
#predictors with a distribution too skewed or full of outliers:
#Duration.of.diabetes
#predictors looks near zero variance: Cr,AST
plot_f(data,36)
figurelist[[4]]
#predictors looks near zero variance: ALT,TBIL,DBIL,CK
plot_f(data,37)
# check zero or near zero variance Predictors
#library(caret)
nzv <- nearZeroVar(data, saveMetrics= TRUE)
nzv
#conclusion: There is no near zero variance predictor.
# Missing Value Analysis
sum(is.na(data))
#conclusion: There is no missing value.

# Identifying Correlated Predictors
NumericData <- data[22:47]
numColN <- colnames(NumericData)
descrCor <-  cor(NumericData)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .90)
# values above 0.9 almost certainly point towards the presence of collinearity.
highCorr # conclusion: there is no highly correlated predictor
#removing descriptors with absolute correlations above 0.75.
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
numColN <- colnames(NumericData)
numColN[highlyCorDescr]
filteredDescr <- NumericData[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

#选variable：PCA


#conclusion from data analytics:
#1.factor variables numeric values will influence model. 
# model may think some levels the higher the better.
# solution:one hot encoding.
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
# Minimum Covariance Determinant (remove data with high MCD)
#5. Correlation:there are 3 variables highly (>0.75) correlated to others.
#solution: Greedy elimination. combine score, CPA, ridge...


###general data-preprocessing
# one-hot-encoding
dummies <- dummyVars(~ ., data = data[-1], fullRank = T)
dmfpredictors <- predict(dummies, newdata = data[-1])
T_D_data <- data.frame(data[1],dmfpredictors)
summary(T_D_data)

## for Decision Tree based model and naive bayes model, 
#you don't need to deal with the outliers
###split the data
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

# for models you need to deal with outliers. 
# Remove the outliers before splitting the data
# calculate the mahalanobis distance among 75% of the main data
output75 <- cov.mcd(T_D_data, quantile.used = nrow(T_D_data)*.75)

mhmcd75 <- mahalanobis(T_D_data, output75$center, output75$cov)

names_outlier_MCD75 <- which(mhmcd75 > cutoff)

excluded_mcd75 <- names_outlier_MCD75

data_clean_mcd <- T_D_data[-excluded_mcd75, ]


