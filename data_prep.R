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