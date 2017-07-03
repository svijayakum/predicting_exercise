---
title: "Manner of Exercise"
author: "Swathi Vijayakumar"
date: "January 28, 2016"
output: html_document
---


## Purpose and background 
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement inorder to improve health and find patterns in behavior. For this project we will use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. The goal of your project is to predict the manner in which they did the exercise.
    
    
## Loading training data, testing Data and packages

```{r}
train_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
HAR_Train = read.csv(url(train_url))
HAR_Test = read.csv(url(test_url))
dim(HAR_Train)
dim(HAR_Test)
library(caret); library(rattle); library(randomForest)
```



## Data clean-up and transformation

### Step 1: Remove columns with execisive number of missing values. 

When you look at a summary of the data, it's clear that for some of the measurement columns, almost all of the data is missing. Count the number of missing values each column and remove all the columns with almost 98% of the values missing.

```{r}
na_countTrain = sapply(HAR_Train, function(y) sum(length(which(is.na(y)))))
na_countTrain = data.frame(na_countTrain)

HAR_Train = HAR_Train [,colSums(is.na(HAR_Train)) < 19216]
dim(HAR_Train)

names = colnames(HAR_Train)
HAR_Test = HAR_Test[,names(HAR_Test) %in% names]
dim(HAR_Test)
```

### Step 2: Remove the first 5 column and near zero covariates


```{r}
nzv = nearZeroVar(HAR_Train)
nzv

HAR_Train = HAR_Train[,-nzv]  
HAR_Test = HAR_Test[,-nzv]  

HAR_Train = HAR_Train[,-(1:5)]  
HAR_Test = HAR_Test[,-(1:5)]  

dim(HAR_Train)
dim(HAR_Test)
```

## Model Building

### Split the training data into training and validation sets.  Since the the typical split for a training and a "testing" data set are 80%, 20% respectively that's how we'll split the data.  

```{r}
set.seed(230)
inTrain = createDataPartition(y=HAR_Train$classe, p=0.8, list=FALSE)
training = HAR_Train[inTrain,]
validation = HAR_Train[-inTrain,]
dim(training)
dim(validation)
```



### Predicting with Trees

We'll strart with building a decision tree on the training data set followed by predicting classe in the validation data set using this model.

```{r}
fitTree = train(classe~. ,data = training, method = "rpart")
print(fitTree)
print(fitTree$finalModel, digits = 3)

fancyRpartPlot(fitTree$finalModel)

predTree = predict(fitTree, validation)


matrixTree = confusionMatrix(predTree, validation$classe)
matrixTree

plot(matrixTree$table, col = matrixTree$byClass, main = paste("Decision Tree Confusion Matrix: Accuracy =", round(matrixTree$overall['Accuracy'], 3)))

```

The decision tree model used bootstrapped resampling with 25 reps.  No model pre-processing was performed. Unfortunetly using a decision tree gives us a very poor accuracy (only 50%) when predicting in the validation set. Not the best model. 

### Predicting with Random Forest

We will next build a random forest model on the testing data set and predict classe in the validation data set using this model.

```{r}
fitrf = randomForest(classe~. ,data = training)

print(fitrf)

predrf = predict(fitrf, validation)

matrixRF = confusionMatrix(predrf, validation$classe)

plot(fitrf, main = paste("RandomForestModelFit"))

plot(matrixRF$table, col = matrixRF$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(matrixRF$overall['Accuracy'], 2)))
```

For this dataset Random forest uses calssification with 500 trees and 7 variables at each split. Predicting on the validation set gives us a very good accuracy (99.7%). The out of sample error is 1-0.997 = 0.003. This is a good out of sample error. We will continue using the random forest model to predict on the testing set.  

## Use Random forest to make a prediction on the test set.

The final step is to run the final prediction on the 20 test set data points using the final Random Forest model. 

```{r}
predrfTest = predict(fitrf, HAR_Test)
predrfTest
```

