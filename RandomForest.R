# packages need to be installed fist time only. After that disable all lines starting with 'install.packages' 
install.packages("caret")
install.packages("klaR")
install.packages("Rcpp")
install.packages("pROC")
install.packages("rJava")
install.packages("RWeka")
install.packages("gbm")
install.packages("randomForest")
install.packages("e1071")
install.packages("ROCR")


library(caret)
library(Rcpp)
library(pROC)
library(randomForest)


# 10 fold KNN classification with Caret package
cat("\014")  # command to clear the screen


# Load the datasets. ID column contains the class variable and it is the first column in the data set
# You need to change the code if class variable has different name and also the indices of the predictor variables
# where we used 'testData[,2:56]'. Your data sets might have different numbers of predictors
# ID column has been used in many places in the file. All occurences need to be replaced with your class variable name
originalData <- read.csv("DatasetTraining.csv", stringsAsFactor=TRUE, header=TRUE)
testData <- read.csv("DatasetTesting.csv", stringsAsFactor=TRUE, header=TRUE)

#list of all classifcation and regression methods supported by R
#names(getModelInfo())
#this grid must be used for svmRadialCost
grid <- expand.grid(mtry = (1:10)*5)
# 10 fold cross validation
# classProbs = TRUE option is used when metric="ROC"
train_control <- trainControl(method="repeatedcv", number=10, repeats=10,classProbs = TRUE, summaryFunction = twoClassSummary)


# train the model .  ID is the dependent variable name
model <- train(ID~., data=originalData, trControl=train_control, method="rf",metric="ROC",   tuneGrid = grid)
model

plot(model)
# make predictions using the predector variables. It is important to choose the right index here. Otherwise program will not work
# in this case, column 2 to 56 contains predictor variables
predictedResult <- predict(model, testData[,2:56])

# summarize results
# confusion metrix generate overall statistics and recall(sensitivity) and precision(Pos Pred Value) value for the positive class (True event)
# First encountered in the actual data set class will be treated as positive class unless explicitly specified
# specificity can be treated as the recall of the negative class, while Neg Pred Value can be treated as the precison of the
#negative class

CM<-confusionMatrix(predictedResult, originalData$ID) # first encountered class will be treated as positive class
CM

print("Precison of classes (in following order of classes:) and mean precison of all classes")
print(unique(testData$ID))
mean(unlist(precisionScore(as.numeric(testData$ID), as.numeric(predictedResult))))

print("Recall of classes (in following order of classes:) and mean recall of all classes")
print(unique(testData$ID))
mean(unlist(recallScore(as.numeric(testData$ID), as.numeric(predictedResult))))

print("F-measure of classes (in following order of classes:) and mean F-measure of all classes")
print(unique(testData$ID))
mean(unlist(F1Score(as.numeric(testData$ID), as.numeric(predictedResult))))
