###########################
#
# Ensemble SVM, GBM, Logistic, NB, randomforest
#
# 0.80383

rm(list = ls())

library(caret)
library(caretEnsemble)
library(pROC)
library(rpart)
library(randomForest)
library(gbm)
library(plyr)
library(e1071)
library(kernlab)
library(caTools)

set.seed(1708)

load('data/data_cleaned_2.RData')

###### 
## Subset data
# clear

keep <- c("PassengerId", "Age", "Survived", "Pclass", "Sex", "Fare", "Embarked", "Title", "Fare3", 'AgeBin')

dt <- dt[keep]
testing <- dt[is.na(dt$Survived), ]
training <- dt[!is.na(dt$Survived), ]
testing$Survived <- NULL
training$PassengerId <- NULL
training <- within(training, Survived <- factor(as.factor(Survived), labels = c('No', 'Yes')))

my_control <- trainControl(
  method='repeatedcv',
  number=5,
  repeats=5,
  savePredictions=TRUE,
  classProbs=TRUE,
  summaryFunction=twoClassSummary
)

model_list <- caretList(
  Survived ~. + Pclass:Sex + Pclass:Age + Age:Sex, data=training,
  trControl=my_control,
  metric='ROC',
  methodList=c('rf', 'gbm', 'svmRadial', 'rpart', 'glmnet'),
  tuneList=list(
    rf1=caretModelSpec(method='rf', tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method='rf', tuneGrid=data.frame(.mtry=10), preProcess='pca'),
    nn=caretModelSpec(method='nnet', tuneLength=2, trace=FALSE)
  )
)

model_list

modelCor(resamples(model_list))

predict(model_list, testing)

p <- predict(model_list, newdata=training)
print(p)

greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

####

model_preds <- lapply(model_list, predict, newdata=testing, type='prob')
model_preds <- lapply(model_preds, function(x) x[,'Yes'])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing)
model_preds$ensemble <- ens_preds

write.csv(data.frame(PassengerId = testing$PassengerId, Survived = as.numeric(ens_preds > 0.5)), file = "myEnsemble.csv", row.names = F)
