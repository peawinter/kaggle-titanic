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


###############

training <- within(training, survived <- factor(as.factor(survived), labels = c('No', 'Yes')))
training <- within(training, pclass <- as.factor(pclass))

my_control <- trainControl(
  method='repeatedcv',
  number=5,
  repeats=5,
  savePredictions=TRUE,
  classProbs=TRUE,
  index=createResample(training$survived, 25),
  summaryFunction=twoClassSummary
)

model_list_1 <- caretList(
  survived ~ (pclass + age + title) ^ 2 + fare + cabinbin + embarked + sibspbin, data=training,
  trControl=my_control,
  metric='ROC',
  methodList=c('gbm', 'rpart', 'glmnet'),
  tuneList=list(
    rf=caretModelSpec(method='rf', ntree = 1501),
    svmRadial = caretModelSpec(method='svmRadial', proProc = c("center", "scale"), tuneLength = 8),
    svmLinear = caretModelSpec(method='svmLinear', tuneLength = 8), 
    nn=caretModelSpec(method='nnet', tuneLength=2, trace=FALSE)
  )
)

## compare different models
ggplot(model_list_1$gbm)

model_resamps_1 <- resamples(model_list_1)

bwplot(model_resamps_1, layout = c(3, 1))
splom(model_resamps_1)
model_difValues_1 <- diff(model_resamps_1)
summary(model_difValues_1)
trellis.par.set(caretTheme())
bwplot(model_difValues_1, layout = c(3, 1))
trellis.par.set(caretTheme())
dotplot(model_difValues_1)

sort(sapply(model_list_1, function(x) min(x$results$ROC)), decreasing = T)


modelCor(resamples(model_list))

p <- predict(model_list, newdata=training)
print(p)

greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

modelCor(resamples(model_list))

#### calibration

training_preds <- lapply(model_list_1, predict, newdata = training, type = 'prob')
training_preds <- lapply(training_preds, function(x) x[, 'Yes'])
training_preds <- data.frame(training_preds)

training_miss <- training[(training$survived == 'Yes') != (training_preds$rf > 0.5), ]

table(training_miss$sex, training_miss$survived)

training_miss[which(training_miss$survived=="No"),]

#### ensemble

greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

#### predict

testing <- within(testing, pclass <- as.factor(pclass))
testing_preds <- lapply(model_list_1, predict, newdata=testing, type='prob')
testing_preds <- lapply(testing_preds, function(x) x[,'Yes'])
testing_preds <- data.frame(testing_preds)

ens_preds <- predict(greedy_ensemble, newdata=testing)
model_preds$ensemble <- ens_preds
write.csv(data.frame(PassengerId = testing$passengerid, Survived = as.numeric(testing_preds$rf > 0.5)), file = "myRF_new.csv", row.names = F)

#################