# ensemble model 

rm(list = ls())

library(caret)
library(caretEnsemble)
library(mlbench)
library(pROC)
library(rpart)
library(gbm)
library(plyr)
library(e1071)
library(kernlab)

set.seed(1708)

load('data/data_cleaned_2.RData')

###### 
## Subset data
# clear
keep <- c("PassengerId", "Survived", "Pclass", "Sex", "SibSp", "Age", "Parch", "Fare", "Embarked", "Title", "Fare2", "Fare3", 'Kid', 'AgeBin')

dt <- dt[keep]
testing <- dt[is.na(dt$Survived), ]
training <- dt[!is.na(dt$Survived), ]
testing$Survived <- NULL
training$PassengerId <- NULL
training$Survived <- as.factor(training$Survived)

# subset of training set
valid_idx <- sample(1:nrow(training), nrow(training) / 5)
valid <- training[valid_idx, ]
training <- training[-valid_idx, ]

########
# training model by gbm
my_control <- trainControl(
  method='repeatedcv',
  number=5,
  repeats=5,
  savePredictions=TRUE,
  classProbs=TRUE,
  # index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)

myGrid <- expand.grid(interaction.depth = c(2, 3, 4, 6, 8),
                      n.trees = (1:10) * 50,
                      shrinkage = 0.1, 
                      n.minobsinnode = 10)

gbmFit1 <- train(Survived ~ . + Sex:Age + Sex:Pclass, data = training,
                 method = "gbm",
                 trControl = my_control,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 tuneGrid = myGrid)
gbmFit1

# plot training curve
trellis.par.set(caretTheme())
ggplot(gbmFit1)

# predict valid data
valid_gbm_pred <- predict(gbmFit1, newdata = valid)
table(valid$Survived, valid_gbm_pred)

# valid_pred
# 0   1
# 0 103  11
# 1  18  46

gbmFit2 <- train(Survived ~ . + Sex * AgeBin + Sex * Pclass, data = training,
                 method = "gbm",
                 trControl = my_control,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 tuneGrid = myGrid)
gbmFit2

# plot training curve
trellis.par.set(caretTheme())
ggplot(gbmFit2)

# predict valid data
valid_gbm_pred_2 <- predict(gbmFit2, newdata = valid)
table(valid$Survived, valid_gbm_pred_2)

# valid_gbm_pred_2
# 0   1
# 0 103  11
# 1  17  47

########
## svm

my_control <- trainControl(
  method='repeatedcv',
  number=10,
  repeats=10,
  savePredictions=TRUE,
  classProbs=TRUE,
  # index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)

svmFit1 <- train(Survived ~ .+ Sex:Age + Sex:Pclass, data = training,
                 method = "svmRadial",
                 trControl = my_control,
                 proProc = c("center", "scale"),
                 tuneLength = 8,
                 verbose = FALSE)
svmFit1

# plot training curve
trellis.par.set(caretTheme())
ggplot(svmFit1)

# predict valid data
valid_svm_pred <- predict(svmFit1, newdata = valid)
table(valid$Survived, valid_svm_pred)

# valid_svm_pred
# 0   1
# 0 104  10
# 1  19  45

svmFit2 <- train(Survived ~ Sex * AgeBin + Fare3 + SibSp + Parch + Pclass + Embarked, data = training,
# svmFit2 <- train(Survived ~ . + Sex * AgeBin, data = training,
                 method = "svmRadial",
                 trControl = my_control,
                 proProc = c("center", "scale"),
                 tuneLength = 8,
                 verbose = FALSE)
svmFit2

# plot training curve
trellis.par.set(caretTheme())
ggplot(svmFit2)

# predict valid data
valid_svm_pred <- predict(svmFit2, newdata = valid)
table(valid$Survived, valid_svm_pred)

# valid_svm_pred
# 0   1
# 0 106  8
# 1  19  45

svmFit3 <- train(Survived ~ Sex * AgeBin + Fare3 + SibSp + Parch + Pclass + Embarked, data = training,
                 method = "svmLinear",
                 trControl = my_control,
                 proProc = c("center", "scale"),
                 tuneLength = 8,
                 verbose = FALSE)
# svmFit3

# plot training curve
trellis.par.set(caretTheme())
ggplot(svmFit3)

# predict valid data
valid_svm_pred <- predict(svmFit3, newdata = valid)
table(valid$Survived, valid_svm_pred)

######
# rda
rdaFit <- train(Survived ~ .+ Sex:Age + Sex:Pclass, data = training,
                method = "rda",
                trControl = my_control,
                tuneLength = 4,
                metric = "ROC")
rdaFit

# plot training curve
trellis.par.set(caretTheme())
ggplot(rdaFit)

# predict valid data
valid_rda_pred <- predict(rdaFit, newdata = valid)
table(valid$Survived, valid_rda_pred)

# valid_rda_pred
# 0   1
# 0 101  13
# 1  21  43

rdaFit2 <- train(Survived ~ Sex * AgeBin + Fare3 + SibSp + Parch + Pclass + Embarked, data = training,
                method = "rda",
                trControl = my_control,
                tuneLength = 4,
                metric = "ROC")
rdaFit2

# plot training curve
trellis.par.set(caretTheme())
ggplot(rdaFit2)

# predict valid data
valid_rda_pred_2 <- predict(rdaFit2, newdata = valid)
table(valid$Survived, valid_rda_pred_2)

######
# rpart

my_control <- trainControl(
  method='repeatedcv',
  number=5,
  repeats=3,
  savePredictions=TRUE,
  classProbs=TRUE,
  # index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)

treeFit <- train(Survived ~ .+ Sex:Age + Sex:Pclass, data = training,
                method = "rpart",
                trControl = my_control,
                tuneLength = 30,
                metric = "ROC")
treeFit

# plot training curve
trellis.par.set(caretTheme())
ggplot(treeFit)

# predict valid data
valid_rda_pred <- predict(rdaFit, newdata = valid)
table(valid$Survived, valid_rda_pred)

# valid_rda_pred
# 0   1
# 0 101  13
# 1  21  43

######
# model comparison

resamps <- resamples(list(GBM = gbmFit1,
                          SVM = svmFit2,
                          RDA = rdaFit))
resamps

summary(resamps)

trellis.par.set(caretTheme())
bwplot(resamps, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")

splom(resamps)


