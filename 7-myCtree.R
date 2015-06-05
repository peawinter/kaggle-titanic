## public score 0.78947
## public score 0.79426

rm(list = ls())
set.seed(1708)

library(ggplot2)
library(e1071)
library(randomForest)

load('data/data_cleaned_2.RData')

###### 
## Subset data
# clear
keep <- c("PassengerId", "Survived", "Pclass", "Sex", "SibSp", "Age", "Parch", "Fare", "Embarked", "Title", "Fare2", 'Kid')

dt <- dt[keep]
test <- dt[is.na(dt$Survived), ]
train <- dt[!is.na(dt$Survived), ]
test$Survived <- NULL
train$PassengerId <- NULL

######
## random forest
val_idx <- sample(1:nrow(train), nrow(train) / 5)
validSet <- train[val_idx, ]
trainSet <- train[-val_idx, ]

rfc <- randomForest(as.factor(Survived) ~ ., data = trainSet, importance = T, ntree = 4000)
varImpPlot(rfc)
valid_pred <- predict(rfc, validSet)
table(validSet$Survived, valid_pred)

# valid_pred
# 0   1
# 0 102  12
# 1  16  48

## add Kid
# valid_pred
# 0   1
# 0 104  10
# 1  18  46

# valid_pred
# 0   1
# 0 105   9
# 1  18  46

rfc <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Embarked + Title + Fare + Pclass:Sex + Pclass:Age + Age:Sex,
                    data = trainSet, importance = T, ntree = 4000)
varImpPlot(rfc)
valid_pred <- predict(rfc, validSet)
table(validSet$Survived, valid_pred)

# valid_pred
# 0   1
# 0 106   8
# 1  17  47

######
## submit
# subset of training set
rfc <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Embarked + Title + Fare + Pclass:Sex + Pclass:Age + Age:Sex, data = train, importance = T, ntree = 4000)
test_pred <- predict(rfc, test)
write.csv(data.frame(PassengerId = test$PassengerId, Survived = test_pred), file = "myRFC.csv", row.names = F)
