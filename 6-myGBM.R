## 0.76555

rm(list = ls())
set.seed(1708)

library(ggplot2)
library(readr)
library(e1071)
library(gbm)


#####
# load the data
load('data/data_cleaned.RData')
nokeep <- c("Pclass", "Embarked", "Title", "Title4", "EmbarkedC", 
            "Fare2", "Fare24", 'Pclass3', 'Parch22', 'SibSp22')
dt <- dt[ , -which(names(dt) %in% nokeep)]
# add sexage attribute
dt$SexAge <- dt$Sex * dt$Age

test <- dt[is.na(dt$Survived), ]
train <- dt[!is.na(dt$Survived), ]
test$Survived <- NULL
train$PassengerId <- NULL

# subset of training set
val_idx <- sample(1:nrow(train), nrow(train) / 5)
validSet <- train[val_idx, ]
trainSet <- train[-val_idx, ]

#####
# Create random forest based on PCLASS, SEX, FARE, AGE
gbm <- gbm(Survived ~ ., 
           data = trainSet, distribution = "adaboost", n.trees = 1000)

valid_pred <- predict(gbm, validSet, n.trees = 500)
table(validSet$Survived, valid_pred > 0)

#   FALSE TRUE
# 0   101   13
# 1    16   48

gbm2 <- gbm(Survived ~ ., 
            data = trainSet, distribution = "bernoulli", n.trees = 2000)

valid_pred_2 <- predict(gbm2, validSet, n.trees = 1000)
table(validSet$Survived, valid_pred_2 > 0)

# FALSE TRUE
# 0   101   13
# 1    13   51

# Make our prediction on the TRAIN data set [For calculating error]

gbm_final <- gbm(Survived ~ ., 
                 data = train, distribution = "bernoulli", n.trees = 2000)

mypred <- predict(gbm_final, test, n.trees = 1000)
output <- as.numeric(mypred > 0)

# output into csv
write.csv(data.frame(PassengerId = test$PassengerId, Survived = output), file = "myGBM.csv", row.names = F)
