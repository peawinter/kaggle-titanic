# Public score: 0.77990

rm(list = ls())
set.seed(1708)

library(ggplot2)
library(readr)
library(gridExtra)
library(caret)
library(plotROC)
library(bestglm)
library(glmnet)
library(pROC)

###### 
load('data/data_cleaned.RData')
## Data preprocessing
# remove unnecessary columns
dt <- within(dt, Embarked <- as.numeric(factor(as.factor(Embarked), labels = 1:3)))

test <- dt[is.na(dt$Survived), ]
train <- dt[!is.na(dt$Survived), ]

keep <- c('Sex', 'Age', 'SexAge', 'Pclass', 'Parch', 'SibSp', 'Fare', 'Embarked')
train_y <- train$Survived
train <- train[keep]
test_id <- test$PassengerId
test <- test[keep]

# subset of training set
val_idx <- sample(1:nrow(train), nrow(train) / 5)
trainAll <- train
trainAll_y <- train_y
valid <- train[val_idx, ]
valid_y <- train_y[val_idx]
train <- train[-val_idx, ]
train_y <- train_y[-val_idx]

## logistic regression

# benchmark
mylr1 <- glm(train_y ~ ., data = train, family = "binomial")
valid_pred_1 = predict(mylr1, newdata = valid, type="response") 
table(valid_y, valid_pred_1 > 0.5)

# valid_y FALSE TRUE
# 0    99   15
# 1    16   48

mylr2 <- glm(train_y ~ . + (Sex + Age + Pclass + SibSp + Fare)^2, data = train, family = "binomial")
valid_pred_2 = predict(mylr2, newdata = valid, type="response") 
table(valid_y, valid_pred_2 > 0.5)

# valid_y FALSE TRUE
# 0   101   13
# 1    18   46

mylr3 <- glm(train_y ~ Sex * Age * Pclass + SibSp + Fare + Embarked, data = train, family = "binomial")
valid_pred_3 = predict(mylr3, newdata = valid, type="response") 
table(valid_y, valid_pred_3 > 0.5)

# valid_y FALSE TRUE
# 0   108    6
# 1    17   47

## CV with logistic regression (using glmnet)
train <- within(train, Pclass <- as.numeric(Pclass))

Xy <- data.frame(train, y = train_y)
bestglm(Xy, family = binomial, IC = 'CV', t = 10)

# Best Model:
#   Estimate  Std. Error    z value     Pr(>|z|)
# (Intercept)  7.48929609 0.637594471  11.746175 7.388849e-32
# Sex         -2.53987077 0.209904323 -12.100136 1.054369e-33
# Age         -0.03600912 0.008394632  -4.289542 1.790423e-05
# Pclass      -1.14672261 0.130646814  -8.777272 1.674877e-18
# SibSp       -0.36817179 0.113422677  -3.246016 1.170324e-03

mylr4 <- glm(train_y ~ (Sex + as.factor(Pclass)) ^ 2 + SibSp + Age, data = train, family = "binomial")

valid_pred_4 = predict(mylr4, newdata = valid, type="response") 
table(valid_y, valid_pred_4 > 0.5)

valid_pred_df <- data.frame(m1 = valid_pred_1,
                            m2 = valid_pred_2,
                            m3 = valid_pred_3,
                            m4 = valid_pred_4,
                            y = valid_y)
valid_roc_df <- calculate_multi_roc(valid_pred_df, c("m1", "m2", 'm3', 'm4'), 'y')
valid_roc_plot <- multi_ggroc(valid_roc_df, label = c("m1", "m2", "m3", 'm4'))
plot_journal_roc(valid_roc_plot,
                 color = c('black', 'blue', 'green', 'red'),
                 legend = TRUE)

# calculate auc 
valid_roc_1 <- roc(valid_y, valid_pred_1)
valid_roc_2 <- roc(valid_y, valid_pred_2)
valid_roc_3 <- roc(valid_y, valid_pred_3)
valid_roc_4 <- roc(valid_y, valid_pred_4)

#####
##
# output into csv
mylr_final <- glm(trainAll_y ~ (Sex * Pclass * Age) + SibSp + Fare + Embarked, data = trainAll, family = "binomial")

mypred <- predict(mylr_final, newdata = test, type = "response")
output <- as.numeric(mypred > 0.5)
write.csv(data.frame(PassengerId = test_id, Survived = output), file = "myLR4.csv", row.names = F)
