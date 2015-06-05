# Public score: 0.77033

# 

rm(list = ls())
set.seed(1708)

library(ggplot2)
library(e1071)

load('data/data_cleaned_2.RData')

###### 
## Subset data
# clear
keep <- c("PassengerId", "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch2", "Fare", "Embarked",
          "Title", "Fare3", "AgeBin")
# nokeep <- c("Pclass", "Fare", "Embarked", "Title", "Title5", "EmbarkedC", "Fare2", "Fare24",'Pclass3')
dt <- dt[keep]
test <- dt[is.na(dt$Survived), ]
train <- dt[!is.na(dt$Survived), ]
test$Survived <- NULL
train$PassengerId <- NULL
# subset of training set
val_idx <- sample(1:nrow(train), nrow(train) / 5)
validSet <- train[val_idx, ]
trainSet <- train[-val_idx, ]

#######
## SVM 
formula <- Survived ~ .

tune.out = tune(svm, formula, data = trainSet, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.005, 0.008, 0.01, 0.1, 1, 5)))
summary(tune.out)

# Parameter tuning of ‘svm’:
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.01
# 
# - best performance: 0.1814564 
# 
# - Detailed performance results:
#   cost     error dispersion
# 1 0.001 0.1969136 0.02097848
# 2 0.010 0.1814564 0.03338429
# 3 0.100 0.1852049 0.02850814
# 4 1.000 0.1842704 0.03061737
# 5 5.000 0.1842456 0.03064681

bestmod = tune.out$best.model
summary(bestmod)

# Call:
#   best.tune(method = svm, train.x = Survived ~ ., data = trainSet, ranges = list(cost = c(0.001, 
#                                                                                           0.01, 0.1, 1, 5)), kernel = "linear")
# 
# 
# Parameters:
#   SVM-Type:  eps-regression 
# SVM-Kernel:  linear 
# cost:  0.01 
# gamma:  0.05882353 
# epsilon:  0.1 
# 
# 
# Number of Support Vectors:  397

validSet_svm_pred = predict(bestmod, validSet)

table(validSet$Survived, validSet_svm_pred > 0.5)

# FALSE TRUE
# 0   101   13
# 1    16   48

#######
# kernal = 'radical'

formula <- Survived ~ Sex * AgeBin + Fare3 + SibSp + Parch2 + Pclass + Embarked

tune.out = tune(svm, formula, data = trainSet, kernel = "radial", 
                ranges = list(cost = c(0.001, 0.005, 0.008, 0.01, 0.1, 1, 5)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

validSet_svm_pred = predict(bestmod, validSet)

table(validSet$Survived, validSet_svm_pred > 0.5)

# FALSE TRUE
# 0   106    8
# 1    17   47

########
# submit
tune.out = tune(svm, Survived ~ ., data = train, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.008, 0.01, 0.02, 0.03)))
summary(tune.out)

# Parameter tuning of ‘svm’:
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.008
# 
# - best performance: 0.1728182 
# 
# - Detailed performance results:
#   cost     error dispersion
# 1 0.001 0.1738797 0.01488730
# 2 0.008 0.1728182 0.02914144
# 3 0.010 0.1728833 0.02912157
# 4 0.020 0.1733667 0.02906990
# 5 0.030 0.1740848 0.02911781

bestmod = tune.out$best.model
summary(bestmod)

# Call:
#   best.tune(method = svm, train.x = Survived ~ ., data = train, ranges = list(cost = c(0.001, 
#                                                                                        0.008, 0.01, 0.02, 0.03)), kernel = "linear")
# 
# 
# Parameters:
#   SVM-Type:  eps-regression 
# SVM-Kernel:  linear 
# cost:  0.008 
# gamma:  0.05882353 
# epsilon:  0.1 
# 
# 
# Number of Support Vectors:  488

mypred <- predict(bestmod, test)
output <- as.numeric(mypred > 0.5)

# output into csv
write.csv(data.frame(PassengerId = test$PassengerId, Survived = output), file = "mySVM.csv", row.names = F)


