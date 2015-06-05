# Public Score: 0.76077

rm(list=ls())
set.seed(1708)
library(ada)

#####
# load the data
load('data/data_cleaned.RData')
nokeep <- c("Pclass", "Embarked", "Title", "Title4", "EmbarkedC", 
            "Fare2", "Fare24", 'Pclass3', 'Parch22', 'SibSp22')
dt <- dt[ , -which(names(dt) %in% nokeep)]

## subset data
test <- dt[is.na(dt$Survived), ]
train <- dt[!is.na(dt$Survived), ]
test$Survived <- NULL
train$PassengerId <- NULL
# subset of training set
val_idx <- sample(1:nrow(train), nrow(train) / 5)
validSet <- train[val_idx, ]
trainSet <- train[-val_idx, ]

# run the model on the training data
adamod <- ada(Survived ~ ., data = trainSet, verbose = TRUE, na.action=na.rpart)

# ...and predict the test data
adamod <- addtest(adamod, test.x = validSet[,-1], test.y = validSet[,1])

valid_pred <- predict(adamod, validSet)

table(validSet$Survived, valid_pred)
# valid_pred
# 0   1
# 0 105   9
# 1  19  45

# error reduction for progressive splits
plot(adamod, test=TRUE)

# variable importance plotting
varplot(adamod)

#######
# out-of-sample predictions on the test data set
adamod <- ada(Survived ~ ., data = train, verbose = TRUE, na.action=na.rpart)

output <- predict(adamod, newdata = test, type="vector")

write.csv(data.frame(PassengerId = test$PassengerId, Survived = output), file = "myBoost.csv", row.names = F)