rm(list = ls())

library(RCurl)
library(caret)
library(ggplot2)
library(pROC)
library(ROCR)

set.seed(1708)

load(url("http://topepo.github.io/caret/descr.RData"))
load(url("http://topepo.github.io/caret/mutagen.RData"))

#
inTrain <- createDataPartition(mutagen, p = 0.75, list = FALSE)

training <- descr[inTrain, ]
testing <- descr[-inTrain, ]

#
training_class <- mutagen[inTrain]
testing_class <- mutagen[-inTrain]

#
table(training_class)
table(testing_class)

# 
ncol(training)
training_cor <- cor(training)
training_cor[is.na(training_cor)] <- 0.0
high_cor <- findCorrelation(training_cor, 0.90)
length(high_cor)
training <- training[, -high_cor]
testing <- testing[, -high_cor]

#
myControl <- trainControl(number = 10)
mySVM <- train(training, training_class,
               method = 'svmRadial',
               tuneLength = 5,
               trControl = myControl,
               scaled = FALSE)

gbmGrid <- expand.grid(n.trees = (1:5) * 50,
                       interaction.depth = 2:5,
                       shrinkage = .1, 
                       n.minobsinnode = 10)

myGBM <- train(training, training_class,
               method = 'gbm', 
               trControl = myControl,
               bag.fraction = 0.5,
               tuneGrid = gbmGrid
               )

plot(myGBM)
plot(myGBM, metric = 'Kappa')
plot(myGBM, plotType = 'level')

#

predict(myGBM, newdata = testing)[1:10]

#

myList <- list(svm = mySVM, gbm = myGBM)
predList <- predict(myList, newdata = testing)

lapply(predList, function(x) x[1:5])

# 

myPredValues <- extractPrediction(myList, testing, testing_class)
myTestValues <- subset(myPredValues, dataType == 'Test')

table(myTestValues$model)

#

myList2 <- list(svm = myGBM)

myProbValues <- extractProb(myList2, testX = testing, testY = testing_class)
myProbValues <- subset(myProbValues, dataType == 'Test')

plotClassProbs(myProbValues)

ggplot(myProbValues, aes(x = mutagen, fill = as.factor(obs))) +
  geom_histogram(binwidth = 0.05, alpha = 0.5, position = 'identity') +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="mutagen: ") +
  ggtitle("Prediction probability")

ggplot(myProbValues, aes(x = mutagen, fill = as.factor(obs))) +
  geom_density(alpha = 0.3) +
  ggtitle("Prediction probability")

myGBMpred <- subset(myPredValues, model == 'gbm')
myGBMprob <- subset(myProbValues, model == 'gbm')

confusionMatrix(myGBMpred$pred, myGBMpred$obs)

pred <- prediction(myProbValues$mutagen, myProbValues$obs)

perf <- performance(pred, 'tpr', 'fpr')

plot(perf, col = 'black')

plotObsVsPred(myPredValues)

#

myGBMimp <- varImp(myGBM)

myGBMimp

plot(myGBMimp, top = 20)
