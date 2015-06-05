#######
# new
# 1. add Fare3
# 2. add agebin

# clean data
rm(list = ls())

library(readr)
train <- read_csv("data/train.csv")
test  <- read_csv("data/test.csv")
test$Survived <- NaN
test_id <- test$PassengerId
dt <- rbind(train, test)

## Create binary feature matrix
# feature matrix
# title
title_list <- c('mr.', 'miss.', 'master.', 'mrs.', 'dr.', 'don.', 
                'rev.', 'mme.', 'ms.', 'major.', 'lady.', 'sir.', 
                'mlle.', 'col.', 'capt.', 'Jonkheer')
title_val <- c(4, 1, 2, 1, 3, 4, 4, 1, 1, 3, 1, 1, 1, 3, 3, 4)

names(title_val) <- title_list
for (idx in 1:nrow(dt)) {
  tmp <- strsplit(tolower(dt$Name[idx]), ' ')[[1]]
  for (i in 1:length(tmp)) {
    if (tmp[i] %in% title_list) {
      dt$Title[idx] = title_val[tmp[i]]
      break
    }
  }
  if (dt$Title[idx] == 'NaN')
    dt$Title[idx] = 1
}

keep <- c('PassengerId', 'Survived', 'Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked', 'Title')
dt <- dt[keep]

dt$Title <- as.factor(dt$Title)
dt <- cbind(dt, model.matrix( ~ 0 + Title, dt))

# Age NaN
age.mod <- lm(Age ~ Pclass + Sex +
                SibSp + Parch + Fare, data = dt)
fare.mod<- lm(Fare ~ Pclass + Sex +
                SibSp + Parch + Age, data = dt)

# Replace missing values in AGE and FARE with prediction
dt$Age[is.na(dt$Age)] <- predict(age.mod, dt)[is.na(dt$Age)]
dt$Fare[is.na(dt$Fare)] <- predict(fare.mod, dt)[is.na(dt$Fare)]

# Embarked
dt$Embarked[dt$Embarked == ""] <- "S"
dt$Embarked <- as.factor(dt$Embarked)
dt <- cbind(dt, model.matrix( ~ 0 + Embarked, dt))

# Fare
dt$Fare2 <- rep(0, nrow(dt))
dt$Fare2[dt$Fare <= 10] <- 1
dt$Fare2[dt$Fare > 10] <- 2
dt$Fare2[dt$Fare > 20] <- 3
dt$Fare2[dt$Fare > 30] <- 4
dt$Fare2 <- as.factor(dt$Fare2)
dt <- cbind(dt, model.matrix( ~ 0 + Fare2, dt))

dt$Fare3 <- rep(0, nrow(dt))
dt$Fare3[dt$Fare <= 7.75] <- 1
dt$Fare3[dt$Fare > 7.75] <- 2
dt$Fare3[dt$Fare > 26.0] <- 3
dt$Fare3 <- as.factor(dt$Fare3)
dt <- cbind(dt, model.matrix( ~ 0 + Fare3, dt))

# Parch
dt$Parch2 <- as.factor(as.numeric(dt$Parch > 3) + as.numeric(dt$Parch > 0))
dt$Parch <- as.numeric(dt$Parch > 0)
dt <- cbind(dt, model.matrix( ~ 0 + Parch2, dt)) 

# SibSp
dt$SibSp2 <- as.factor(as.numeric(dt$SibSp > 0) + as.numeric(dt$SibSp > 3))
dt$SibSp <- as.numeric(dt$SibSp > 0)
dt <- cbind(dt, model.matrix( ~ 0 + SibSp2, dt)) 

# Sex
dt <- within(dt, Sex <- as.numeric(factor(as.factor(Sex), labels = c(0, 1))) - 1)

# Pclass
dt$Pclass <- as.factor(dt$Pclass)
dt <- cbind(dt, model.matrix( ~ 0 + Pclass, dt))

# Sex-Age
dt$SexAge <- dt$Sex * dt$Age

# kids or not
dt$Kid <- as.numeric(dt$Age < 18)

# agebin
dt$AgeBin <- as.factor(as.numeric(dt$Age > 9) + as.numeric(dt$Age > 24) + as.numeric(dt$Age > 50))
dt <- cbind(dt, model.matrix( ~ 0 + AgeBin, dt))

#######
save(dt, file = 'data/data_cleaned_2.RData')