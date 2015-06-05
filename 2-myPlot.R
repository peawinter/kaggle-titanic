rm(list = ls())
set.seed(1708)

library(ggplot2)
library(readr)

train <- read_csv("train.csv")
test  <- read_csv("test.csv")

## Plot
# histogram by age
ggplot(train, aes(x = Age, fill = as.factor(Survived))) +
  geom_bar(binwidth = 5, position = 'fill') +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ") +
  ggtitle("Survived by Age")

# histogram by sex
ggplot(train, aes(x = Sex, fill = as.factor(Survived))) +
  geom_bar() +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ")

ggplot(train, aes(x = Fare, fill = as.factor(Survived))) + 
  geom_bar(binwidth = 5) +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ")

# histogram by age and sex and class
ggplot(train) +
  geom_histogram(aes(x=Age, fill = factor(Survived)), binwidth = 5, alpha=1, position = 'fill') +
  facet_wrap(~ Sex + Pclass) +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ")

# histogram by sibsp
ggplot(train, aes(x = SibSp, fill = as.factor(Survived))) + 
  geom_bar(binwidth = 1, position = 'Fill') +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ")

# histogram by parch
ggplot(train, aes(x = Parch, fill = as.factor(Survived))) + 
  geom_bar(binwidth = 1, position = 'Fill') +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ")

# histogram by embarked port
ggplot(train, aes(x = Embarked, fill = as.factor(Survived))) + 
  geom_bar(binwidth = 1) +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ")

# extract title
title_list <- c('mr.', 'miss.', 'master.', 'mrs.', 'dr.', 'don.', 'rev.', 'mme.', 'ms.', 'major.', 'lady.', 'sir.', 'mlle.', 'col.', 'capt.')
train$Title <- rep(NaN, nrow(train))
for (idx in 1:nrow(train)) {
  tmp <- strsplit(tolower(train$Name[idx]), ' ')[[1]]
  for (i in 1:length(tmp)) {
    if (tmp[i] %in% title_list) {
      train$Title[idx] = tmp[i]
      break
    }
  }
  if (train$Title[idx] == 'NaN')
    print(idx)
}

for (idx in 1:nrow(test)) {
  tmp <- strsplit(tolower(test$Name[idx]), ' ')[[1]]
  for (i in 1:length(tmp)) {
    if (tmp[i] %in% title_list) {
      test$Title[idx] = tmp[i]
      break
    }
  }
  if (test$Title[idx] == 'NaN')
    print(idx)
}

ggplot(train, aes(x = Title, fill = as.factor(Survived))) + 
  geom_bar(binwidth = 1, position='Fill') +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Survived: ") +
  ggtitle("Survived by Title")