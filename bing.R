surv <- train[,2]
fare <- train[,10]
cab <- train[,11]
gender <- train$Sex
age
cab
sum(cab=="")
train[which((cab!="")),c(2,11)]
head(train)

attach(train)

count_gender <- table(surv, gender)
count_gender
count_cab_f <- table(surv[gender=="female"], cab[gender=="female"])
count_cab_f

count_cab_m <- table(surv[(gender=="male")][Age<10], cab[gender=="male"][Age<10])
count_cab_m

### age < 16
### age  16 - 50
### age >50
under_16 <- Age<=8
between_16_50 <- Age %in% 25:50
elder <- Age >50

count_young <- table(surv[under_16], gender[under_16])
count_young


count_between <- table(surv[between_16_50], gender[between_16_50])
count_between

count_elder <- table(surv[elder], gender[elder])
count_elder

agecount <- table(surv[gender=="female"], Age[gender=="female"])

agecount
train[,10:11]
head(train, 100)
22