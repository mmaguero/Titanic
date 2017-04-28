# set directory environment
setwd("C:\\Academico\\MII\\SIGE\\Prácticas\\P1\\data")
#getwd()

# data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Install and load required packages for decision trees and forests
library(rpart)
#install.packages('randomForest')
library(randomForest)
#install.packages('party')
library(party)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age NAs
summary(combi$Age)
# tree on the subset with the age values available and replace missing
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
#
which(combi$Embarked == '')
#
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
# 
which(is.na(combi$Fare))
# 
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

# Age Factor
combi$AgeClass = ifelse(combi$Age<=2,1,
                        ifelse(combi$Age>2 & combi$Age<=4,2,
                               ifelse(combi$Age>4 & combi$Age<=10,3,
                                      ifelse(combi$Age>10 & combi$Age<=20,4,
                                             ifelse(combi$Age>20 & combi$Age<=35,5,6)))))
combi$AgeClass = as.factor(combi$AgeClass)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# randomForest

# Build Random Forest Ensemble
set.seed(415)
#
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanic05-firstForest.csv", row.names = FALSE)
fit # Accuracy
### Not sent, because not are improvement the last submission (0.79426/0.79426)

# party

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
#
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanic05-CIForest.csv", row.names = FALSE)

### Your submission scored 0.81340, are improvement 0.79426

### Accuracy
PredictionTrain <- predict(fit, train, OOB=TRUE, type = "response")
intern <- data.frame(original = train$Survived, trained = PredictionTrain)

correct <- 0
for(i in 1:nrow(intern)){
  if(intern$original[i] == intern$trained[i]){
    print(i)
    correct <- correct + 1
  }
  
}
errorRate <- correct/nrow(intern)
print(paste("Accuracy: ", errorRate))
###

# Build condition inference tree Random Forest
set.seed(415)
# use Survived ~ Age + Sex + Pclass + FamilySize
fit <- cforest(as.factor(Survived) ~ Age + Sex + Pclass + FamilySize,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
#
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanic05-CIForestFamilySize.csv", row.names = FALSE)

### Your submission scored 0.76555, are not improvement 0.81340

### Accuracy
PredictionTrain <- predict(fit, train, OOB=TRUE, type = "response")
intern <- data.frame(original = train$Survived, trained = PredictionTrain)

correct <- 0
for(i in 1:nrow(intern)){
  if(intern$original[i] == intern$trained[i]){
    print(i)
    correct <- correct + 1
  }
  
}
errorRate <- correct/nrow(intern)
print(paste("Accuracy: ", errorRate))
###

# Build condition inference tree Random Forest
set.seed(415)
# use Survived ~ Age + Sex + Pclass + FamilySize
fit <- cforest(as.factor(Survived) ~ AgeClass + Sex + Pclass + FamilySize,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
#
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanic05-CIForestAgeClass.csv", row.names = FALSE)

### Your submission scored 0.76555, are not improvement 0.81340

### Accuracy
PredictionTrain <- predict(fit, train, OOB=TRUE, type = "response")
intern <- data.frame(original = train$Survived, trained = PredictionTrain)

correct <- 0
for(i in 1:nrow(intern)){
  if(intern$original[i] == intern$trained[i]){
    print(i)
    correct <- correct + 1
  }
  
}
errorRate <- correct/nrow(intern)
print(paste("Accuracy: ", errorRate))
###

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + AgeClass + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
#
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanic05-CIForest02.csv", row.names = FALSE)

### Your submission scored 0.81340, are improvement 0.79426

### Accuracy
PredictionTrain <- predict(fit, train, OOB=TRUE, type = "response")
intern <- data.frame(original = train$Survived, trained = PredictionTrain)

correct <- 0
for(i in 1:nrow(intern)){
  if(intern$original[i] == intern$trained[i]){
    print(i)
    correct <- correct + 1
  }
  
}
errorRate <- correct/nrow(intern)
print(paste("Accuracy: ", errorRate))
###

