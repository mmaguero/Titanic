# set directory environment
setwd("C:\\Academico\\MII\\SIGE\\Prácticas\\P1\\data")

# data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Look at gender patterns
summary(train$Sex)
# % per sex no survived / survived for total
prop.table(table(train$Sex, train$Survived))
# % per sex no survived / survived for sex
prop.table(table(train$Sex, train$Survived),1)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic02-Female.csv", row.names = FALSE)

### Your submission scored 0.76555

# Look at age patterns
summary(train$Age)

# add child variable 
train$Child <- 0
train$Child[train$Age < 18] <- 1

# number surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# total surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# % surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# what is happens if reduced the years in 4
# add child variable 
train$Child <- 0
train$Child[train$Age < 4] <- 1

# number surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# total surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# % surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive, except years old between two and four 
test$Survived[test$Sex == 'female'] <- 1
# Update the prediction to say that all male with less four years old will survive
test$Survived[test$Sex == 'male' & test$Age < 4] <- 1
test$Survived[test$Sex == 'female' & test$Age < 4] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic02-MaleMinor4.csv", row.names = FALSE)

### Your submission scored 0.77033, are improvement 0.76555

### Accuracy
train$SurvivedB <- 0
train$SurvivedB[train$Sex == 'female'] <- 1
train$SurvivedB[train$Sex == 'male' & train$Age < 4] <- 1
train$SurvivedB[train$Sex == 'female' & train$Age < 4] <- 0

intern <- data.frame(original = train$Survived, trained = train$SurvivedB)

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

# what is happens if reduced the years between 2 and 3.9
# add child variable 
train$Child <- 0
train$Child[test$Age > 1.9 & test$Age < 4] <- 1

# number surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# total surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# % surviver child per sex
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive, except years old between two and four 
test$Survived[test$Sex == 'female'] <- 1
# Update the prediction to say that all male with years old between two and four will survive
test$Survived[test$Sex == 'male' & (test$Age > 1.9 & test$Age < 4)] <- 1
test$Survived[test$Sex == 'female' & (test$Age > 1.9 & test$Age < 4)] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic02-MaleBetween2-4.csv", row.names = FALSE)

### Your submission scored 0.79426, are improvement 0.77033

### Accuracy
train$SurvivedB <- 0
train$SurvivedB[train$Sex == 'female'] <- 1
train$SurvivedB[train$Sex == 'male' & (train$Age > 1.9 & train$Age < 4)] <- 1
train$SurvivedB[train$Sex == 'female' & (train$Age > 1.9 & train$Age < 4)] <- 0
intern <- data.frame(original = train$Survived, trained = train$SurvivedB)

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

# Look at class and fare patterns
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
# Look at age patterns
train$Age2 <- '4+'
train$Age2[train$Age < 4 & train$Age >= 2] <- '2-4'
train$Age2[train$Age < 2] <- '<2'
# run a longer aggregate function
aggregate(Survived ~ Fare2 + Age2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic02-genderClass.csv", row.names = FALSE)

### Your submission scored 0.62..., are not improvement 0.79426 (ups, i choose incorrect csv file)
### Your submission scored 0.77990, are not improvement 0.79426

### Accuracy
train$SurvivedB <- 0
train$SurvivedB[train$Sex == 'female'] <- 1
train$SurvivedB[train$Sex == 'female' & train$Pclass == 3 & train$Fare >= 20] <- 0
intern <- data.frame(original = train$Survived, trained = train$SurvivedB)

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

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
# Update once more to say that females who pay fare more than 20 and age between 2-4 also perish
test$Survived[test$Sex == 'female' & (test$Age < 4 & test$Age >= 2) & test$Fare >= 20] <- 0

# Update once more to say that males who is ages old < 4 and class < 3 also will survive
test$Survived[test$Sex == 'male' & (test$Age < 4 & test$Age >= 2) & test$Pclass < 3] <- 1
# Update once more to say that males who is ages old < 2 and class = 3 and paid fare < 30 also will survive
test$Survived[test$Sex == 'male' & test$Age < 2 & test$Fare < 30 & test$Pclass == 3] <- 1
# Update once more to say that males who is ages old between 2-4 and class = 3 and paid fare < 30 also will survive
test$Survived[test$Sex == 'male' & (test$Age < 4 & test$Age >= 2) & test$Fare < 20 & test$Pclass == 3] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic02-genderClassAge.csv", row.names = FALSE)

### Your submission scored 0.77990, are not improvement 0.79426

### Accuracy
train$SurvivedB <- 0
# Update the prediction to say that all females will survive
train$SurvivedB[train$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
train$SurvivedB[train$Sex == 'female' & train$Pclass == 3 & train$Fare >= 20] <- 0
# Update once more to say that females who pay fare more than 20 and age between 2-4 also perish
train$SurvivedB[train$Sex == 'female' & (train$Age < 4 & train$Age >= 2) & train$Fare >= 20] <- 0

# Update once more to say that males who is ages old < 4 and class < 3 also will survive
train$SurvivedB[train$Sex == 'male' & (train$Age < 4 & train$Age >= 2) & train$Pclass < 3] <- 1
# Update once more to say that males who is ages old < 2 and class = 3 and paid fare < 30 also will survive
train$SurvivedB[train$Sex == 'male' & train$Age < 2 & train$Fare < 30 & train$Pclass == 3] <- 1
# Update once more to say that males who is ages old between 2-4 and class = 3 and paid fare < 30 also will survive
train$SurvivedB[train$Sex == 'male' & (train$Age < 4 & train$Age >= 2) & train$Fare < 20 & train$Pclass == 3] <- 1
intern <- data.frame(original = train$Survived, trained = train$SurvivedB)

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
