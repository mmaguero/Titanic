# set directory environment
setwd("C:\\Academico\\MII\\SIGE\\Prácticas\\P1\\data")

# data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# train data types
str(train)
# test data types
str(test)

# See the number of survived
table(train$Survived)
# %
prop.table(table(train$Survived))

# add new column in test set with value 0 (everyone dies)
test$Survived <- rep(0, 418)

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic01.csv", row.names = FALSE)

### Accuracy
train$SurvivedB <- 0
train$SurvivedB[train$Sex == 'female'] <- 1
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
