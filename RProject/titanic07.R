#Read the data

library(caret)

readData <- function(path.name, file.name, column.types, missing.types){
  read.csv(paste(path.name, file.name, sep=''), colClasses = column.types, na.strings = missing.types )
}
#Titanic.path <- './data/'
Titanic.path <- 'C:\\Academico\\MII\\SIGE\\Prácticas\\P1\\data\\'
train.file.name <- 'train.csv'
test.file.name <- 'test.csv'
missing.types <- c('NA', '')
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]

train <- readData(Titanic.path, train.file.name, train.column.types, missing.types)
test <- readData(Titanic.path, test.file.name, test.column.types, missing.types)
test$Survived <- NA # add Survived column to test dataset and fill it out with NA.

combi <- rbind(train, test) # combine training and test set for further manipulation

# Data Clearing and Feature Engineering
# Feature Engineering
# Name. Extract the title and Surname from Name and create two new features.
title.extract <- function(x){
  strsplit(x, split = "[,.]")[[1]][2]
}
combi$Title <- sapply(combi$Name, FUN = title.extract)
combi$Title <- sub(" ", "", combi$Title) # delete the space in the Title

# combine the similiar into the same category
combi$Title[combi$PassengerId == 797] <- 'Mrs' # this passenger is a female doctor
combi$Title[combi$Title %in% c('Mlle', 'Mme')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'lady'
combi$Title <- as.factor(combi$Title)

combi$Surname <- sapply(combi$Name, FUN = function(x){strsplit(x, split="[,.]")[[1]][1]})

# SibSp and Parch. Create a new feature FamilySize which equals SibSq + Parch +1

combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Base on the two features FamilySize and Surname, we create a new feature FamilyID in order 
# to classify the passengers from the same family into groups. As there are many single passenger 
# whose family is 1, we label these passengers whose FamilySize <=2 as Small. 
# Note there may be some records whose FamilySize doesn't equal the frequence of FamilyID, it's 
# obvious inconformity exists. So we labeled the records where frequence of FamilyID <=2 as Small as well.
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
combi$FamilyID[combi$FamilySize <= 2] <- "Small"
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,] 
# if famIDs frequency <=2, regard it as "Small" as well. 
combi$FamilyID[combi$FamilyID %in% famIDs$Var1 ] <- "Small"
combi$FamilyID <- as.factor(combi$FamilyID)

# For Cabin, we can extract the first letter of Cabin as a label. It's obviouse the Cabin numbers 
# correspond different Passenger Class which is a crucial factor determining whether one could survive. 
# However, since the Cabin of most records are missing, although the importance of this variable seems
# to be significant in training, it doesn't increase the predicting score in my models.

# We write a function to extract the first letter of Cabin. A new factor 'N' is assigned to the 
# missing values. Also, for the passengers with the same ticket number, the most frequent Cabin level 
# besides 'N' is assigned to the rest passengers.
extractCabin <- function(combi){
  # extract the first letter of Cabin
  combi$Cabin <- sapply(combi$Cabin, FUN = function(x){strsplit(x, split='')[[1]][1]})
  combi$Cabin[is.na(combi$Cabin)] <- 'N'
  combi$Cabin <- as.factor(combi$Cabin)
  
  # set the same number tickets with the same Cabin label
  combi.ticket <- table(factor(combi$Ticket))
  combi.ticket.moreThanOne <- combi.ticket[combi.ticket>1]
  combi.temp <- combi[combi$Ticket %in% names(combi.ticket.moreThanOne), ]
  for(name in names(combi.ticket.moreThanOne)){
    row.sameTicket <- combi[combi$Ticket == name, ]
    Cabin_boolean <- row.sameTicket$Cabin %in% c('A','B','C','D','E','F','G')
    if(sum(Cabin_boolean) > 0){
      correctCabin <- names(sort(table(row.sameTicket$Cabin[Cabin_boolean]), decreasing=TRUE))[1]
      row.sameTicket$Cabin[row.sameTicket$Cabin == "N"] <- correctCabin
      # modify the Cabin of combi dataset
      combi$Cabin[row.sameTicket$PassengerId] <- row.sameTicket$Cabin
    }
  }
  combi$Cabin <- as.factor(combi$Cabin)
  return(combi)
}
combi <- extractCabin(combi)

# For Ticket, we extracted the first alphabet character of Ticket to label the ticket.
extractTicket <- function(ticket){
  pattern <- c('\\/', '\\.', '\\s', '[[:digit:]]')
  for (p in pattern){
    # replace all chracter matches the pattern p with ""
    ticket <- gsub(p, "", ticket)
  }
  ticket <- substr(toupper(ticket), 1,1) # only extract the first alphabet character to label the ticket
  ticket[ticket==""] <- 'N'
  ticket <- as.factor(ticket)
}
combi$Ticket <- extractTicket(combi$Ticket)

# Dealing with missing values

# Fare. Replace the NA in Fare with the median of Fare.

combi$Fare[is.na(combi$Fare)] <- median(combi$Fare, na.rm = TRUE)

# Embarked. Replase the NA in Embarked with the most frequent label S.

combi$Embarked[is.na(combi$Embarked)] <- "S"

# Age. We fill out the NA in Age by fitting a decision tree, which uses the features exclude Survived 
# to predict Age. Here we just use the default parameters of rpart, for more details, check rpart.control.

library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data = combi[!is.na(combi$Age), ], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age), ])

# Fitting Models

# Write a function to extract features from dataset. As the dataset is small and many missing values 
# in Cabin and Ticket, these two features actually don't increase the predicting score so I didn't 
# included them in my models.
train <- combi[1:nrow(train), ]
test <- combi[nrow(train)+1 : nrow(test), ]

extractFeatures <- function(data){
  features <- c('Pclass',
                'Sex',
                'Age',
                'SibSp',
                'Parch',
                'Fare',
                #'Cabin',
                'Embarked',
                'Survived',
                'Title',
                'FamilySize',
                'FamilyID'
                #'Ticket'
  )
  fea <- data[ , features]
  return(fea)
}

# Logistic regression (Ridge)
#install.packages('glmnet')
library(glmnet)

# create a sparse matrix of the dataset but column Survived. This is converting categorical variables 
# into dummies variables.
x <- model.matrix(Survived~., data = extractFeatures(train))
y <- extractFeatures(train)$Survived
newx <- model.matrix(~., data = extractFeatures(test)[,-which(names(extractFeatures(test)) %in% 'Survived')])

# The glmnet package can fit lasso, ridge and elasticnet. Here we just fit ridge which performs a 
# little bit better than lasso in this dataset.
# Actually if you set set.seed(1), and use misclassification error as metric (set type.measure = 'class'), 
# the submitted score can be as high as 0.82297. However, it seems the result of this metric isn't as 
# stable as deviance.
set.seed(1)
fit_ridge <- cv.glmnet(x, y, alpha = 0, family = 'binomial', type.measure = 'class')
pred_ridge <- predict(fit_ridge, newx = newx, s = 'lambda.min', type='class')

# The submitted can score 0.80861.
set.seed(1)
fit_ridge <- cv.glmnet(x, y, alpha = 0, family = 'binomial', type.measure = 'deviance')
pred_ridge <- predict(fit_ridge, newx = newx, s = 'lambda.min', type='class')

submission <- data.frame(PassengerId = test$PassengerId, Survived = pred_ridge)
write.csv(submission, file = "titanic07-ridge.csv", row.names=FALSE) 

# Your submission scored 0.82297 or 0.80861, are improvement 0.81340

### Accuracy
PredictionTrainRidge <- predict(fit_ridge, train, OOB=TRUE, type = "response")
intern <- data.frame(original = train$Survived, trained = PredictionTrainRidge)

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

# Gradient boosting machine

library (caret)
library (lattice)
library (ggplot2)

fitControl <- trainControl ( method = 'repeatedcv' ,
                             number = 3 ,
                             repeats = 3 )
# for caret, there are only four tuning parameters below.

# tune n.trees
newGrid <- expand.grid ( n.trees = c ( 50 , 100 , 200 , 300 ), 
                         interaction.depth = c ( 6 ),
                         shrinkage = 0.01 ,
                         n.minobsinnode = 10
)
fit_gbm <- train ( Survived ~ . , data = extractFeatures ( train ), 
                   method = 'gbm' , 
                   trControl = fitControl ,
                   tuneGrid = newGrid ,
                   bag.fraction = 0.5 ,
                   verbose = FALSE )
fit_gbm $ bestTune

# tune interaction.depth
set.seed ( 1234 )
newGrid <- expand.grid ( n.trees = c ( 200 ), 
                         interaction.depth = c ( 4 : 12 ),
                         shrinkage = 0.01 ,
                         n.minobsinnode = 10
)
fit_gbm <- train ( Survived ~ . , data = extractFeatures ( train ), 
                   method = 'gbm' , 
                   trControl = fitControl ,
                   tuneGrid = newGrid ,
                   bag.fraction = 0.5 ,
                   verbose = FALSE )
fit_gbm $ bestTune

# decrease learning rate
set.seed ( 1234 )
newGrid <- expand.grid ( n.trees = c ( 2000 ), 
                         interaction.depth = c ( 10 ),
                         shrinkage = 0.001 ,
                         n.minobsinnode = 10
)
fit_gbm_LowerRate <- train ( Survived ~ . , data = extractFeatures ( train ), 
                             method = 'gbm' , 
                             trControl = fitControl ,
                             tuneGrid = newGrid ,
                             bag.fraction = 0.5 ,
                             verbose = FALSE )
fit_gbm_LowerRate $ results

# predict
pred_gbm <- predict ( fit_gbm_LowerRate , extractFeatures ( test ))
submission <- data.frame ( PassengerId = test $ PassengerId , Survived = pred_gbm )
write.csv(submission, file = "titanic07-gbm_ntree-2000_rate-0.001_inter-10.csv", row.names=FALSE)

### submited 0.81340

### Accuracy
PredictionTrainGBM <- predict(fit_gbm_LowerRate, train, OOB=TRUE, type = "response")
intern <- data.frame(original = train$Survived, trained = PredictionTrainGBM)

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

# Random Forest

library ( party )
set.seed ( 1 )
fit_crf <- cforest ( Survived ~ . , data = extractFeatures ( train ), controls = cforest_unbiased ( ntree = 2000 , mtry = 3 ))
pred_crf <- predict ( fit_crf , extractFeatures ( test ), OOB = TRUE , type = "response" )

submission <- data.frame ( PassengerId = test $ PassengerId , Survived = pred_crf )
write.csv(submission, file = "titanic07-crf_seed1.csv", row.names=FALSE)

### Submited in titanic05

### Accuracy
PredictionTrainCRF <- predict(fit_crf, train, OOB=TRUE, type = "response")
intern <- data.frame(original = train$Survived, trained = PredictionTrainCRF)

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

# Ensemble

cat ( 'Difference ratio between ridge and conditional random forest:' , sum ( pred_ridge != pred_crf ) / nrow ( test ))

cat ( 'Difference ratio between ridge and conditional gbm:' , sum ( pred_ridge != pred_gbm ) / nrow ( test ))

cat ( 'Difference ratio between conditional random forest and gbm:' , sum ( pred_crf != pred_gbm ) / nrow ( test ))

ensemble <- as.numeric ( pred_ridge ) + as.numeric ( pred_gbm ) -1 + as.numeric ( pred_crf ) -1
ensemble <- sapply ( ensemble / 3 , round )

submission <- data.frame ( PassengerId = test $ PassengerId , Survived = ensemble )
write.csv(submission, file = "titanic07-ensemble_vote2.csv", row.names=FALSE)

# Your submission scored 0.82297, are improvement 0.81340

# Your submission scored 0.82297, are not improvement 0.82297

### Accuracy
ensembleTrain <- as.numeric ( PredictionTrainRidge ) + as.numeric ( PredictionTrainGBM ) -1 + as.numeric ( PredictionTrainCRF ) -1
ensembleTrain <- sapply ( ensembleTrain / 3 , round )
intern <- data.frame(original = train$Survived, trained = ensembleTrain)

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
