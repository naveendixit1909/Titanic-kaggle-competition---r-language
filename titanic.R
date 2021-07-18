titanic_train <- read.csv(file.choose(), stringsAsFactors = FALSE)
titanic_test <- read.csv(file.choose(), stringsAsFactors = FALSE)

titanic_train$IsTrainSet <- T
titanic_test$IsTrainSet <- F

titanic_test$Survived <- NA

titanic_full <- rbind(titanic_test, titanic_train)

titanic_full[titanic_full$Embarked== "", "Embarked"] <- "S"
table(titanic_full$Embarked)

age_median <- median(titanic_full$Age, na.rm = T)

titanic_full[is.na(titanic_full$Age), "Age"] <- age_median 

table(is.na(titanic_full$Age))

boxplot(titanic_full$Fare)

boxplot.stats(titanic_full$Fare)

upper_whisker <- boxplot.stats(titanic_full$Fare)$stats[5]
outlier_filter <- titanic_full$Fare < upper_whisker

titanic_full[outlier_filter,]
fare_eq <- "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
 
fare_model <- lm(formula = fare_eq,
   data = titanic_full[outlier_filter,]
   )
fare_row <- titanic_full[is.na(titanic_full$Fare),
             c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
             ]
titanic_full[]


fare_prediction <- predict(fare_model, newdata = fare_row)

titanic_full[is.na(titanic_full$Fare), "Fare"] <- fare_prediction


table(is.na(titanic_full$Fare))

table(titanic_full$Survived)

str(titanic_full)

titanic_full$Pclass <- as.factor(titanic_full$Pclass)
titanic_full$Sex <- as.factor(titanic_full$Sex)
titanic_full$Embarked <- as.factor(titanic_full$Embarked)




titanic_train <- titanic_full[titanic_full$IsTrainSet== T,]
titanic_test <- titanic_full[!titanic_full$IsTrainSet== T,]

titanic_train$Survived <- as.factor(titanic_train$Survived)

str(titanic_train) 

survived_eq <- "Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked"
survived_formula <- as.formula(survived_eq)

install.packages("randomForest")

library(randomForest)
titanic_model <- randomForest( formula = survived_formula, data = titanic_train, ntree = 500, nodesize = 0.1* nrow(titanic_test))

features_eq <- "Pclass + Sex + Age + Sibsp + Parch + Fare + Embarked"
suvived <- predict(titanic_model, newdata = titanic_test)
suvived
PassengerId <- titanic_test$PassengerId
outputdf <-as.data.frame(PassengerId)
outputdf$Survived <- suvived
write.csv(outputdf, file = "Kaggle_Submission_2.csv", row.names = F)
