#Setting Working Directory
path <- "D:/Study/ML/R/Titanic"
setwd(path)

#Loading Test and Train Data Set
Train = read.csv("Titanic_train.csv")
Test = read.csv("Titanic_test.csv")

summary(Train)
str(Train)

# Adding missing values for "Age" column with mean

Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)
summary(Train)
summary(Test)

# Creating Dataset of Independent/Dependent variables
nonvars = c("PassengerId","Name","Ticket","Cabin","Embarked")
Train = Train[,!(names(Train) %in% nonvars)]
str(Train)

#Check for MultiCollinearity
Train$Sex = as.numeric(Train$Sex)
Test$Sex = as.numeric(Test$Sex)
cor(Train)

# Build Logistic Regression Model
TitanicLog1 = glm(Survived~., data=Train, family=binomial)
summary(TitanicLog1)

#Revised Model
TitanicLog2 = glm(Survived~.-Parch-Fare,data=Train, family=binomial)
summary(TitanicLog2)

#Test Accuracy of Model

baseAcur = 549 / (549+342)
predictTrain = predict(TitanicLog2, type="response")
table(Train$Survived, predictTrain >= 0.5)

accuracy = (244 + 458) / nrow(Train)
sensitivity = 244 / (244 + 98)
specificity = 458 / (458 + 91)

cat("accuracy: ", accuracy, " > ", "baseline: ", baseAcur)

# Predict on Test data
predictTest = predict(TitanicLog2,type="response", newdata = Test)
Test$Survived = as.numeric(predictTest >= 0.5)
table(Test$Survived)
Predictions = data.frame(Test[c("PassengerId","Survived")])
write.csv(file = "TitanicPred", x = Predictions)



