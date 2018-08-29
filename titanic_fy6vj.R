# sys6018-competition-titanic
# https://www.kaggle.com/c/titanic/overview
# Fang You (fy6vj)

library(readr)  
library(dplyr) 
library(caret)

titanic_train <- read_csv("train.csv")
titanic_pred <- read_csv("test.csv")
# sample <- read_csv("gender_submission.csv")


# factorise variables
titanic_train$Survived <- factor(titanic_train$Survived)
titanic_train$Pclass <- factor(titanic_train$Pclass)
titanic_train$Sex <- factor(titanic_train$Sex)
titanic_pred$Pclass <- factor(titanic_pred$Pclass)
titanic_pred$Sex <- factor(titanic_pred$Sex)

# CV
train <- createDataPartition(titanic_train$Survived, p=0.6, list=FALSE)
training <- titanic_train[train, ]
testing <- titanic_train[-train, ]

glm <- glm(Survived ~ Pclass + Sex + Age + Fare, data = training, family = "binomial")
summary(glm)
# AIC: 404.6

glm_all <- glm(Survived ~ Pclass + Sex + Age + Fare, data = titanic_train, family = "binomial")

probs <- predict(glm_all, newdata = titanic_test)
preds <- rep(0, length(probs))  # Initialize prediction vector
preds[probs > 0.5] <- 1 # p>0.5 -> 1
titanic_pred_output <- data.frame(titanic_pred$PassengerId, preds)

write.table(titanic_pred_output, file = "titanic.csv", row.names=F, col.names=c("PassengerId", "Survived"), sep=",")
