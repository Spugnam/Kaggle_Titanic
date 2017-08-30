source("titanic_helper.R")

library(seqinr)
library(e1071)
library(party)
library(Amelia)
library(ggplot2)
library(corrgram)

#----------------------------- Read Data --------------------------------
titanic.master.data <- read.csv("train.csv",header = TRUE, sep=",", na.strings = c("NA", ""))
titanic.exploration.data<-titanic.master.data
#View(titanic.exploration.data)

titanic.evaluation.data <- read.csv("test.csv",header = TRUE, sep=",", na.strings = c("NA", ""))
titanic.evaluation.data$Survived <- 0

# add mean to Age
#Create LM models for predicting missing values in AGE and FARE
age.mod <- lm(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = titanic.exploration.data)

# Replace missing values in AGE and FARE with prediction
titanic.exploration.data$Age[is.na(titanic.exploration.data$Age)] <- predict(age.mod, titanic.exploration.data)[is.na(titanic.exploration.data$Age)]
titanic.evaluation.data$Age[is.na(titanic.evaluation.data$Age)] <- predict(age.mod, titanic.evaluation.data)[is.na(titanic.evaluation.data$Age)]
```

# Change variable class
titanic.exploration.data <- clean.features.one(titanic.data = titanic.exploration.data)
titanic.evaluation.data <- clean.features.one(titanic.data = titanic.evaluation.data)

#create new numeric variable family size
titanic.exploration.data$family.size <- 1+titanic.exploration.data$Parch+titanic.exploration.data$SibSp
titanic.evaluation.data$family.size <- 1+titanic.evaluation.data$Parch+titanic.evaluation.data$SibSp

#create new binned factor for family
titanic.exploration.data <- generate.new.feature.binned.family(titanic.data = titanic.exploration.data)
titanic.evaluation.data <- generate.new.feature.binned.family(titanic.data = titanic.evaluation.data)

#create new binned fare
titanic.exploration.data <- generate.new.feature.binned.fare(titanic.data = titanic.exploration.data)
titanic.evaluation.data <- generate.new.feature.binned.fare(titanic.data = titanic.evaluation.data)

#generate.new.feature.binned.fare <- function(titanic.data)  #Remove
  
#------------------------ Attempt Logistic Regression -----------------------------------------
#Train The Data
attach(titanic.exploration.data)
titanic.logistic.model <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Embarked+binned.fare+binned.family ,data = titanic.exploration.data, family = 'binomial')
titanic.logistic.model

table(titanic.exploration.data$Survived, titanic.exploration.data$binned.family)
# colnames(titanic.exploration.data)
#lapply(titanic.exploration.data, class)

glm.probs <- predict.glm(titanic.logistic.model,titanic.evaluation.data)
glm.probs <- predict.glm(titanic.logistic.familymodel,titanic.evaluation.data)

glm.probs
glm.probs[is.na(glm.probs)] <- 0   # no undefined value
glm.probs[glm.probs >= 0] <- 1  # here 0 or 1 value predicted from <0 >0
glm.probs[glm.probs < 0] <- 0
summary(glm.probs)

# Tests
length(glm.probs[glm.probs >= 0])
length(glm.probs[glm.probs < 0])
1-length(glm.probs[glm.probs >= 0])/length(glm.probs[glm.probs < 0])
length(glm.probs[is.na(glm.probs)])

# Identification of best predictors using RSS **********
library(leaps)
regfit.full=regsubsets(Survived ~ Pclass+Sex+Age+SibSp+Parch+Embarked+Fare+binned.family ,data = titanic.exploration.data)
reg.summary = summary(regfit.full)
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(8,reg.summary$adjr2[8], col="red",cex=2,pch=20)
# Coefficient of the top predictors
coef(regfit.full ,7)

# Attempt with polym***************
titanic.logistic.polym.model<- lm(factor(Survived)~polym(Age, degree = 2, raw=FALSE)+Pclass+Sex+binned.fare+binned.family, data=titanic.exploration.data)
titanic.logistic.polym.model
glm.probs.polym <- predict.glm(titanic.logistic.polym.model,titanic.evaluation.data)
plot(glm.probs.polym) # don't understand why predict values are between 1 and 2 vs 0 and 1 ??

glm.probs.polym[is.na(glm.probs.polym)]    # no undefined value
glm.probs.polym[glm.probs.polym < 1.5] <- 0
glm.probs.polym[glm.probs.polym >= 1.5] <- 1  
summary(glm.probs.polym)
# End Attempt with polym**************

# Comparison
titanic.evaluation.data['Survived'] <- glm.probs
titanic.evaluation.data['Survived_polym'] <- glm.probs.polym

#Test Data
titanic.evaluation.data['Survived'] <- glm.probs.polym
write.csv(titanic.evaluation.data[c("PassengerId","Survived")],"gender_submission.csv", row.names = FALSE)
