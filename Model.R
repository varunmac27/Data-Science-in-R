CKData <- read.csv("~/Desktop//Second\ Sem/Healthcare/Assignment\ 2/CKData.csv")
View(CKData)
table(is.na(CKData))
#No null values
str(CKData)
library(dplyr)
CKData <- CKData %>% select(-X)

#Changing structure
CKData$Educ <- factor(CKData$Educ)
CKData$Unmarried <- factor(CKData$Unmarried)
CKData$Income <- factor(CKData$Income)
CKData$PVD <- factor(CKData$PVD)
CKData$Fam.Hypertension <- factor(CKData$Fam.Hypertension)
CKData$Activity <- factor(CKData$Activity)
CKData$Smoker <- factor(CKData$Smoker)
CKData$PoorVision <- factor(CKData$PoorVision)
CKData$Hypertension <- factor(CKData$Hypertension)
CKData$Diabetes <- factor(CKData$Diabetes)
CKData$Stroke <- factor(CKData$Stroke)
CKData$CVD <- factor(CKData$CVD)
CKData$CHF <- factor(CKData$CHF)
CKData$CKD <- factor(CKData$CKD)
CKData$Fam.CVD <- factor(CKData$Fam.CVD)
CKData$Insured <- factor(CKData$Insured)
table(CKData$Activity)

#Changing levels
CKData$Racegrp  <- relevel(CKData$Racegrp, ref='white') #White
CKData$CareSource  <- relevel(CKData$CareSource, ref='noplace') #No place
CKData$Activity  <- relevel(CKData$Activity, ref='1') #Mostly sit

#Checking most important variables
log <- glm(CKD~., data=CKData, family = "binomial")
summary(log)
#In the order of significance: 
#Age, Hypertension, Racegroup= Hispanic, Diabetes, CVD, PVD1

#Training and testing model: 
index <- sample(2, nrow(CKData), replace = T, prob = c(0.7,0.3))
TrainData <- CKData[index == 1, ]
TestData <- CKData[index == 2, ]
mod1 <- glm(CKD~., data=TrainData, family = "binomial")
summary(mod1)

probabilities <- predict(mod1, TestData, type = "response")
probabilities
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
predicted.classes <- factor(predicted.classes)

mean(predicted.classes == TestData$CKD)
#0.944

library(caret)
confusionMatrix(predicted.classes, TestData$CKD)


log <- glm(CKD~. , data=df1, family = "binomial")
library(epiDisplay)
logistic.display(log)





