#Testing

#Significant
chisq.test(CK$Agecat, CK$CKD)    #p-value < 0.00000000000000022
chisq.test(CK$Racegrp, CK$CKD)     #p-value = 0.000000000000001843
chisq.test(CK$Educ, CK$CKD)      #p-value = 0.000005362
chisq.test(CK$Unmarried, CK$CKD)     #p-value = 0.000002014
chisq.test(CK$Income, CK$CKD)       #p-value = 0.00000000003581
chisq.test(CK$CareSource, CK$CKD)     #p-value = 0.0000000000001243
chisq.test(CK$Insured, CK$CKD)     #p-value < 0.00000000000000022
chisq.test(CK$PVD, CK$CKD)         #p-value < 0.00000000000000022
chisq.test(CK$Activity, CK$CKD)      #p-value < 0.00000000000000022
chisq.test(CK$PoorVision, CK$CKD)     #p-value = 0.00000000000009865
chisq.test(CK$Smoker, CK$CKD)        #p-value = 0.0000002147
chisq.test(CK$hyper, CK$CKD)        #p-value < 0.00000000000000022
chisq.test(CK$Stroke, CK$CKD)      #p-value < 0.00000000000000022
chisq.test(CK$CHF, CK$CKD)       #p-value < 0.00000000000000022
chisq.test(CK$db, CK$CKD)      #p-value < 0.00000000000000022
chisq.test(CK$cvd, CK$CKD)       #p-value < 0.00000000000000022
chisq.test(CK$Anemia, CK$CKD)      #p-value = 0.00003015

#Insignificant
chisq.test(CK$Female, CK$CKD)   #p-value = 0.2846
chisq.test(CK$Dyslipidemia, CK$CKD)    #p-value = 0.9507
chisq.test(CK$BMIcat, CK$CKD)     #p-value = 0.2962


#Final Selecting Variables
df = subset(CK, select = -c(Female, BMI, Dyslipidemia, `Fam Diabetes`,
                            BMIcat, `Fam Hypertension`, `Fam CVD`, Diabetes, Hypertension,
                            CVD, Age))

df1 <- na.omit(df)
summary(df1)

#Model
log <- glm(CKD~., data=df1, family = "binomial")
summary(log)


#Odds Ratio
install.packages("epiDisplay")
library(epiDisplay)
logistic.display(log)


#Validating
index <- sample(2, nrow(df), replace = T, prob = c(0.7,0.3))
TrainData <- df[index == 1, ]
TestData <- df[index == 2, ]
mod1 <- glm(CKD~., data=TrainData, family = "binomial")
summary(mod1)

probabilities <- predict(mod1, TestData, type = "response")
probabilities
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
predicted.classes <- factor(predicted.classes)

mean(predicted.classes == TestData$CKD)
#0.938152

#Confusion Matrix
library(caret)
confusionMatrix(predicted.classes, TestData$CKD)







