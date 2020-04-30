
str(training_data)

train = subset(training_data, select= c(gleason_score, t_score, n_score, m_score, race, 
                                        previous_cancer, smoker, side, rd_thrpy, h_thrpy,
                                        chm_thrpy, brch_thrpy, rad_rem, multi_thrpy,
                                        survival_1_year, survival_7_years, agecat, BMIcat, 
                                        Fam, diffintumor, psadiff))

#Significant
chisq.test(training_data$gleason_score, training_data$survival_7_years) #p-value < 2.2e-16
chisq.test(training_data$t_score, training_data$survival_7_years)   #p-value < 2.2e-16
chisq.test(training_data$m_score, training_data$survival_7_years)  #p-value < 2.2e-16
chisq.test(training_data$n_score, training_data$survival_7_years)   #p-value < 2.2e-16
chisq.test(training_data$race, training_data$survival_7_years)   #p-value = 0.002052
chisq.test(training_data$rd_thrpy, training_data$survival_7_years)   #p-value < 2.2e-16
chisq.test(training_data$chm_thrpy, training_data$survival_7_years)   #p-value < 2.2e-16
chisq.test(training_data$brch_thrpy, training_data$survival_7_years)   #p-value = 9.432e-10
chisq.test(training_data$multi_thrpy, training_data$survival_7_years)  #p-value < 2.2e-16
chisq.test(training_data$survival_1_year, training_data$survival_7_years) #p-value < 2.2e-16
chisq.test(training_data$agecat, training_data$survival_7_years)  #p-value = 1.117e-05
chisq.test(training_data$BMIcat, training_data$survival_7_years)  #p-value = 0.0002984
chisq.test(training_data$Fam, training_data$survival_7_years)  #p-value = 0.03903
chisq.test(training_data$h_thrpy, training_data$survival_7_years)  #p-value = 0.02563
chisq.test(training_data$rad_rem, training_data$survival_7_years)  #p-value = 0.03444
chisq.test(training_data$previous_cancer, training_data$survival_7_years)  #p-value = 0.0182
summary(aov(training_data$diffintumor~training_data$survival_7_years)) #<2e-16 ***
summary(aov(training_data$psadiff~training_data$survival_7_years))   #<2e-16 ***
  
#Insiginficant
chisq.test(training_data$smoker, training_data$survival_7_years)   #p-value = 0.3814
chisq.test(training_data$side, training_data$survival_7_years)  #p-value = 0.681

train1 <- na.omit(train)
str(train1)
train1$BMIcat <- as.factor(train1$BMIcat)

training <- train1 %>% select (-c(smoker, side))
str(train1)
library(randomForest)
fit = randomForest(survival_7_years ~ ., data=training, ntree=500, 
                   importance=TRUE, proximity=TRUE)
training <-  training_data %>% select (-c(smoker, side ))

importance(fit)
varImpPlot(fit)
summary(fit)

train2 <- subset(train1, select = -survival_7_years)
library(rpart)

a <- rpart(survival_7_years~., data=training)
library(rpart.plot)
rpart.plot(a)

training <-  train1 %>% select (-c(smoker, side ))


p <- predict(a, newdata= TestData, type= "class")
p
summary(p)
Test <- cbind(TestData, p)













