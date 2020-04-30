data <- Package_Pricing_at_Mission_Hospital_Data_Supplement

str(data)

install.packages("funModeling")
library(funModeling)
df_status(data)
#KeyComplaints-Code has null values due to no complaints faced: regular checkups
#14.52% of the people had no complaints but the rest had one 

#Null in key code complaints

kcc <- data[which(is.na(data$`KEY COMPLAINTS -CODE`)==TRUE),]
table(kcc$`PAST MEDICAL HISTORY CODE`)
#11: hypertension and others; 25: Null

table(kcc$`MODE OF ARRIVAL`)
#AMBULANCE TRANSFERRED   WALKED IN 
#5           2          29 
#29 walked-in: not emergency cases

table(kcc$`STATE AT THE TIME OF ARRIVAL`)
#All were alert

table(kcc$`TYPE OF ADMSN`)
#ELECTIVE EMERGENCY 
#31         5

table(kcc$`TOTAL LENGTH OF STAY`)
#Varied from 5-21

options(scipen=99)
plot(density(kcc$`TOTAL COST TO HOSPITAL`), xlab= "Cost incurred by people with no complaint", ylab= "Proportion of people")

#Implant cost vs total cost
plot(data$`TOTAL COST TO HOSPITAL`~ data$`COST OF IMPLANT`)

#Total length of stay vs total cost : increases
boxplot(data$`TOTAL COST TO HOSPITAL`~data$`TOTAL LENGTH OF STAY`)

#Total length of stay in ward vs total cost : stable
plot(data$`TOTAL COST TO HOSPITAL`~data$`LENGTH OF STAY- WARD`)
#Remove total length of stay in ward variable
summary(aov((data$`TOTAL COST TO HOSPITAL`~data$`LENGTH OF STAY- WARD`)))
#p-value: 0.0113 *


#Total length of stay in ward vs total cost : increases
plot(data$`TOTAL COST TO HOSPITAL`~data$`LENGTH OF STAY- ICU`)


table(data$`TYPE OF ADMSN`, data$`MODE OF ARRIVAL`)
#Type of admission preferred over mode of arrival

table(data$`STATE AT THE TIME OF ARRIVAL`, data$`TYPE OF ADMSN`)

table(data$`STATE AT THE TIME OF ARRIVAL`)
#Removing this variable

summary(data$CREATININE)
summary(data$UREA)
summary(data$HB)

c <- data[which(is.na(data$CREATININE)==TRUE), ]
#All walk ins

u <- data[which(is.na(data$UREA)==TRUE), ]
u$`TYPE OF ADMSN`

pmhc <- data[which(is.na(data$`PAST MEDICAL HISTORY CODE`)==TRUE), ]
summary(pmhc$`TOTAL COST TO HOSPITAL`)
table(data$Diabetes1)
table(data$Diabetes2)
table(data$hypertension1)
table(data$hypertension2)
table(data$hypertension3)
table(data$other)

pmhc1 <- data[which(is.na(data$`PAST MEDICAL HISTORY CODE`)==FALSE), ]
summary(pmhc1$`TOTAL COST TO HOSPITAL`)

data$`PAST MEDICAL HISTORY CODE` <- as.factor(data$`PAST MEDICAL HISTORY CODE`)
xtabs(~data$`PAST MEDICAL HISTORY CODE`+data$GENDER)

data$`KEY COMPLAINTS -CODE` <- as.factor(data$`KEY COMPLAINTS -CODE`)
xtabs(~data$`KEY COMPLAINTS -CODE`+data$GENDER)













