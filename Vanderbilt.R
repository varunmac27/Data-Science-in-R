#Vanderbilt Case Study 

getwd()
setwd("/Users/jigyasasachdeva/desktop")
V <- Vanderbilt_Univ_Case_Dataset
rm(Vanderbilt_Univ_Case_Dataset)
str(V)
#All numericals except Date (POSIXct format), Day as character
V$DOW <- factor(V$DOW, levels = c("Mon", "Tue", "Wed", "Thu", "Fri"))

#Univariate

table(V$DOW) #Almost same values
#Fri Mon Thu Tue Wed 
#49  47  48  49  48 

pairs(V[, 3:19], col= "steelblue")
#T-28, T-21 are not linearly related to Actual

dev.off()
par(mfrow= c(1,2))     
boxplot(V$`T - 1`~V$DOW, col= c("darkgrey", "steelblue"), 
        xlab= "Days", ylab= "Surgeries predicted a day before")
boxplot(V$Actual~V$DOW, col= c("darkgrey", "steelblue"), 
        main= "Actual surgeries vs Days", xlab= "Days", ylab= "Actual number of surgeries")
#On average, fridays have a lot more actual surgeries than predicted a day before

aov1 <- aov(V$Actual~V$DOW)
TukeyHSD(aov1)

#---------------------------------------------------------
dev.off()
fri <- V[V$DOW=="Fri",]
plot(density(fri$`T - 1`, fri$Actual))


#---------------------------------------------------------
library(dplyr)
V1 <- V %>% select (`T - 28`,`T - 21`, `T - 14`, `T - 13`,
                    `T - 12`, `T - 11`, `T - 10`, `T - 9`, 
                    `T - 8`, `T - 7`, `T - 6`, `T - 5`, 
                    `T - 4`, `T - 3`, `T - 2`, `T - 1`)

install.packages("tseries")
library(tseries)
adf.test(V1, alternative = c("stationary", "explosive"), k = trunc((length(V1)-1)^(1/3)))

#-------------------------------------------------------
#Created a new variable: Emergency : Difference between T1 and Actual 
#These were unannounced surgeries

V$Emergency <- V$Actual - V$`T - 1`
library(psych)
describe(V$Emergency)
#Range: -9 to 23 
#mean: 6.37 
#median: 7
#se: 0.3

plot(density(V$Emergency))
#Normally distrubuted variable

#-----------------------------------------------------

boxplot(V$Emergency~V$DOW, col=c("darkgrey", "steelblue"), 
        main= "Emergency cases v/s Days",
        xlab= "Day of the week", ylab= "Number of emergency cases")
aov2<- aov(V$Emergency~V$DOW)
TukeyHSD(aov2)
#Mon-Fri and Tue-Fri is significant with negative coeff

#-----------------------------------------------------

#Tests
means <- colMeans(V2, na.rm = FALSE, dims = 1)
plot(V$Actual~means)
cor(V$Actual, means)
cor(V$Actual, V$`T - 21`)
#Correlation is decreasing for every T from 1 to 28
t.test(V$Actual, means)


#Model
linear<- lm(V$Actual~means+V$DOW+V$Emergency)
summary(linear)
dev.off()
car::vif(linear)

mod1 <- lm(Actual~., data=V)
car::vif(mod1)
mod <- lm(Actual~`T - 28`+`T - 21`+`T - 7`, data=V)
summary(mod)

V$month <- months(V$SurgDate, abbreviate = TRUE)
V$month <- factor(V$month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May",                                       "Jun", "Jul", "Aug", "Sep"))
boxplot(V$Actual~V$month, col= rainbow(12), main= "Actual surgeries v/s months",
        xlab= "Month", ylab= "Actual number of surgeries")
actualmonth<- aov(V$Actual~V$month)
Tu <- TukeyHSD(actualmonth)

boxplot(V$Emergency~V$month, col= rainbow(12), main= "Emergencies v/s months",
        xlab= "Month", ylab= "Number of emergencies")
emermonth<- aov(V$Emergency~V$month)
TukeyHSD(emermonth)

V$week <- strftime(V$SurgDate,format="%W")


#-----------------------------------
#Model

Dec <- DecRelVanderbilt[,-1]
Train <- Dec[1:227, ]
Test <- Dec[228:nrow(Dec),]
mod1 <- lm(Actual~., data= Train)
summary(mod1)
predicted <- predict(mod1, newdata = Test)
Test$Actual - predicted

#------------------------------------
#Model2

Vfinal <- select(Vanderbilt_Univ_Case_Dataset, -1)
str(Vfinal)
Vfinal$DOW <- factor(Vfinal$DOW)

Training <- Vfinal[1:227, ]
Testing <- Vfinal[228:nrow(Vfinal),]

mod2 <- lm(Actual~., data= Training)
summary(mod2)

predicting <- predict(mod2, newdata = Testing)
predicting

diff <- Testing$Actual - predicting
diff
mean (diff)

RSS <- c(crossprod(mod2$residuals))
MSE <- RSS / length(mod2$residuals)
RMSE <- sqrt(MSE)
V$SurgDate$day


#---------------------------------------------
#Mod3

mod3 <- lm(Actual~`T - 1`+ DOW, data = Train)


#-------------------------

V$day <- format(V$SurgDate, format = "%d")

typeof(V$day)
V$day <- as.numeric(V$day)


V$week[V$day<7] <- "First week"
V$week[V$day>=7 & V$day<15] <- "Second week"
V$week[V$day>=15 & V$day<22] <- "Third week"
V$week[V$day>21] <- "Fourth week"
table(V$week)

V$week <- factor(V$week, levels= c("First week", "Second week", 
                                      "Third week", "Fourth week"))
boxplot(V$Actual~V$week, col= rainbow(4))
boxplot(V$Emergency~V$week, col= rainbow(4))


#--------------------------------------------------


modx<- lm(V$Actual~V$`T - 1`)
summary(modx)


V$Quarter <- quarters(V$SurgDate)


boxplot(V$Emergency~ V$Quarter, col= c("Coral", "Steelblue"))
boxplot(V$Actual~ V$Quarter, col= c("Coral", "Steelblue"))
aovEQ <- aov(V$Emergency~V$Quarter)
TukeyHSD(aovEQ)
aovAQ <- aov(V$Actual~V$Quarter)
TukeyHSD(aovAQ)








