library(readxl)
Parkinson <- read_excel("Desktop/Second Sem/Healthcare/Group Assignment 5/Parkinson.xlsx")
View(Parkinson)

Parkinson$Subject <- as.factor(Parkinson$Subject)
levels(Parkinson$Subject)

#[1] "S01" "S02" "S04" "S05" "S06" "S07" "S08" "S10" "S13" "S16" "S17" "S18" "S19" "S20" "S21"
#[16] "S22" "S24" "S25" "S26" "S27" "S31" "S32" "S33" "S34" "S35" "S37" "S39" "S42" "S43" "S44"
#[31] "S49" "S50"

x <- Parkinson %>% group_by(Subject) %>% count(status==1)
x
which(x$`status == 1`)
#[1]  1  2  3  4  5  7 10 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30

#24 observations

str(Parkinson)
Parkinson$status <- as.factor(Parkinson$status)
#Rest all numeric


#Correlation 
install.packages("ggpubr")
library(ggpubr)
cor(Parkinson$RPDE, Parkinson$D2)
#0.2369314
#Not taking a PCA here because they aren't correlated


#PCA for jitter 
jitter.pca <- prcomp(Parkinson[,c(6:10)], center = TRUE,scale. = TRUE)
jitter.pca
summary(jitter.pca)
typeof(jitter.pca) #list
jitter.pca
#Combining PC1 to the table 
jitter <- as.data.frame(jitter.pca[["x"]])
Parkinson <- cbind(Parkinson,jitter$PC1)

#PCA for shimmer 
shimmer.pca <- prcomp(Parkinson[,c(11:16)], center = TRUE,scale. = TRUE)
shimmer.pca
summary(shimmer.pca)
#Combining PC1 to the table 
shimmer <- as.data.frame(shimmer.pca[["x"]])
Parkinson <- cbind(Parkinson,shimmer$PC1)


#PCA for Noice to Tonel components in the voice
ntc.pca <- prcomp(Parkinson[,c(17:18)], center = TRUE,scale. = TRUE)
ntc.pca
summary(ntc.pca)
#Combining PC1 to the table 
ntc <- as.data.frame(ntc.pca[["x"]])
Parkinson <- cbind(Parkinson,ntc$PC1)


#PCA for fundamental frequency variation variable
ffv.pca <- prcomp(Parkinson[,c(22,23,25)], center = TRUE,scale. = TRUE)
ffv.pca
summary(ffv.pca)
#Combining PC1 to the table 
ffv <- as.data.frame(ffv.pca[["x"]])
Parkinson <- cbind(Parkinson,ffv$PC1)






