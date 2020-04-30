# Data-Science-in-R

# Trend analysis to increase EMR presence
- Researched and performed feature selection of 3000 variables of National Institute of Health Trends Survey to cluster 18 variables into buckets based on Technology Assessment Model.
- Obtained inductive inferences from univariate and bivariate descriptive statistics. Generated plots to test hypothesis. 
- Applied statistical methods such as Principal Component Assessment and Factor Analysis. R
- Measured performance of the multivariate logistic regression model formed based on predictions on test data.

# Screening for Chronic Kidney Disease
- Imputed derived variables, created new variables with interaction effect and down-sampled the target variable. 
- Performed Chi-Square test, Analysis of Variance (Anova) and Correlation test for feature reduction and pattern discovery.
- Using odds ratio in logistic regression, interpreted the likelihood probability of increase in the disease.
- Applied random forest model to study the interaction effect and derived decision rules to classify patients. 

# Vanderbilt Case study 
- With the given dataset, predict the number of surgeries that would take place on a particular day
- With current procedures, predicting a day in advance leads to over compensation for staff and lesser supplies
- With final day surgeries, and days in advance number of surgeries: calculated the least mean squared error using logistic regression
- 7 days prior was the least error: hence a fair prediction can be made 7 days advanced
- The number of emergencies (a derived variable), its mean and variance was also calculated: to be prepared
- Compared the analysis on weekdays

# Decision Tree Practice
- Computed Gini index and information gain manually for creating decision trees to understand the underlying concept
- Creating C&R trees and C5 trees in R with the given dataset using rpart and ctree libraries
- Controlled tree parameters using cp, minbucket, minsplit and maxdepth in list=parms()function. 

# Bagging and Random Forest Practice
- Computing Confusion Matrix, ROC curve, AUC and understanding Specifity and Sensitivity concepts
- Understanding general classifiers and creating random forest and bagging models to outperform the general classifier
- Created tree using tree() function and prunes using prune() function to bootstrap multiple trees to aggregate
