##How many observations are in the dataset?
setwd("C:/Users/scott/school/BMI6106")
list.files()
#reading in file
birthwt_table = read.table("birthwt.txt", sep = " ", header = T)
### Q1A - 189 obs


## Examine each column(variable), determine what type of variable each represents,
##  and indicate whether each one is discrete or continuous. 
##  Then, go on to determine the following distribution or descriptive statistics as appropriate:
##  For Discrete Variables: 
## • Indicate whether the feature is nominal, ordinal, or binary
## • How many levels each variable has

## For Continuous variables:
##  • Determine the mean, standard deviation, and median

str(birthwt_table)
summary(birthwt_table)
sd(birthwt_table$age)
sd(birthwt_table$lwt)
sd(birthwt_table$ptl)
sd(birthwt_table$ftv)
sd(birthwt_table$bwt)
### Q1B - Variables in table:
#low - discrete - binary - 2, 
#age - continuous - mean (23.24) median (23), std (5.298678), 
#lwt - continuous - mean (129.8) median (121), std (30.57938), 
#race - discrete - nominal - 3,
#smoke - discrete - binary - 2, 
#ptl - continuous - mean (0.1958) median (0), std (0.4933419), 
#ht - discrete - binary - 2, 
#ui - discrete - binary - 2, 
#ftv - continuous - mean (0.7937) median (0), std (1.059286), 
#bwt - continuous - mean (2945) median (2977), std (729.0224)
  
##How many individuals older than 30 smoke?
library(tidyverse)
birthwt_table = birthwt_table %>% 
  mutate(older_smoker = if_else(condition = age > 30 & smoke==1,true = 1,false = 0))

table(birthwt_table$older_smoker)
### Q1C - 8 people 

## Plot a histogram for birth weight
library(ggplot2)

?ggplot
### Q1D
ggplot(birthwt_table, aes(x = bwt)) +
  geom_histogram(binwidth = 100, fill ="blue", color ="black") +
  labs(title ="Histogram of Birth Weight", x="Birth Weight", y="Count")

## Calculate the probability of randomly selecting an individual that has 
#either a low birth weight or a mother who was a smoker. 
prob_low <- mean(birthwt_table$low == 1)
prob_smoke <- mean(birthwt_table$smoke == 1)
prob_either <- prob_low+prob_smoke
print(prob_either)

## Q1G - 70.3%

## Calculate the probability of randomly selecting an individual that is white 
#and has more than 3 physician visits during the first trimester.

prob_white <- mean(birthwt_table$race ==1)
print(prob_white)
prob_visits <- mean(birthwt_table$ftv > 3)
print(prob_visits)
prob_both <- prob_white*prob_visits
print(prob_both)
### Q1H - 1.3%

#2A. What is the probability that given a positive mammogram exam, a woman 
# has a positive cancer diagnosis? Assume that the breast cancer incidence 
# rate is 1%, the positivity rate for the exam if a patient has cancer is 90%,
# and there is a false positive rate of 8% for the exam. 
#Use Bayes
P_C <- 0.01 #Prob cancer
P_TP_gvn_C <- 0.9 #Prob test positive given cancer
P_TP_not_C <- 0.08 #false positive
P_not_C <- 0.99 #Prob Not cancer
#Prob of testing positive
P_TP <- (P_TP_gvn_C*P_C)+(P_TP_not_C*P_not_C)
#Prob Cancer given postive test
P_C_gvn_TP = (P_TP_gvn_C*P_C)/P_TP

print(P_C_gvn_TP)
### Q2A - 10.2%

## For every attempt to call your friend, there is a 70% probability of 
# actually speaking with them. Calculate the probability of having exactly 
# 12 successes in 20 attempts. 
P_CS <- .7 #Prob speaking when called
P_CN <- .3 #Prob not speaking when called
N <- 20 #Total attempts
K <- 12 #Total successes

exact_12 <- dbinom(K, size=N, prob=P_CS)
print(exact_12)

### Q2B - 11.4%

## The cholesterol levels of a group of patients are normally distributed 
# with a mean of 200 mg/dL and a standard deviation of 25 mg/dL
mu <- 200  # Mean
sigma <- 25  # Standard deviation

# What is the probability that a randomly selected patient will have a 
# cholesterol level between 180 mg/dL and 220 mg/dL
prob_lt_220 <- pnorm(220, mu, sigma)
prob_lt_180 <- pnorm(180, mu, sigma)
prob_bet_180_220 <- prob_lt_220 - prob_lt_180
print(prob_bet_180_220)
### Q2C1 - 57.6%

## Additionally, calculate the interquartile range (IQR) 
# of the cholesterol levels.
Q1 <- qnorm(0.25, mean = mu, sd = sigma)
Q3 <- qnorm(0.75, mean = mu, sd = sigma)
IQR <- Q3-Q1
print(IQR)
### Q2C2 - 33.7

# Discuss how these statistics can be used to identify patients at 
# risk of cardiovascular diseases 

### Q2C3 - If one were to try to identify those patients with the highest percentile
# of cholesterol levels they can simply use the qnorm and feed in the percentile
# desired along with the mean and standard deviation.  Anyone higher than that 
# value can be considered at higher risk

# How the distribution might change if the standard deviation were 
# reduced to 15 mg/dL

### Q2C4 - the distribution would be much more narrow. IQR would be smaller, so it
# would not take as high of a value to be considered high risk than it was at sd 25

#Q3 Naïve Bayes classifier

# The dataset consists of measurements taken from breast masses using characteristics
# of cell nuclei extracted from digitized images. Your goal is to build a Naïve Bayes
# classifier to predict whether a tumor is benign or malignant. The dataset should 
# be available on canvas 

# Part 1: Data Preparation and Exploration
#a) Load the dataset in your preferred language and perform an initial exploration.
# What are the dimensions of the dataset? Are there any missing values? 
setwd("C:/Users/scott/school/BMI6106")
list.files()
Breast_Cancer = read.csv("Breast_cancer_Naive.csv", header=TRUE ) #load dataset
head(Breast_Cancer) #look at first few rows
View(Breast_Cancer)
str(Breast_Cancer) #look at vars in dataset
sum(is.na(Breast_Cancer)) #check for missings
### Q3_Pt1_A - Dim 569 obs 32 Vars --- No missings

#b) Summarize the key characteristics of the dataset using appropriate descriptive
# statistics and visualizations. What are the distributions of numerical features? 
summary(Breast_Cancer) #summary stats on each var
table(Breast_Cancer$diagnosis) #number Malignant (212) and benign (357)
nrow(Breast_Cancer[Breast_Cancer$diagnosis=="M",])
plot(density(Breast_Cancer$radius_mean))

install.packages("Amelia") #install packages needed
library(Amelia)
library(tidyverse)
install.packages("psych") #install packages needed
library(psych)
library(ggplot2)
str(Breast_Cancer) #look at data
names(Breast_Cancer) #look at var names
#see what plot looks like for dependent var
ggplot(Breast_Cancer, aes(x = diagnosis)) +
  geom_bar(fill = "blue") +
  labs(title = "Histogram of Diagnosis Counts", x = "Diagnosis", y = "Count")

#look at dataset
psych::describe(Breast_Cancer)

#look at a variable in the histogram by diagnosis
Breast_Cancer %>%
  ggplot(aes(x = radius_mean, fill = factor(diagnosis))) +
  geom_histogram(color = "white", bins = 25, alpha = 0.5) +
  theme_classic() +
  facet_wrap(~ diagnosis, labeller = labeller(diagnosis = c("M" = "Malignant", "B" = "Benign"))) +
  labs(title = "Radius Distribution by Diagnosis", x = "radius_mean", y = "Count")

##Facet plot of densities for features 
Breast_Cancer %>% 
  select(radius_mean, texture_mean, perimeter_mean, area_mean, smoothness_mean, 
         compactness_mean, concavity_mean, concave.points_mean, symmetry_mean, 
         fractal_dimension_mean) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

### Q3 Part 2: Naïve Bayes Classifier Implementation
## a.	Preprocess the data for Naïve Bayes classification. Make sure to encode
# categorical variables (if any) and split the data into training and testing sets 
# (70-30 split). Show your code and explain your preprocessing steps. 

# First I am converting the outcome variable to a factor variable.
Breast_Cancer$diagnosis = as.factor(Breast_Cancer$diagnosis)
missmap(Breast_Cancer) #take a look at missingness
#Dont have missing but do have some 0's

#Convert '0' values into NA
Breast_Cancer[,3:32][Breast_Cancer[, 3:32] == 0] <- NA #only need from var 3 on
head(Breast_Cancer,20)##Check the first 20 lines of the data frame

install.packages("mice") #will need these for next steps
library(mice)
install.packages("caret") #will need these for next steps
library(caret)
colSums(is.na(Breast_Cancer)) #see about how many missings there are and which vars
#new dataframe with missing values filled with estimates from random forest
mice_mod_bc <- mice(Breast_Cancer[, c("concavity_mean","concave.points_mean",
  "concavity_se","concave.points_se","concavity_worst","concave.points_worst")], method='rf')
mice_complete_bc <- complete(mice_mod_bc) #put the missing values assigned to new dataframe

# transfer the imputed columns to the data set
Breast_Cancer$concavity_mean = mice_complete_bc$concavity_mean
Breast_Cancer$concave.points_mean = mice_complete_bc$concave.points_mean
Breast_Cancer$concavity_se = mice_complete_bc$concavity_se
Breast_Cancer$concave.points_se = mice_complete_bc$concave.points_se
Breast_Cancer$concavity_worst = mice_complete_bc$concavity_worst
Breast_Cancer$concave.points_worst = mice_complete_bc$concave.points_worst

colSums(is.na(Breast_Cancer)) #check again now for missingness
missmap(Breast_Cancer)

set.seed(876) #set random seed
#split into 70-30 split for test and train
indxTrain <- createDataPartition(y = Breast_Cancer$diagnosis,p = 0.70,list = FALSE)
training <- Breast_Cancer[indxTrain,]
testing <- Breast_Cancer[-indxTrain,]

##	Train a Naïve Bayes classifier. Which are the features with the highest
# contribution to the classifier? Report the accuracy, sensitivity, and specificity
# of your model.

#Check dimensions of the split
#verify they all are close to the full data frame
prop.table(table(Breast_Cancer$diagnosis)) * 100
prop.table(table(training$diagnosis)) * 100
prop.table(table(testing$diagnosis)) * 100

#create objects x which holds the predictor variables and y 
#which holds the response variables
x = training[,-2]
y = training$diagnosis

#Run the Naive Bayes algorithm on the training data set using a resampling method, 
#remember the goal is to maximize the class prediction.
library(naivebayes)
model = train(x,y,'naive_bayes',trControl=trainControl(method='cv',number=10))

#Model Evaluation
#Predict using the testing set
Predict <- predict(model,newdata = testing )


X <- varImp(model) #creating and plotting the variable importance
plot(X)
###Q3 part 2b -- Best variables are perimeter_worst, area_worst, radius_worst

## Create and interpret the confusion matrix. What does it tell you about the 
# model's performance? Are there any potential concerns regarding false positives 
# or false negatives? 
confusionMatrix(Predict, testing$diagnosis )

### Q3 pt 2c --This shows the good accuracy at almost 96%. both sensitivity and specificity is 
# high.  We certainly want to minimize false negative the most because we want to
# be sure to detect cancer if it is really there.  false positive is also important
# so we dont scare patients unnecessarily and run unneeded test


##Plot the ROC curve for your Naïve Bayes model and compute the AUC (Area Under
# the Curve). What does the AUC tell you about the classifier's ability to 
# distinguish between benign and malignant cases? 
library(pROC)
roc_ = roc(testing$diagnosis,predict(model, newdata = testing, type ="prob")[,2])

### Q3 pt 2d
plot(roc_,print.auc=T)
# The ROC curve show an AUC at .99, which is very very high.  It has a great ability
# to detect between benign and malignant!!

 