# 2. Problem Set R

### Now is yoiur turn to practice!!

#### We are using a new Dataset, This is a frequently used dataset for multiple applications in statistics and machine learning. This dataset is deposited in a website that is pretty useful in bioinformatics to look for tutorials, datasets and advice Kaggle (https://www.kaggle.com)

#### Go to https://www.kaggle.com/saurabh00007/diabetescsv and download the csv file

#### This dataset consists on clinical variables for 768 patients to evaluate a few variables to predict whether a patient has diabetes.

#### Please write the R code necessary to run the next items:

#### 1. Load the dataset and show the first 5 lines<br>
setwd("C:/Users/scott/school/BMI6106")
list.files()
diabetes_table = read.table("diabetes.csv", sep = ",", header = T)
#or could have just used read.csv function instead here
head(diabetes_table)
class(diabetes_table)

#just to see a list of all columns
data.frame(Column = names(diabetes_table)) 

#### 2. How many patients have diabetes?<br>

table(diabetes_table$Outcome)
nrow(diabetes_table[diabetes_table$Outcome==1,]) #or can do this way also - all that have diabetes and gives all the rows
# 268 with diabetes

#### 3. How many patients have diabetes that are older than 45?<br>
library(tidyverse)
diabetes_table = diabetes_table %>% 
  mutate(older_diabetes = if_else(condition = Age > 45 & Outcome==1,true = 1,false = 0))
head(diabetes_table,2)

table(diabetes_table$older_diabetes)
# 58 with diabetes over 45

#### 4. What is the mean and variance of glucose levels for individuals without diabetes<br>
No_Diabetes <- diabetes_table %>% filter(Outcome == 0)
summary_glucose <- No_Diabetes %>% 
  summarize(mean = mean(Glucose), var = var(Glucose))
summary_glucose
#mean = 109.98
#var = 683.3623


#### 5. Create a new discrete variable that has 1 if the individual has diabetes and high blood pressure (above 100), 2 if an indivual has diabetes and low blood pressure and 3 if the individual does not have diabetes.<br>
diabetes_table <- diabetes_table %>%
  mutate(diabetes_bp = if_else(
    Outcome == 1 & BloodPressure > 100, 1,
    if_else(Outcome == 1 & BloodPressure <= 100, 2, 3)
  ))

#### 6. Construct two plots of the distribution of BMI for individuals with diabetes and without diabetes<br><br>

?ggplot
diabetes_table %>%
  ggplot(aes(x = BMI, fill = factor(Outcome))) +
  geom_histogram(color = "white", bins = 25, alpha = 0.5) +
  theme_classic() +
  facet_wrap(~ Outcome, labeller = labeller(Outcome = c("0" = "No Diabetes", "1" = "Diabetes"))) +
  labs(title = "BMI Distribution by Diabetes Outcome", x = "BMI", y = "Count")

