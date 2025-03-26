##
##Question 1 
##
set.seed(645) #Set a seed for reproducing

#First create dataframe of 10,000 2-child families with equally likely genders
two_child_gender <- data.frame(
  child1 = sample(c("Boy", "Girl"), 10000, replace = TRUE, prob = c(0.5, 0.5)),
  child2 = sample(c("Boy", "Girl"), 10000, replace = TRUE, prob = c(0.5, 0.5))
)

#Create a result field identifying combos of genders
two_child_gender$Result <- ifelse(two_child_gender$child1 == "Boy" & two_child_gender$child2 == "Boy", "Both Boys",
  ifelse(two_child_gender$child1 == "Girl" & two_child_gender$child2 == "Girl", "Both Girls", "Mixed"))

#then see how many are both boys
both_boys = sum(two_child_gender$Result == "Both Boys")
print(both_boys)


#create dataframe of 2-child families with days of week 
two_child_day <- data.frame(
  child1 = sample(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), 10000,
                  replace = TRUE, prob = c(100/7, 100/7, 100/7, 100/7, 100/7, 100/7, 100/7)),
  child2 = sample(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), 10000, 
                  replace = TRUE, prob = c(100/7, 100/7, 100/7, 100/7, 100/7, 100/7, 100/7))
)

#two_child_day

#merge the 2 dataframes together
child_gender_day <- merge(two_child_gender, two_child_day, by = "row.names")
#child_gender_day

#see how many of our 2-child families are both boys and a Wednesday child
boys_wednesday = sum(child_gender_day$Result == "Both Boys" & 
                       (child_gender_day$child1.y == "Wed" | child_gender_day$child2.y == "Wed"))
boys_wednesday
##
##Q1A - In this sample there were 728 families out of 10000 with 2 boys with at least one on Wednesday
##

##
##Q1B - I created 2 dataframes containing child1 and child2 columns. One with random sample of gender
# the other with random sample of days of week.  Then merged the 2 together and summed up those with 
# both boys and at least one with a Wednesday entry
##

##
##Question 2
##
#P(S) = Probability of spam (A priori probability) = 0.5
#P(G1) = Probability of group 1 (classified as spam) = 0.5
#P(G2) = Probability of group 2 (not classified spam) = 0.5
#Event A = Probability of spam given has 'this isnt spam'
#P(A) = P(A|G1)P(G1) + P(A|G2)P(G2)
#P(A|G1) = 210/1000 (proportion of this isnt spam emails in group 1)
#P(A|G2) = 23/1000 (proportion of this isnt spam emails in group 2)
##
#P(A) = 0.21(0.5) + 0.023/(0.5) = .1165 
#Q2 The Probability of email being spam given contains phrase "This isnt spam" is 0.1165
#

##
##Question 3
##

#read in the dataset
#TA will need to replace my location with his location
heart_clev = read.csv("C:/Users/scott/school/BMI6106/heart_cleveland_upload.csv", sep = ",", header = T)
tot_rows = nrow(heart_clev) #get total rows
tot_rows #297 observations

tot_disease = sum(heart_clev$condition == 0) #get total with disease (narrowing)
tot_disease #160 with disease


# Parameters
# Number of 'successes' trying to find (will need to do 1 minus this in function)
x <- 3 
# Number of 'successes' in population
m <- tot_disease
# Number of 'failures' in population
n <- tot_rows - tot_disease
# Sample size
k <- 25

# Use Hypergeometric distribution - sampling without replacement
# find probability finding at least 3, which is 1 minus prob X <=2
# P(x>=3) = 1 - P(x<=2)
# P(x<=2) = P(x=0) + P(x=1) + P(x=2)
Px_0 <- (choose(m, 0))*(choose(n, k))/choose(tot_rows, k)
Px_0
Px_1 <- (choose(m, 1))*(choose(n, k-1))/choose(tot_rows, k)
Px_1         
Px_2 <- (choose(m, 2))*(choose(n, k-2))/choose(tot_rows, k)
Px_2
#add them all up
Px_le2 = Px_0 + Px_1 + Px_2
#subtract it from 1
Px_ge3 = 1 - (Px_le2)
Px_ge3
##
#Q3A - Prob using hypergeometric function manual way is 0.9999993
##

#Found easier way to do this with phyper function
#probability of having at least three narrowing in the sample
prob_at_least_3 <- 1 - phyper(x-1, m, n, k, lower.tail = TRUE)
print(paste("The probability of having at least three narrowing in the sample is", round(prob_at_least_3, 8)))

#Q3A Confirming answer with R function phyper is 0.99999932

##
##Question 3B - first find number with high cholesterol
##
# get number of patients with high cholesterol
patients_high_chol = sum(heart_clev$chol >= 300)
patients_high_chol #45 patients with high chol

# get probability of high cholesterol
prob_high_chol = patients_high_chol/tot_rows
prob_high_chol #prob is .1515

#Using Geometric function to find E(X) = 1/P  -- gives avg number found before first high cholesterol
E_x = 1/prob_high_chol
E_x
##
#Q3B the average number found before first high chol is 6.6
##

##
##Question 4
##

#read in the dataset
#TA will need to replace my location with his location here
meps_subset_2019 = read.csv("C:/Users/scott/school/BMI6106/meps_subset_2019.csv", sep = ",", header = T)

##H0 - not significant difference in mean healthcare expenditures between heath status groups
##H1 - At least one health status group has significant diff mean expenditures from another

##conduct Exploratory Data Analysis and examine dist

#identify how many in each grouping
table(meps_subset_2019$RTHLTH53)

library(tidyselect)
library(tidyverse)

#Produce descriptive statistics by group
meps_subset_2019 %>%  group_by(RTHLTH53) %>% 
  summarise(n = n(), 
            mean = mean(TOTEXP19, na.rm = TRUE), 
            sd = sd(TOTEXP19, na.rm = TRUE),
            stderr = sd/sqrt(n),
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median = median(TOTEXP19, na.rm = TRUE),
            min = min(TOTEXP19, na.rm = TRUE), 
            max = max(TOTEXP19, na.rm = TRUE),
            IQR = IQR(TOTEXP19, na.rm = TRUE))

#Produce Boxplots and visually check for outliers
library(ggplot2)

#new df with removing records with health levels unknown
meps_subset_2019_rm <- meps_subset_2019[meps_subset_2019$RTHLTH53 > 0, ]

ggplot(meps_subset_2019_rm, aes(x = RTHLTH53, y = TOTEXP19, fill = RTHLTH53)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Health Categories") + 
  theme_bw() + theme(legend.position="none")


#Designate RTHLTH53 as a categorical factor
meps_subset_2019$RTHLTH53_factor<-as.factor(meps_subset_2019$RTHLTH53)

##Check for normality and homogeneity of variances

#look at QQ Plots to see normality
#install.packages("qqplotr")
library(qqplotr)
#Perform QQ plots by group
ggplot(data = meps_subset_2019_rm, mapping = aes(sample = TOTEXP19, color = factor(RTHLTH53))) +
  stat_qq_line() +
  stat_qq_point(col="black") +
  facet_wrap(~ RTHLTH53, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()


#plotting the distro of expenses on the health levels reported
meps_subset_2019_rm %>%
  ggplot(aes(x = TOTEXP19, fill = factor(RTHLTH53))) +
  geom_histogram(color = "white", alpha = 0.5, bins = 25) +
  theme_classic() +
  facet_wrap(~ RTHLTH53, labeller = labeller(RTHLTH53 = c("1" = "Excellent", 
                "2" = "Very Good", "3" = "Good", "4" = "Fair", "5" = "Poor"))) +
  labs(title = "Histogram of Expenses by Health Level", x = "Tot Expenses", y = "Count")

#do a log transformation on this skewed variable
#but before doing log, replace any 0's with something small so log will work on those values
meps_subset_2019_rm$TOTEXP19 <- ifelse(meps_subset_2019_rm$TOTEXP19 <= 0, 0.1, meps_subset_2019_rm$TOTEXP19)
meps_subset_2019_rm$log_exp <- log(meps_subset_2019_rm$TOTEXP19)


#replotting the distro of expenses on the health levels reported after transform
meps_subset_2019_rm %>%
  ggplot(aes(x = log_exp, fill = factor(RTHLTH53))) +
  geom_histogram(color = "white", alpha = 0.5, bins = 25) +
  theme_classic() +
  facet_wrap(~ RTHLTH53, labeller = labeller(RTHLTH53 = c("1" = "Excellent",
          "2" = "Very Good", "3" = "Good", "4" = "Fair", "5" = "Poor"))) +
  labs(title = "Histogram of Expenses by Health Level", x = "Log Tot Expenses", y = "Count")


meps_subset_2019_rm$HealthFactor <- as.factor(meps_subset_2019_rm$RTHLTH53)

#Homogeneity of variances
#Use the Levene’s test to check the homogeneity of variances. 
#The function leveneTest() [in car package] will be used:
#install.packages("car")
library(car)
#?leveneTest
leveneTest(TOTEXP19 ~ HealthFactor, data = meps_subset_2019_rm)
#with an F-value of 375.03 and a very small p-value 2.2e-16 this
#shows that the variances between groups are not the same

leveneTest(log_exp ~ HealthFactor, data = meps_subset_2019_rm)
#and here as well, with an F-value of 48.562 and a very small p-value 2.2e-16 this also
#shows that the variances between groups are not the same - but guess we'll just go with it


#Perform one-way ANOVA to test group differences
res.aov2 <- aov(TOTEXP19 ~ HealthFactor, data = meps_subset_2019_rm)
summary(res.aov2)
plot(res.aov2, 1)

res.aov22 <- aov(log_exp ~ HealthFactor, data = meps_subset_2019_rm)
summary(res.aov22)
plot(res.aov22, 1)
#Differences are showing significant at 0.05 level with Fvalue of 386.8


#Kruskal-Wallis - ANOVA - do this test since violation of unequal variances

#Perform the Kruskal-Wallis on all HealthFactor types
m1<-kruskal.test(log_exp ~ HealthFactor, data=meps_subset_2019_rm)
print(m1)
#With Chi-square of 2711 and p-value < 0.05 which shows significance that at least one
#of the groups are different

#If significant, conduct Tukeys HSD post-hoc test
TukeyHSD(res.aov22)
#the tukeys test shows each one of the effects are different as the p adj are all o
#and the lower and upper range are all positive

#use Dunns test to confirm what Tukeys test shows
#install.packages("FSA")
library(FSA)

PT = dunnTest(log_exp ~ HealthFactor, data=meps_subset_2019_rm,
              method="bh")    # Can adjust p-values; 
# See ?p.adjust for options 

PT
#this also shows each comparison is significant.  Can see p.adj are all very significant

##Calculate Effect size (eta-squared)

#Compute eta-squared for effect size
#install.packages("effectsize")  
library(effectsize)

summary(res.aov22)

#calculate eta_squared from ANOVA object
eta2 <- eta_squared(res.aov22)
print(eta2)

#calc cohen f 
cohen_f <- sqrt(eta2[[1]] / (1 - eta2[[1]]))

#print the result
print(cohen_f)

#the effect size of .2345 is a small to medium sized effect

##Perform power analysis to determine required sample size
#install.packages("pwr")
library(pwr)

#Use pwr.anova.test on cohen_f 
result <- pwr.anova.test(k = 5, f = cohen_f, power = 0.8, sig.level = 0.05)
print(result)
#print the estimated sample size per group
print(result$n)
#sample size needed for each group is 45.

##
##Question 5
##

##Physical and mental health status are closely related, yet individuals may rate 
##them differently. We aim to compare standardized self-reported physical and mental health scores.

##Is there a significant difference between an individual’s self-reported physical 
##health and mental health scores?

##Null Hypothesis (H₀): There is no difference in self-reported physical and mental health scores.
##Alternative Hypothesis (H₁): There is a significant difference between the two.

is_factor <- is.factor(meps_subset_2019$RTHLTH53)
print(is_factor)

##Standardize physical and mental health scores.
#new df with removing records with health levels unknown

meps_subset_2019_rm_miss <- meps_subset_2019[meps_subset_2019$RTHLTH53 > 0, ]
meps_subset_2019_rm_miss <- meps_subset_2019_rm_miss[meps_subset_2019_rm_miss$MNHLTH53 > 0, ]

#do contingencuy table to see what we are looking at
contingency_table <- table(meps_subset_2019_rm_miss$RTHLTH53, meps_subset_2019_rm_miss$MNHLTH53)
print(contingency_table)

#calc a mean and standard deviation on health levels
mean_RTHLTH53 <- mean(meps_subset_2019_rm_miss$RTHLTH53, na.rm = TRUE)
sd_RTHLTH53 <- sd(meps_subset_2019_rm_miss$RTHLTH53, na.rm = TRUE)

#Standardize Health
meps_subset_2019_rm_miss$RTHLTH53_standardized <- (meps_subset_2019_rm_miss$RTHLTH53 - mean_RTHLTH53) / sd_RTHLTH53

#Calc mean and standard deviation for Mntal Health
mean_MNHLTH53 <- mean(meps_subset_2019_rm_miss$MNHLTH53, na.rm = TRUE)
sd_MNHLTH53 <- sd(meps_subset_2019_rm_miss$MNHLTH53, na.rm = TRUE)

#standardize Mental Health
meps_subset_2019_rm_miss$MNHLTH53_standardized <- (meps_subset_2019_rm_miss$MNHLTH53 - mean_MNHLTH53) / sd_MNHLTH53

head(meps_subset_2019_rm_miss,20)

##Check normality of differences.
#calc diffs
meps_subset_2019_rm_miss$diffs <- meps_subset_2019_rm_miss$RTHLTH53_standardized - 
  meps_subset_2019_rm_miss$MNHLTH53_standardized

hist(meps_subset_2019_rm_miss$diffs, probability = TRUE, main = "Histogram of Diffs", xlab = "Differences")
#histogram is showing normality in differnces

qqnorm(meps_subset_2019_rm_miss$diffs, main = "QQ Plot Differences Phys and Mental health")
qqline(meps_subset_2019_rm_miss$diffs, col = "red")
#The QQ Plot, because there are so many observations migth be a little misleading to see
#so many off the diagonal but still appears pretty normal

##Perform paired t-test
t_test_result <- t.test(meps_subset_2019_rm_miss$RTHLTH53_standardized, 
                        meps_subset_2019_rm_miss$MNHLTH53_standardized, paired = TRUE)
print(t_test_result)
#the t-test shows a value of -1.537e-14 and p-value of 1, indicating there is not 
#sufficient evidence to reject the Null hypothesis that the differences between the 
#report health and mental health are different from one another

##Calculate Cohen’s d effect size.

#compute mean and sd of the diffs
mean_diff <- mean(meps_subset_2019_rm_miss$diffs)
sd_diff <- sd(meps_subset_2019_rm_miss$diffs)

#calc Cohens D
cohens_d <- mean_diff/sd_diff

#print result
print(cohens_d)
#Since the magnitude of the difference of this result is near 0, there is no real effect
#in the differences between the reported health measures.

##Conduct power analysis.
library("pwr")
pwr.t.test(power=0.80,d=0.0000000000000000915,sig.level=0.05,alternative="two.sided")
#assume i'm getting an error here since the d value is so tiny that there wouldnt be
# a valid meaningful power analyses.  Maybe something not right with my cohens d?

##
##Question 6
##

#Individuals with hypertension often have higher medical costs due to ongoing treatments 
#and medications. However, is this difference significant?

#Do individuals with hypertension have significantly different healthcare expenditures 
#compared to those without?

#Null Hypothesis (H₀): There is no difference in median healthcare expenditures between
#individuals with and without hypertension.

#Alternative Hypothesis (H₁): There is a significant difference.

#see table of this var
table(meps_subset_2019$HIBPDX)

#create df of those with valid hypertension values (adults)
meps_subset_2019_adults <- meps_subset_2019[meps_subset_2019$HIBPDX < 99, ]

##Compare distributions of expenditures
meps_subset_2019_adults %>%
  ggplot(aes(x = TOTEXP19, fill = factor(HIBPDX))) +
  geom_histogram(color = "white", alpha = 0.5, bins = 25) +
  theme_classic() +
  facet_wrap(~ HIBPDX, labeller = labeller(HIBPDX = c("1" = "HYPR", "2" = "No HYPR"))) +
  labs(title = "Histogram of Expenses by HyperTension", x = "Tot Expenses", y = "Count")

ggplot(meps_subset_2019_adults, aes(x = HIBPDX, y = TOTEXP19, fill = HIBPDX)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Hypertension Categories") + 
  theme_bw() + theme(legend.position="none")

##Perform Mann-Whitney U test.
#Designate HIBPDX as a categorical factor
meps_subset_2019_adults$HIBPDX<-as.factor(meps_subset_2019_adults$HIBPDX)

#Produce descriptive statistics by group
meps_subset_2019_adults %>%  group_by(HIBPDX) %>% 
  summarise(n = n(), 
            mean = mean(TOTEXP19, na.rm = TRUE), 
            sd = sd(TOTEXP19, na.rm = TRUE),
            stderr = sd/sqrt(n),
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median = median(TOTEXP19, na.rm = TRUE),
            min = min(TOTEXP19, na.rm = TRUE), 
            max = max(TOTEXP19, na.rm = TRUE),
            IQR = IQR(TOTEXP19, na.rm = TRUE))

#Produce Boxplots and visually check for outliers
ggplot(meps_subset_2019_adults, aes(x = HIBPDX, y = TOTEXP19, fill = HIBPDX)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Hypertension Yes or No") + 
  theme_bw() + theme(legend.position="none")


library(qqplotr)
#Perform QQ plots by group
ggplot(data = meps_subset_2019_adults, mapping = aes(sample = TOTEXP19, color = factor(HIBPDX))) +
  stat_qq_line() +
  stat_qq_point(col="black") +
  facet_wrap(~ HIBPDX, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()

#compare group 1 vs 2, so we are subsetting the data
subset_hyper <- meps_subset_2019_adults[meps_subset_2019_adults$HIBPDX %in% c("1", "2"), ]

#convert to factor
subset_hyper$HIBPDX <- as.factor(subset_hyper$HIBPDX)

?wilcox.test
#finally, Perform the Mann-Whitney U test
m1 <- wilcox.test(TOTEXP19 ~ HIBPDX, data=subset_hyper, na.rm=TRUE, exact=FALSE, conf.int=TRUE)
print(m1)
#this result is indicating that we reject the null hypothesis and accept the alternative
#that there is a difference in central tendency between the 2 groups of hypertension
#the confidence interval between 2261 and 2538 on the total expenses between the groups

##Calculate effect size (r).
#get necessary packages
#install.packages("rstatix")
library(rstatix)
#install.packages("coin")
library(coin)
#run the effsize function on the dataframe
effect_size_result <- subset_hyper %>%
  wilcox_effsize(TOTEXP19 ~ HIBPDX)
print(effect_size_result)
#effect size is 0.337 which is a medium size effect

##Conduct power analysis.
#install.packages("wmwpow")
library(wmwpow)

#define parameters
n1 <- 7719  #size for group 1
n2 <- 13974  #size for group 2
#get the effect size value
r_value <- effect_size_result$effsize

#calculate probability p
p_value <- (r_value + 1) / 2
alpha <- 0.05  #sig level

#calculate power
power_result <- shiehpow(n = n1, m = n2, p = p_value, alpha = alpha, dist = "norm", sides = "two.sided")
print(power_result)

##
##Question 7
##

#A. Hidden Markov Models are used to solve for three fundamental problems, 
#what are they and what algorithms do we use to solve for each?

#1 - Likelihood (or probability) of an observed sequence - ie eating 4, 1, 3 ice creams in successive days
# Algorithm to use is the Forward algorithm

#2 - Decoding (or most likely series of states) given the observed outputs 
# Algorithm is Forward-Backward or Viterbi algorithm

#3 - Parameter Learning - given observations, what are the values of state
# transition probabilities and output probabilities to make observed most likely
# Algorithm is the Baum-Welsh (iterative implementation of forward-backward)

#B Given the following hidden markov model
#What algorithm does this represent
#P(Xk+1:n|Sk,X1:k)⋅P(Sk,X1:k)
#This represents the Forward-Backward algorithm

##
##Question 8
##

##Question 8A
#States       Mean       Covariances
#Resting      60.02       26.03
#Walking      90.08       202.95
#Exercising   140.05      563.46

##Question 8B
#For hours 75-79 the patient would be resting

##See other file for code on this problem##













