# Importing & Installing the required packages & libraries
# install.packages("naniar")
# install.packages("dplyr")
# install.packages("VIM")
# install.packages("psych")
# install.packages("lattice")
library(naniar)
library(dplyr)

# Importing the dataset into a Data frame
stroke_data <- read.csv("stroke.csv", na = "")

# Structure of Data frame
str(stroke_data)

# Displaying the number of rows and columns
nrow(stroke_data)
ncol(stroke_data)

# Displaying the summary
summary(stroke_data)

# Data Preparation:
# To check if any NA data present
incomplete_data <- stroke_data[!complete.cases(stroke_data),]
incomplete_data

# Display the missing data in rows
nrow(incomplete_data)

# Finding the missing or unknown data across different columns
miss_scan_count(data = stroke_data , search = list("N/A", "Unknown"))
# There are 1544 unknown smoking status values.
# This is too much to be removed without skewing the data

# Finding unique values across different variables
cat("Gender:")
unique(stroke_data$gender)
cat("Married:")
unique(stroke_data$ever_married)
cat("Work type:")
unique(stroke_data$work_type)
cat("Residence type:")
unique(stroke_data$Residence_type)
cat("Smoking:")
unique(stroke_data$smoking_status)

# converting the stroke data into numeric
all_numeric <- stroke_data

all_numeric <- all_numeric %>%
        mutate(gender = case_when(gender == "Male" ~1,
                                  gender == "Female" ~0,
                                  gender == "Other" ~2),
               ever_married = case_when(ever_married == "Yes" ~1,
                                        ever_married == "No" ~0),
               work_type = case_when(work_type == "children" ~0,
                                     work_type == "Never_worked" ~1,
                                     work_type == "Private" ~2,
                                     work_type == "Govt_job" ~3,
                                     work_type == "Self-employed" ~4),
               Residence_type = case_when(Residence_type == "Urban" ~1,
                                          Residence_type == "Rural" ~0),
               smoking_status = case_when(smoking_status == "never smoked" ~0,
                                          smoking_status == "formerly smoked" ~1,
                                          smoking_status == "smokes" ~2,
                                          smoking_status == "Unknown" ~3) 
        )
str(all_numeric)

# 1. Id and Date attributes 
# As these attributes were used to identify the patients
# records only, hence they can be dropped for further processing
new_stroke_data <- all_numeric[, c(2,3,4,5,6,7,8,9,10,11,12)]
str(new_stroke_data)

# 2. BMI attribute:
# Removing the N/A from bmi attribute which account for 3.9% of all values
new_stroke_data <- new_stroke_data[new_stroke_data$bmi != "N/A", ]
str(new_stroke_data)
# Removed 201 rows with N/A value from the data frame

# Converting the BMI attribute from character to numeric
new_stroke_data["bmi"] <- as.numeric(new_stroke_data$bmi)
str(new_stroke_data)

# 3. Gender attribute:
# As there is only 1 entry with "Other" value which is encoded as 2, hence
# Removing patients who were categorized as ‘Other’ in the gender column
new_stroke_data = filter(new_stroke_data, gender!= 2)
str(new_stroke_data)

# 4. Residence_type attribute:
# Renaming the variable
new_stroke_data <- new_stroke_data %>% rename("residence_type" = "Residence_type")
# Factoring the variable
new_stroke_data$residence_type = as.factor(new_stroke_data$residence_type)
str(new_stroke_data)

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(new_stroke_data))

# visualizing the missing data
library(VIM)
missing_values <- aggr(new_stroke_data, prop = FALSE, numbers = TRUE)

# Display the summary of missing data
summary(missing_values)

# Making use of psych library
library(psych)

# Finding correlation between different variables
pairs.panels(new_stroke_data, 
             smooth = FALSE, # If TRUE, draws less smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman", # Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered
             factor = 2, # Jittering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals


#####################################################################
# Q1: Are aged people more likely to get a stroke than younger people 
#####################################################################
# H0 = Elder and younger people are equally likely to get a stroke
# H1 = Elder people are more likely to get a stroke than younger people

# Stroke variable is a categorical dichotomous variable with following labels:
# 0 = Patients did not have a stroke before; 1 = Patients had a stroke before
new_stroke_data$stroke <- factor(new_stroke_data$stroke, 
                                 labels = c("Patients did not have a stroke before", 
                                            "Patients had a stroke before"))

# Displaying the structure of DF
str(new_stroke_data)

# Displays the value of patients who got a stroke &
# who did not get a stroke
table(new_stroke_data$stroke)

# Displays the Age of the patients
table(new_stroke_data$age)

# Displays the chance of getting a stroke according to there age
table(new_stroke_data$age, new_stroke_data$stroke)

# Attach the DF with the function
attach(new_stroke_data)

# Plot the graph to analyze the specified attributes
plot(stroke, age, pch = 9, col = "yellow", 
     main = "Comaprison of Stroke with Age", 
     xlab = "Stroke", ylab = "Age (in Years)")

# We can split the dichotomous variable into 
# 2 different visualization & then examine the data
# Importing the library 'lattice'
library("lattice")

# Visualizing the variables
histogram(~age | stroke, 
          data = new_stroke_data, 
          main = "Distribution of Age with the occurrence of stroke", 
          xlab = "Age (in Years)", ylab = "Stroke %")

# Visual analysis seems to indicate that the 
# data is Normally Distributed
# summarizing it below
tapply(age, stroke, median)

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(age)

# Add a line that represents the Normal Distribution
qqline(age, col = "red")
# Assuming that AGE variable is Normally Distributed

# Visualizing the Q-Q-Plot for Normally Distributed variable
with(new_stroke_data,
     {qqnorm(age, 
             main ="Normal Q-Q-Plot of Age", 
             xlab = "Theoritical Quantities", 
             ylab = "Sample Quantities")
       qqline(age)
     })

# Comparing the two variables
with(new_stroke_data, 
     qqplot(age[stroke == "Patients did not have a stroke before"], 
            age[stroke == "Patients had a stroke before"], 
            main = "Comparing 2 variables of Stroke Data", 
            xlab = "Age (Stroke = Yes)", 
            ylab = "Age (Stroke = No)"))

# Formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample comes from a Normal Distribution
# If p.value > 0.05 then it is normally distributed
normality_test <- shapiro.test(new_stroke_data$age)
normality_test$p.value

# We observed that p-value is 1.25e-31 < than 0.05, thus
# The age variable is not Normally Distributed

# This test does not work on a dichotomous variable
with(new_stroke_data, tapply(age, stroke, shapiro.test))

# Results show
# Patients did not have a stroke before = p-value = 2.2e-16 - It is not ND
# Patients had a stroke before = p-value = 1.9e-10 - It is not ND

# As we haven't decided any dependent or independent variable, 
# we can use Kruskal-Test if we have one data as continuous and 
# other as categorical data 
kruskal.test(age~stroke, data = new_stroke_data)

# Calculating the pairwise comparisons between Age and Stroke
pairwise.wilcox.test(new_stroke_data$age, new_stroke_data$stroke, 
                     p.adjust.method = "BH")

# The pairwise comparison shows that the levels are significantly different
# cut-off = 0.05
# p-value = 0.00000000000000022 which is almost equal to 0

# As, p-value < 0.05 thus the
# Null (H0) hypothesis is rejected

# Answer for Question 1:
# Elder people are more likely to get a stroke than younger people

########################################################
# Q2: Are males more likely to get a stroke than females
########################################################
# H0 = Males and females are equally likely to get a stroke
# H1 = Males have more chances to get a stroke than females

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(gender)

# Add the line to show if data is ND
qqline(gender, col = "red")
# The gender field is not normally distributed

# Convert the gender variable to
# a categorical dichotomous variable with appropriate labels
# 0 = Female and 1 = Male
new_stroke_data$gender <- factor(new_stroke_data$gender, 
                             labels = c("Female", 
                                        "Male"))

# Structure of the DF
str(new_stroke_data)

# Analyze the gender of the patients
table(new_stroke_data$gender)

# Analyzing the chances of stroke with gender
table(new_stroke_data$stroke, new_stroke_data$gender)

attach(new_stroke_data)

# Plot the graph to analyze the specified attributes
plot(stroke, gender, pch = 9, col = "lightblue", 
     main = "Comaprison of Gender with Stroke", 
     xlab = "Stroke", ylab = "Gender (Ratio)")

# Visualizing the variables
histogram(~gender | stroke, 
          data = new_stroke_data, 
          main = "Distribution of gender with occurrence of Stroke", 
          xlab = "Gender", ylab = "Count of patients")

# Applying the chi-square statistic with the function
# it can be applied as both are the categorical variables
chisq <- chisq.test(gender, stroke)
chisq

# Observed count values for the hypothesis
chisq$observed

# Expected count of the values for the hypothesis
round(chisq$expected)

# Visualize the Pearsons residuals
round(chisq$residuals)

# Print the p.value
chisq$p.value

# cut-off = 0.05
# p-value = 0.68
# p-value > 0.05 thus we, Accept the H0

# p-value > 0.05 indicates that the
# Null (H0) hypothesis is accepted
# This indicates that males and females are equally likely to get a stroke

# Answer to Question 2:
# Thus the chance of a male patient getting a
# stroke is same as that of a female patient.

##################################################################################
# Q3: Does increased average glucose level indicate the presence of heart diseases
##################################################################################
# Normal range of Glucose level <= 140 mmol/L
# Diabetic range of Glucose level > 200 mmol/L

# H0 = Increased average glucose level shows presence of heart diseases
# H1 = Increased average glucose level is not related to presence of heart diseases

# heart_disease is a categorical dichotomous variable with following labels:
# 0 = Patient doesn't have any heart disease; 1 = Patient has heart disease
new_stroke_data$heart_disease <- factor(new_stroke_data$heart_disease, 
                                 labels = c("Patient does not have heart disease", 
                                            "Patient has heart disease"))

# Analyze the data with specified attributes
table(new_stroke_data$avg_glucose_level)
table(new_stroke_data$heart_disease)

# Comparing the analysis of the specified attributes
table(new_stroke_data$avg_glucose_level, new_stroke_data$heart_disease)

# Plot the graph to analyze the specified attributes
boxplot(heart_disease, avg_glucose_level, pch = 9, col = "Red", 
     main = "Comparison of the Average glucose level v/s Heart Disease", 
     xlab = "Heart Disease Status", ylab = "Average glucose level(mmol/L)")

# Analyzing the distribution of the variables
histogram(~avg_glucose_level | heart_disease, 
          data = new_stroke_data, 
          main = "Distribution of Average Glucose Level with Heart Disease", 
          xlab = "Average Glucose Level(mmol/L)", 
          ylab = "Number of patients")

# Quantile-quantile plot (Q-Q-Plot) allows us to check
# if the data is Normally Distributed or not
qqnorm(avg_glucose_level)

# Adding the line that represents the Normal Distribution
qqline(avg_glucose_level, col = "red")
# Assume it is Normally Distributed

# Comparing the two variables
with(new_stroke_data, 
     qqplot(avg_glucose_level[heart_disease == "Patient has heart disease"], 
            avg_glucose_level[heart_disease == "Patient does not have heart disease"], 
            main = "Comparing 2 variables of Stroke Data", 
            xlab = "High Average Glucose Level (heart disease = Presence of Heart Disease)", 
            ylab = "Low Average Glucose Level (heart disease = Absence of Heart Disease)"))

# Formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample comes from a ND
# If p.value > 0.05 then it is normally distributed
normality_test <- shapiro.test(new_stroke_data$avg_glucose_level)
normality_test$p.value
# p.value = 1.147224e-60 < 0.05
# Hence, it is not Normally Distributed

# This test does not work on a dichotomous variable
with(new_stroke_data, tapply(avg_glucose_level, heart_disease, shapiro.test))

# Results show
# Patient does not have heart disease with low average glucose level = p-value < 0.05 - It is not ND
# Patient has heart disease with high average glucose level = p-value < 0.05 - It is not ND

# After examining that the dependent var(Average Glucose level) is not normally distributed,
# we choose the Non-parametric Kruskal- Wallis test
# with the independent categorical variable Heart disease
kruskal.test(avg_glucose_level~heart_disease, data = new_stroke_data)

# p-value = 0.000006
# cut-off = 0.05
# p-value < 0.05 thus the Null (H0) hypothesis is rejected
# So, increased average glucose level has no effect on heart disease

# Answer for Question 3:
# Increased average glucose level does not indicate presence of heart disease

###########################################################
# Q4: Is work-type related to a patient having hypertension
###########################################################
# H0 = Hypertension has positive correlation to Work type
# H1 = Hypertension and Work Type are not correlated

# work_type: "children" ~0,"Never_worked" ~1,"Private" ~2,
# "Govt_job" ~3,"Self-employed" ~4

# hypertension: "Doesn't have" ~0,"Have" ~1
# Convert the hypertension variable to
# a categorical dichotomous variable with appropriate labels
# 0 = Patients not having hypertension and  1 = Patients having hypertension
new_stroke_data$hypertension <- factor(new_stroke_data$hypertension, 
                                       labels = c("Patients not having hypertension", 
                                                  "Patients having hypertension"))

# Structure of the DF
str(new_stroke_data)

# Analyze the work_type of the patients
table(new_stroke_data$work_type)

# Analyzing smoking_status with work_type
table(new_stroke_data$hypertension, new_stroke_data$work_type)

# Plot the graph to analyze the specified attributes
plot(hypertension, work_type, pch = 15, col = "blue", 
     main = "Comaprison of Work type with Hypertension", 
     xlab = "Hypertension", ylab = "Work Type")

# Visualizing the variables
histogram(~work_type | hypertension, 
          data = new_stroke_data, 
          main = "Distribution of Work Type with Hypertension", 
          xlab = "Work Type", ylab = "Count of patients")

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(work_type)

# Add the line to show if data is Normally Distributed
qqline(work_type, col = "red")
# The work_type field is not normally distributed

# Applying the chi-square statistic with the function
# it can be applied as both are categorical variables
chisq1 <- chisq.test(work_type, hypertension)
chisq1

# Observed count values for the hypothesis
chisq1$observed

# Expected count of the values for the hypothesis
round(chisq1$expected)

# Visualize the Pearsons residuals
round(chisq1$residuals)

# Print the p.value
chisq1$p.value

# cut-off = 0.05
# p-value = 0.000023
# As, p-value < 0.05 thus, we Reject the H0

# Answer to Question 4:
# Thus, Hypertension has no correlation to work-type

#########################################################
# Q5: Are married people more likely to have hypertension
#########################################################
# H0 = Marital status has positive correlation with hypertension
# H1 = Marital status and hypertension are not correlated

# hypertension: "Doesn't have" ~0,"Have" ~1
# ever_married: "Yes" ~1: Married,"No" ~0: Unmarried

# Convert the ever_married variable to
# a categorical dichotomous variable with appropriate labels
# 0 = Unmarried and  1 = Married
new_stroke_data$ever_married <- factor(new_stroke_data$ever_married, 
                                 labels = c("Unmarried", 
                                            "Married"))

# Structure of the DF
str(new_stroke_data)

# Analyze the married status of the patients
table(new_stroke_data$ever_married)

# Analyzing hypertension with ever_married
table(new_stroke_data$hypertension, new_stroke_data$ever_married)

# Plot the graph to analyze the specified attributes
plot(hypertension, ever_married, pch = 15, col = "blue", 
     main = "Comaprison of Marital status with hypertension", 
     xlab = "Hypertension", ylab = "Marital status")

# Visualizing the variables
histogram(~ever_married | hypertension, 
          data = new_stroke_data, 
          main = "Distribution of Marital status with Hypertension", 
          xlab = "Marital status", ylab = "Count of people")

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(ever_married)

# Add the line to show if data is ND
qqline(ever_married, col = "red")
# The ever_married field is not normally distributed

# Applying the chi-square statistic with the function
# it can be applied as both are categorical variables
chisq2 <- chisq.test(ever_married, hypertension)
chisq2

# Observed count values for the hypothesis
chisq2$observed

# Expected count of the values for the hypothesis
round(chisq2$expected)

# Visualize the Pearsons residuals
round(chisq2$residuals)

# Print the p.value
chisq2$p.value

# cut-off = 0.05
# p-value = 1.02e-29 which is almost equal to 0
# As, p-value < 0.05 then we, Reject the H0

# Answer to Question 5:
# Thus, the marital status has no correlation with hypertension

#############################################
# Q6: Does age influence the BMI of a patient
#############################################

#H0: Age is positively correlated with BMI of a patient
#H1: Age has no correlation to BMI of a patient

# Converting the age attribute from numeric to integer
new_stroke_data["age"] <- as.integer(new_stroke_data$age)
# Converting the bmi attribute from numeric to integer
new_stroke_data["bmi"] <- as.integer(new_stroke_data$bmi)
str(new_stroke_data)

# Plot the graph to analyze the specified attributes
plot(age, bmi, pch = 19, col ="orange", 
     main = "Comaprison of Age with BMI", 
     xlab = "Age (in years)", ylab = "BMI(kg/m2)")


# Visualizing the variables separately using hist function
# for Age
hist(age, col = "green", main = "Distributation of Age", 
     xlab = "Age (in years)")
# for BMI
hist(bmi, col = "green", main = "Distribution of BMI", 
     xlab = "BMI (kg/m2)")

# Visual analysis of the data
histogram(~age | bmi,
          data = new_stroke_data,
          main = "Distributation of Age v/s BMI",
          xlab = "Age (in years)" ,ylab = "BMI (kg/m2)")

# Quantile-quantile plot (Q-Q-Plot) allows us to check
# if the data is Normally Distributed or not
# Adding the line that represent the ND of data using
# function qqplot() & qqline()
with(new_stroke_data,
     {qqnorm(new_stroke_data$age, 
             main ="Normal Q-Q-Plot of Age", 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles")
             qqline(new_stroke_data$age)
     })

with(new_stroke_data,
     {qqnorm(new_stroke_data$bmi, 
             main ="Normal Q-Q-Plot of BMI", 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles")
             qqline(new_stroke_data$bmi)
     })

# Examine the linear correlation between both vars
with(new_stroke_data, qqplot(age, bmi))

# Testing the linearity of the variables
# We can run the formal test of normality provided through 
# the widely used Shapiro-wilks test
normality_test <- shapiro.test(new_stroke_data$age)
normality_test$p.value
# For this variable p-value = 0.000003

normality_test <- shapiro.test(new_stroke_data$bmi)
normality_test$p.value
# For this variable p-value = 0.005

# p-values tells us the chance that the sample comes from Normal Distribution
# If p-value < 0.05 then the variable is not Normally Distributed
# If p-value > 0.05 then the variable is Normally Distributed
shapiro.test(new_stroke_data$age)
# 0.000003 < 0.05
# Thus Age is not Normally Distributed
shapiro.test(new_stroke_data$bmi)
# 0.005 < 0.05
# # Thus BMI is not Normally Distributed

# If both the variables are not Normally Distributed,
# "Spearman" correlation method test is used
# Dependent var = BMI
# Independent var = Age
cor.test(age, bmi, method = "spearman")
# cut-off = 0.05
# As, p-value is 0.00000000000000022 < 0.05
# Hence, we will Reject (H0)
# Thus Age and BMI of a patient are not correlated

# Answer to question 6:
# Thus, we can state that Age has no correlation to BMI of a patient

# Saving the modified file of the data worked on
write.csv(new_stroke_data, file = "new_stroke_data.csv")