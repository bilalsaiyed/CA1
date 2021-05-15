# Analyzing and dealing with the data of Stroke
# to visualize the ratio and chances of people getting
# Stroke with the different variables and get them
# to know the functioning.

# Importing & Installing the required packages & libraries
# install.packages("dplyr")
library(dplyr)

# Importing the dataset into a DF
stroke_data <- read.csv("stroke.csv", na = "")

# Structure of DF
str(stroke_data)

# Displaying the number of rows and columns
nrow(stroke_data)
ncol(stroke_data)

# Displaying the summary
summary(stroke_data)

# To check if any NA data present
incomplete_data <- stroke_data[!complete.cases(stroke_data),]
incomplete_data

# Display the missing data in rows
nrow(incomplete_data)

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

# install.packages("naniar")
library(naniar)
miss_scan_count(data = stroke_data , search = list("N/A", "Unknown"))

# Data Pre-processing:
# 1. id and Date attributes 
# As these attributes were used to identify the patients
# records only, hence they can be dropped for further processing
new_stroke_data <- stroke_data[, c(2,3,4,5,6,7,8,9,10,11,12)]

# 2. bmi attribute:
# Removing the N/A values from bmi attribute which account for 3.9% of all values
new_stroke_data <- new_stroke_data[new_stroke_data$bmi != "N/A", ]
str(new_stroke_data)
# Removed 201 rows with N/A value from the data frame
# Converting the bmi attribute from character to numeric
new_stroke_data["bmi"] <- as.numeric(new_stroke_data$bmi)

# 3. Renaming the Residence_type attribute
new_stroke_data <- new_stroke_data %>% rename("residence_type" = "Residence_type")
str(new_stroke_data)

# 4. gender attribute:
# Removing patients who were categorized as ‘Other’ in the gender column
new_stroke_data = filter(new_stroke_data, gender!='Other')
str(new_stroke_data)

# 5. Factoring the columns
new_stroke_data$gender = as.factor(new_stroke_data$gender)
new_stroke_data$ever_married = as.factor(new_stroke_data$ever_married)
new_stroke_data$work_type = as.factor(new_stroke_data$work_type)
new_stroke_data$residence_type = as.factor(new_stroke_data$residence_type)
new_stroke_data$smoking_status = as.factor(new_stroke_data$smoking_status)
new_stroke_data$hypertension = as.factor(new_stroke_data$hypertension)
#new_stroke_data$heart_disease = as.factor(new_stroke_data$heart_disease)
str(new_stroke_data)

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(new_stroke_data))

# visualize the missing data
# install.packages("VIM")
library(VIM)
missing_values <- aggr(new_stroke_data, prop = FALSE, numbers = TRUE)

# Display the summary of missing data
summary(missing_values)

# Display the structure of DF
str(new_stroke_data)

# Installing the library 'psych'
# install.packages("psych")
library(psych)

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


############# Question 1:
# Are aged people more likely to get a stroke than younger people 
############# 
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
plot(stroke, age, pch = 9, col = "LightBlue", 
     main = "Comaprison of Stroke with Age", 
     xlab = "Stroke", ylab = "Age (Years)")

# We can split the dichotomous variable into 
# 2 different visualization & then examine the data
# Importing the library 'lattice'
#installed.packages("lattice")
library("lattice")

# Visualizing the variables
histogram(~age | stroke, 
          data = new_stroke_data, 
          main = "Distribution of Age with the occurence of stroke", 
          xlab = "AGE", ylab = "Stroke %")

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
            main = "Comparing 2 samples of Stroke Data", 
            xlab = "Stroke age = Yes", 
            ylab = "Stroke age = No"))

# Formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample comes from a Normal Distribution
# If p.value > 0.05 then it is normally distributed
normality_test <- shapiro.test(new_stroke_data$age)
normality_test$p.value

# We observed that p-value is < than 0.05, 
# The AGE variable is not Normally Distributed

# This test does not work on a dichotomous variable
with(new_stroke_data, tapply(age, stroke, shapiro.test))

# Results show
# Less chance of getting HA = p-value = 0.002 - It is not ND
# More chance of getting HA = p-value = 0.121 - It is ND

# After consulting the chart, I am aiming
# a dependent var(Age)
# with a independent categorical var(Stroke)
# Format wilcox.test(dependent var ~ independent var)
# wilcox.test(Age~Stroke)
kruskal.test(age~stroke, data = new_stroke_data)
# cut-off = 0.05
# p-value = 3.439e-05 equals to (0.0003)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the chance of patient getting HA between the 
# age(29 to 77) is less

# Answer for Question 1:
# Thus the chance of patient getting a HA 
# between the age(29 to 77) is more.

############## Question 2:
# Are males more likely to get a stroke than females
############## 
# H0 = Males and females are equally likely to get a stroke
# H1 = Males have more chances to get a stroke than females

# Convert the gender variable to
# a categorical dichotomous variable with appropriate labels
# 0 = Female and 1 = Male
#new_stroke_data$gender <- factor(new_stroke_data$gender, 
#                             labels = c("Female", 
#                                        "Male"))

# Structure of the DF
str(new_stroke_data)

# Analyze the gender of the patients
table(new_stroke_data$gender)

# Analyzing the chances of stroke with gender
table(new_stroke_data$stroke, new_stroke_data$gender)

# Plot the graph to analyze the specified attributes
plot(stroke, gender, pch = 9, col = "LightBlue", 
     main = "Comaprison of Gender with Stroke", 
     xlab = "Stroke", ylab = "Gender")

# Visualizing the variables
histogram(~gender | stroke, 
          data = new_stroke_data, 
          main = "Distribution of the gender with Stroke", 
          xlab = "Gender", ylab = "Count of patients getting a stroke") 


str(new_stroke_data)

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(gender)

# Add the line to show if data is ND
qqline(gender, col = "red")
# The gender(sex) field is not normally distributed

# Visual analysis seems to indicate that the 
# data is Normally Distributed
# summarizing it below
tapply(gender, stroke, median)

# Applying the chi-square statistic with the function
# it can be applied as both are the categorical variables
chisq <- chisq.test(new_stroke_data$gender, new_stroke_data$stroke)
chisq

# Observed count values for the hypothesis
chisq$observed

# Expected count of the values for the hypothesis
round(chisq$expected)

# Visualize the pearsons residuals
round(chisq$residuals)

# Print the p.value
chisq$p.value

# cut-off = 0.05
# p-value = 1.876e-06 equals to (0.0018)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the chance of males getting HA is less as compared to female

# Answer to Question 2:
# Thus the chance of male patient getting HA 
# is more then with female.

################# Question 3:
# Increased Average glucose level leads to heart diseases which may
# eventually lead to a stroke
#################
# normal range of Glucose level <= 140
# Diabetic range of Glucose level > 200
# H0 = Increased average glucose level leads to heart diseases
# H1 = Increased average glucose level has no effect on heart disease

# heart_disease variable is a categorical dichotomous variable with following labels:
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
plot(heart_disease, avg_glucose_level, pch = 9, col = "LightBlue", 
     main = "Comparison of the glucose level v/s Heart Disease", 
     xlab = "Glucose Level", ylab = "Heart Disease Status")

# Analyzing the distribution of the variables
histogram(~avg_glucose_level | heart_disease, 
          data = new_stroke_data, 
          main = "Distribution of Average Glucose Level v/s Heart Disease", 
          xlab = "Average Glucose Level(mmol/L)", ylab = "Number of patients having Heart Diseases")

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
            main = "Comparing 2 samples of Stroke Data", 
            xlab = "High Average Glucose Level heart disease = More chance of Heart Disease", 
            ylab = "Low Average Glucose Level heart disease = Less chance of Heart Disease"))

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
# Patient does not have heart disease with low average glucose level = p-value < 0.05 - it is not ND
# Patient has heart disease with high average glucose level = p-value < 0.05 - it is not ND

# After examining for an dependent var(Average Glucose level)
# with an independent categorical var(Heart disease)
# Format for the test is: wilcox.test(dependent var ~ independent var)
wilcox.test(avg_glucose_level~heart_disease)
# p-value = 0.000006

# cut-off = 0.05
# p-value < 0.05 thus we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the rise in average glucose level has no effect on heart disease

# Answer for Question 3:
# Thus Increased average glucose level has no effect on heart disease