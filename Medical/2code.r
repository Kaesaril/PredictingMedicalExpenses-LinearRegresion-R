##### Chapter 6: Regression Methods -------------------

#### Part 1: Linear Regression -------------------


## Example: Predicting Medical Expenses ----

##Health insurance companies aim to make a profit by collecting more money in yearly premiums than they pay out for medical care. To do this, they use data and models to predict how much they will need to spend on medical expenses for different groups of people. Some health conditions are more common in certain groups, like lung cancer in smokers or heart disease in obese individuals.

##The goal of this analysis is to use patient data to estimate
##the average medical expenses for these different groups. 
##These estimates help determine how much people should pay for
##their health insurance premiums. In the first step, data is collected, and in this case, a simulated dataset is used based on real-world statistics from the U.S. Census Bureau.
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable
summary(insurance$expenses)

# histogram of insurance charges Our model's dependent variable is expenses, which measures the medical costs each person charged to the insurance plan for the year.
hist(insurance$expenses)

# table of region
table(insurance$region)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

##None of the correlations in the matrix are very strong, but there are some notable associations. For instance, age and bmi appear to have a weak positive correlation, meaning that as someone ages, their body mass tends to increase. There are also positive correlations between age and expenses, bmi and expenses, and children and expenses.

#An alternative is to create a scatterplot matrix (sometimes abbreviated as SPLOM), which is simply a collection of scatterplots arranged in a grid. It is used to detect patterns among three or more variables
##We can use R's graphical capabilities to create a scatterplot matrix for the four numeric features: age, bmi, children, and expenses. The pairs() function is provided in the default R installation and provides basic functionality for producing scatterplot matrices. To invoke the function, simply provide it the data frame to plot
pairs(insurance[c("age", "bmi", "children", "expenses")])

## The relationship between age and expenses displays several relatively straight lines, while the bmi versus expenses plot has two distinct groups of points.

# more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

## Step 3: Training a model on the data ---- To fit a linear regression model to data with R, the lm() function can be used. This is part of the stats package, which should be included and loaded by default with your R installation
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)
ins_model <- lm(expenses ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
ins_model

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

# making predictions with the regression model
insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)

##The correlation of 0.93 suggests a very strong linear relationship between the predicted and actual values

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

##We can see here that a small number of patients with much larger-than-expected medical expenses are balanced by a larger number of patients with slightly smaller-than-expected expenses.
predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model2,
        data.frame(age = 30, age2 = 30^2, children = 0,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))
