#############################################################
# Classification Methods Essentials
# Logistic Regression Essentials in R
# http://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r
#############################################################
# In this chapter you’ll learn how to:
  
# Define the logistic regression equation and key terms such as log-odds and logit
# Perform logistic regression in R and interpret the results
# Make predictions on new test data and evaluate the model accuracy

library(tidyverse)
library(caret)
library(glmnet)

# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE) # A series of test/training partitions are created, 80% percent.
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]


################# Quick start R code################
# Fit the model
model <- glm( diabetes ~., data = train.data, family = binomial)
# Summarize the model
summary(model)
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == test.data$diabetes)

################Simple logistic regression ##############
# The simple logistic regression is used to predict the probability of class membership based on one single predictor variable.
# The following R code builds a model to predict the probability of being diabetes-positive based on the plasma glucose concentration:
model <- glm( diabetes ~ glucose, data = train.data, family = binomial)
summary(model)$coef
# The output above shows the estimate of the regression beta coefficients and their significance levels. 
# The intercept (b0) is -6.32 and the coefficient of glucose variable is 0.043.
# The logistic equation can be written as p = exp(-6.32 + 0.043*glucose)/ [1 + exp(-6.32 + 0.043*glucose)]. 
# Using this formula, for each new glucose plasma concentration value, you can predict the probability of the individuals in being diabetes positive.
# Predictions can be easily made using the function predict(). Use the option type = “response” to directly obtain the probabilities
newdata <- data.frame(glucose = c(20,  180))
probabilities <- model %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes
# The logistic function gives an s-shaped probability curve illustrated as follow:
train.data %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )


################### Multiple logistic regression #####################

# The multiple logistic regression is used to predict the probability of class membership based on multiple predictor variables, as follow:
  
model <- glm( diabetes ~ glucose + mass + pregnant, 
                data = train.data, family = binomial)
summary(model)$coef

# Here, we want to include all the predictor variables available in the data set. This is done using ~.:
  
model <- glm( diabetes ~., data = train.data, family = binomial)
summary(model)$coef

##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -9.50372    1.31719  -7.215 5.39e-13
## pregnant     0.04571    0.06218   0.735 4.62e-01
## glucose      0.04230    0.00657   6.439 1.20e-10
## pressure    -0.00700    0.01291  -0.542 5.87e-01
## triceps      0.01858    0.01861   0.998 3.18e-01
## insulin     -0.00159    0.00139  -1.144 2.52e-01
## mass         0.04502    0.02887   1.559 1.19e-01
## pedigree     0.96845    0.46020   2.104 3.53e-02
## age          0.04256    0.02158   1.972 4.86e-02

# From the output above, the coefficients table shows the beta coefficient estimates and their significance levels. Columns are:
  
# Estimate: the intercept (b0) and the beta coefficient estimates associated to each predictor variable
# Std.Error: the standard error of the coefficient estimates. This represents the accuracy of the coefficients. The larger the standard error, the less confident we are about the estimate.
# z value: the z-statistic, which is the coefficient estimate (column 2) divided by the standard error of the estimate (column 3)
# Pr(>|z|): The p-value corresponding to the z-statistic. The smaller the p-value, the more significant the estimate is.

# Note that, the functions coef() and summary() can be used to extract only the coefficients, as follow:
  
coef(model)
summary(model )$coef

############### Interpretation #########################

# It can be seen that only 5 out of the 8 predictors are significantly associated to the outcome. These include: pregnant, glucose, pressure, mass and pedigree.

# The coefficient estimate of the variable glucose is b = 0.045, which is positive. This means that an increase in glucose is associated with increase in the probability of being diabetes-positive. However the coefficient for the variable pressure is b = -0.007, which is negative. This means that an increase in blood pressure will be associated with a decreased probability of being diabetes-positive.

# An important concept to understand, for interpreting the logistic beta coefficients, is the odds ratio. An odds ratio measures the association between a predictor variable (x) and the outcome variable (y). It represents the ratio of the odds that an event will occur (event = 1) given the presence of the predictor x (x = 1), compared to the odds of the event occurring in the absence of that predictor (x = 0).

# For a given predictor (say x1), the associated beta coefficient (b1) in the logistic regression function corresponds to the log of the odds ratio for that predictor.

# If the odds ratio is 2, then the odds that the event occurs (event = 1) are two times higher when the predictor x is present (x = 1) versus x is absent (x = 0).

# For example, the regression coefficient for glucose is 0.042. This indicate that one unit increase in the glucose concentration will increase the odds of being diabetes-positive by exp(0.042) 1.04 times.

# From the logistic regression results, it can be noticed that some variables - triceps, insulin and age - are not statistically significant. Keeping them in the model may contribute to overfitting. Therefore, they should be eliminated. This can be done automatically using statistical techniques, including stepwise regression and penalized regression methods. This methods are described in the next section. Briefly, they consist of selecting an optimal model with a reduced set of variables, without compromising the model curacy.

# Here, as we have a small number of predictors (n = 9), we can select manually the most significant:
model <- glm( diabetes ~ pregnant + glucose + pressure + mass + pedigree, 
              data = train.data, family = binomial)

################ Making predictions #####################

# We’ll make predictions using the test data in order to evaluate the performance of our logistic regression model.

# The procedure is as follow:
  
# Predict the class membership probabilities of observations based on predictor variables
# Assign the observations to the class with highest probability score (i.e above 0.5)

# The R function predict() can be used to predict the probability of being diabetes-positive, given the predictor values.

# Predict the probabilities of being diabetes-positive:
probabilities <- model %>% predict(test.data, type = "response")
head(probabilities)
##     21     25     28     29     32     36 
## 0.3914 0.6706 0.0501 0.5735 0.6444 0.1494
# Which classes do these probabilities refer to? In our example, the output is the probability that the diabetes test will be positive. 
# We know that these values correspond to the probability of the test to be positive, 
# rather than negative, because the contrasts() function indicates that R has created a dummy variable with a 1 for “pos” and “0” for neg.
# The probabilities always refer to the class dummy-coded as “1”.
# Check the dummy coding:
contrasts(test.data$diabetes)

############## Predict the class of individuals: ###############
  
# The following R code categorizes individuals into two groups based on their predicted probabilities (p) of being diabetes-positive. 
# Individuals, with p above 0.5 (random guessing), are considered as diabetes-positive.

predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
##    21    25    28    29    32    36 
## "neg" "pos" "neg" "pos" "pos" "neg"

################# Assessing model accuracy #####################

# The model accuracy is measured as the proportion of observations that have been correctly classified. 
# Inversely, the classification error is defined as the proportion of observations that have been misclassified.

# Proportion of correctly classified observations:
  
mean(predicted.classes == test.data$diabetes)

## [1] 0.756