# Determining MCAR or MAR or not random at all nature of our data.
#
# This (short) script performs Little's MCAR test and if needed logistic regression
# (MAR tests) on the dataset and prints the results.

# Set seed for reproducibility ----
set.seed(123)

# Set sink to capture all output in a text file ----
sink("data/analysis/MCAR_MAR.txt")

# Load libraries ----
library(tidyverse)
library(naniar)

# Load data ----
data <- read_csv("data/merged/merged_data_with_gender.csv")

# Perform MCAR test ----
result <- mcar_test(data) # p-value is 0.04!! so we reject 
# the null hypothesis of MCAR, but the p-value is close to 0.05, 
# so we cannot be sure that the data is not MCAR. SO, still
# need to perform a logistic regression like specified in our 
# preregistration plan to determine if data is MAR

write_csv(result, "data/analysis/mcar_test_results.csv")

# Perform logistic regressions for MAR test ----
# Emotion-general context ----
# Create a binary variable indicating missingness for the 
# mediators and the outcome variable
data <- data %>%
  mutate(mediator_ada_missing = if_else(is.na(adaptive_score), 1, 0),
         mediator_mal_missing = if_else(is.na(maladaptive_score), 1, 0),
         outcome_dep_missing = if_else(is.na(depression_score), 1, 0))
print(table(data$mediator_ada_missing))
print(table(data$mediator_mal_missing))
print(table(data$outcome_dep_missing))

# Fit logistic regression models for each missingness variable
# First ensure gender is a factor
data$sex <- as.factor(data$sex)

# Logistic regression for mediator_ada_missing
model_ada <- glm(mediator_ada_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_ada))

# Logistic regression for mediator_mal_missing
model_mal <- glm(mediator_mal_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_mal))

# Logistic regression for outcome_dep_missing
model_dep <- glm(outcome_dep_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex,
                    data = data, family = binomial(link = "logit"))
print(summary(model_dep))

# Sadness-specific context ----
# Create a binary variable indicating missingness for the 
# mediators and the outcome variable
data <- data %>%
  mutate(mediator_ada_missing = if_else(is.na(sadness_adaptive_score), 1, 0),
         mediator_mal_missing = if_else(is.na(sadness_maladaptive_score), 1, 0),
         outcome_dep_missing = if_else(is.na(depression_score), 1, 0))
print(table(data$mediator_ada_missing))
print(table(data$mediator_mal_missing))
print(table(data$outcome_dep_missing))

# Fit logistic regression models for each missingness variable
# First ensure gender is a factor
data$sex <- as.factor(data$sex)

# Logistic regression for mediator_ada_missing
model_ada <- glm(mediator_ada_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_ada))

# Logistic regression for mediator_mal_missing
model_mal <- glm(mediator_mal_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_mal))

# Logistic regression for outcome_dep_missing
model_dep <- glm(outcome_dep_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex,
                    data = data, family = binomial(link = "logit"))
print(summary(model_dep))

# Anxiety-specific context ----
# Create a binary variable indicating missingness for the 
# mediators and the outcome variable
data <- data %>%
  mutate(mediator_ada_missing = if_else(is.na(anxiety_adaptive_score), 1, 0),
         mediator_mal_missing = if_else(is.na(anxiety_maladaptive_score), 1, 0),
         outcome_dep_missing = if_else(is.na(depression_score), 1, 0))
print(table(data$mediator_ada_missing))
print(table(data$mediator_mal_missing))
print(table(data$outcome_dep_missing))

# Fit logistic regression models for each missingness variable
# First ensure gender is a factor
data$sex <- as.factor(data$sex)

# Logistic regression for mediator_ada_missing
model_ada <- glm(mediator_ada_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_ada))

# Logistic regression for mediator_mal_missing
model_mal <- glm(mediator_mal_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_mal))

# Logistic regression for outcome_dep_missing
model_dep <- glm(outcome_dep_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex,
                    data = data, family = binomial(link = "logit"))
print(summary(model_dep))

# Anger-specific context ----
# Create a binary variable indicating missingness for the 
# mediators and the outcome variable
data <- data %>%
  mutate(mediator_ada_missing = if_else(is.na(anger_adaptive_score), 1, 0),
         mediator_mal_missing = if_else(is.na(anger_maladaptive_score), 1, 0),
         outcome_dep_missing = if_else(is.na(depression_score), 1, 0))
print(table(data$mediator_ada_missing))
print(table(data$mediator_mal_missing))
print(table(data$outcome_dep_missing))

# Fit logistic regression models for each missingness variable
# First ensure gender is a factor
data$sex <- as.factor(data$sex)

# Logistic regression for mediator_ada_missing
model_ada <- glm(mediator_ada_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_ada))

# Logistic regression for mediator_mal_missing
model_mal <- glm(mediator_mal_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex, 
                    data = data, family = binomial(link = "logit"))
print(summary(model_mal))

# Logistic regression for outcome_dep_missing
model_dep <- glm(outcome_dep_missing ~ extraversion + agreeableness +
                    conscientiousness + neuroticism + openness + sex,
                    data = data, family = binomial(link = "logit"))
print(summary(model_dep))

# End sink to stop capturing output in the text file ----
sink(file = NULL)

#### SO:
# Data seems not MCAR, also seems not MAR, so need to 
# rerun all analyses with complete cases only, and then compare 
# results with the analyses with FIML data to see if they are consistent. 
# If they are consistent, then we can be more confident in our results, 
# even though the data is not MCAR or MAR. If they are not consistent, 
# then we need to be more cautious in interpreting our results, and we may 
# need to consider alternative methods for handling missing data (e.g., 
# sensitivity analyses, pattern-mixture models, etc.).