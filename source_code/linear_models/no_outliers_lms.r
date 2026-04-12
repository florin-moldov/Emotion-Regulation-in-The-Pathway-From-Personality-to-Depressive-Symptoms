# Linear models with FIML and bootstrap confidence intervals ----

#################################################################
# Outliers removed here, based on outlier_analysis.R script
#################################################################

# Testing of first three preregistered hypotheses:
# 1. We hypothesize that Neuroticism during adolescence will be positively
# predictive of depressive symptoms during adulthood, while Conscientiousness
# and Extraversion during adolescence will be negatively predictive of
# depressive symptoms in adulthood. Relations between Openness to experience
# and Agreeableness and depressive symptoms will be assessed more
# exploratively. However, if anything, we expect Openness to experience and
# Agreeableness during adolescence to be negatively predictive of
# depressive symptoms in adulthood.
#
# 2. We hypothesize that the use of maladaptive (/adaptive) emotion
# regulation strategies in response to negative emotions in early adulthood
# will be positively (/negatively) predictive of depressive symptoms in later
# adulthood. Furthermore, we will explore whether these predictions extend
# beyond emotion regulation strategy use for general negative emotions by
# assessing them in emotion-specific context
# (i.e., while experiencing sadness versus anxiety versus anger).
#
# 3. We hypothesize that Neuroticism will positively predict the use of
# maladaptive and negatively predict the use of adaptive emotion
# regulation strategies. Conversely, Extraversion, Conscientiousness,
# Agreeableness, and Openness to experience are expected to be positively
# predictive of adaptive and negatively predictive of maladaptive emotion
# regulation strategy use. Moreover, we will explore if these predictions apply
# not only to emotion-general regulation strategy use but also to those
# emotion-specific strategy use
# (i.e., in response to sadness versus anxiety versus anger).
#
# Additionally, for all hypotheses, we control for sex effects.

# Set seed for reproducibility
set.seed(123)

# Load necessary libraries ----
library(tidyverse)
library(lavaan)
library(performance)
library(bestNormalize)
library(VGAM)

# Load data ----
data <- read_csv("data/merged/merged_data.csv")
gender <- read_csv("data/preprocessed/preproc_w10_demographics.csv")

# Add gender info to main dataset ----
# First harmonize join key type
data <- data |>
  mutate(ID = trimws(as.character(ID)))
gender <- gender |>
  mutate(ID = trimws(as.character(ID)))
data <- data |>
  left_join(gender |>
              select(ID, sex), by = "ID")

# Save the updated dataset for future use
write_csv(data, "data/merged/merged_data_with_gender.csv")

# Set sink to capture all output in a text file ----
sink("data/analysis/no_outliers_lms_output.txt")

# Define and fit the model for hypothesis 1 ----
# Filter out IDs with outliers 
outlier_ids <- read_csv("data/analysis/h1_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h1_no_outliers <- data |> 
                       filter(!ID %in% outlier_ids)

# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model
# doesn't work with check_model() function from performance package
fit_h1_check <- lm(depression_score ~ extraversion + agreeableness +
                     conscientiousness + neuroticism + openness +
                     sex, data = data_h1_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h1_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h1_check))
dev.off()

# Normality of residuals and homoscedasticity seems to be violated,
# perform formal tests to confirm
print(check_normality(fit_h1_check)) # not OK
print(check_heteroscedasticity(fit_h1_check)) # not OK

# Given violation of residual normality and heteroscedasticity, determine best transformation
# method for depression_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h1_no_outliers$depression_score)
# Fit on non-missing values only
bn_dep <- bestNormalize(data_h1_no_outliers$depression_score[non_missing_idx], r = 100)
print(bn_dep)
# Yeo-Johnson transformation with lambda = -1.426511 is the best transformation
# Initialize and assign with length-matched predictions
data_h1_no_outliers$depression_score_yeo <- NA_real_
data_h1_no_outliers$depression_score_yeo[non_missing_idx] <- as.numeric(
  predict(bn_dep, newdata = data_h1_no_outliers$depression_score[non_missing_idx])
)

# Check again the assumptions with the transformed outcome variable
fit_h1_check_transformed <- lm(depression_score_yeo ~ extraversion +
                                 agreeableness +
                                 conscientiousness + neuroticism + openness +
                                 sex, data = data_h1_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h1_transformed.svg", width = 12, height = 10)
print(check_model(fit_h1_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h1_check_transformed)) # now OK
print(check_heteroscedasticity(fit_h1_check_transformed)) # now OK

# Now we can fit the lavaan model with the transformed outcome variable
# and bootstrap confidence intervals
model_h1 <- "
  # Depression (depression_score_yeo) ~ personality + sex
  depression_score_yeo ~ extraversion + agreeableness + 
                         conscientiousness + neuroticism + openness + sex
"
fit_h1 <- sem(model_h1, data = data_h1_no_outliers, missing = "fiml",
              se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H1***\n")
print(summary(fit_h1, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H1 ***\n")
print(parameterEstimates(fit_h1, boot.ci.type = "bca.simple"))

# Save p-values for later FDR correction by context 
# (i.e., general, sadness, anxiety, anger)
p_values_h1 <- parameterEstimates(fit_h1) |>
  filter(lhs == "depression_score_yeo" & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H1***\n")
print(standardizedSolution(fit_h1, ci = FALSE))

# Define and fit the four models for hypothesis 2 ----
# Emotion-general model ----
# Filter out IDs with outliers 
outlier_ids <- read_csv("data/analysis/h2_general_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h2_gen_no_outliers <- data |> 
                       filter(!ID %in% outlier_ids)

# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model
# doesn't work with check_model() function from performance package
fit_h2_general_check <- lm(depression_score ~ maladaptive_score +
                             adaptive_score +
                             sex, data = data_h2_gen_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_general_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h2_general_check))
dev.off()
print(check_normality(fit_h2_general_check)) # not OK
print(check_heteroscedasticity(fit_h2_general_check)) # not OK

# Given violation of residual normality and heteroscedasticity, determine best transformation
# method for depression_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h2_gen_no_outliers$depression_score)
# Fit on non-missing values only
bn_dep <- bestNormalize(data_h2_gen_no_outliers$depression_score[non_missing_idx], r = 100)
print(bn_dep)
# Yeo-Johnson transformation with lambda = -1.433789 is the best transformation
# Initialize and assign with length-matched predictions
data_h2_gen_no_outliers$depression_score_yeo <- NA_real_
data_h2_gen_no_outliers$depression_score_yeo[non_missing_idx] <- as.numeric(
  predict(bn_dep, newdata = data_h2_gen_no_outliers$depression_score[non_missing_idx])
)

# Check model assumptions again
fit_h2_general_check_transformed <- lm(depression_score_yeo ~
                                         maladaptive_score +
                                           adaptive_score +
                                           sex, data = data_h2_gen_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_general_transformed.svg", width = 12, height = 10)
print(check_model(fit_h2_general_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h2_general_check_transformed)) # now OK
print(check_heteroscedasticity(fit_h2_general_check_transformed)) # now OK

# Now we can fit the lavaan model with the transformed outcome variable
# and bootstrap confidence intervals
model_h2_general <- "
  # Depression (depression_score_yeo) ~ general emotion regulation + sex
  depression_score_yeo ~ maladaptive_score + adaptive_score + sex
"
fit_h2_general <- sem(model_h2_general, data = data_h2_gen_no_outliers, missing = "fiml",
                      se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H2 (General Emotion)***\n")
print(summary(fit_h2_general, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H2 (General Emotion)***\n")
print(parameterEstimates(fit_h2_general, boot.ci.type = "bca.simple"))

# Save p-values for later FDR correction by context
p_values_h2_gen <- parameterEstimates(fit_h2_general) |>
  filter(lhs == "depression_score_yeo" & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H2 (General Emotion)***\n")
print(standardizedSolution(fit_h2_general, ci = FALSE))

# Define and fit the four models for hypothesis 2 ----
# Sadness-specific model ----
# Filter out IDs with outliers
outlier_ids <- read_csv("data/analysis/h2_sadness_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h2_sad_no_outliers <- data |> 
                               filter(!ID %in% outlier_ids)

# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model
# doesn't work with check_model() function from performance package
fit_h2_sadness_check <- lm(depression_score ~ sadness_maladaptive_score +
                             sadness_adaptive_score +
                             sex, data = data_h2_sad_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_sadness_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h2_sadness_check)) # a bit ambiguous
dev.off()
print(check_normality(fit_h2_sadness_check)) # not OK
print(check_heteroscedasticity(fit_h2_sadness_check)) # not OK

# Given violation of residual normality and heteroscedasticity, determine best transformation
# method for depression_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h2_sad_no_outliers$depression_score)
# Fit on non-missing values only
bn_dep <- bestNormalize(data_h2_sad_no_outliers$depression_score[non_missing_idx], r = 100)
print(bn_dep)
# sqrt(x + a) with a = 0 is the best transformation
# Initialize and assign with length-matched predictions
data_h2_sad_no_outliers$depression_score_sqrt <- NA_real_
data_h2_sad_no_outliers$depression_score_sqrt[non_missing_idx] <- as.numeric(
  predict(bn_dep, newdata = data_h2_sad_no_outliers$depression_score[non_missing_idx])
)

# Check model assumptions again
fit_h2_sadness_check_transformed <- lm(depression_score_sqrt ~
                                         sadness_maladaptive_score +
                                           sadness_adaptive_score +
                                           sex, data = data_h2_sad_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_sadness_transformed.svg", width = 12, height = 10)
print(check_model(fit_h2_sadness_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h2_sadness_check_transformed)) # now OK,
print(check_heteroscedasticity(fit_h2_sadness_check_transformed)) # now OK

# Now we can fit the lavaan model with the transformed outcome variable
# and bootstrap confidence intervals
model_h2_sadness <- "
  # Depression ~ sadness-specific emotion regulation + sex
  depression_score_sqrt ~ sadness_maladaptive_score + sadness_adaptive_score + sex
"
fit_h2_sadness <- sem(model_h2_sadness, data = data_h2_sad_no_outliers, missing = "fiml", 
                      se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H2 (Sadness-specific Emotion)***\n")
print(summary(fit_h2_sadness, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H2 (Sadness-specific Emotion)***\n")
print(parameterEstimates(fit_h2_sadness, boot.ci.type = 'bca.simple'))

# Save p-values for later FDR correction by context
p_values_h2_sad <- parameterEstimates(fit_h2_sadness) |>
  filter(lhs == "depression_score_sqrt" & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H2 (Sadness-specific Emotion)***\n")
print(standardizedSolution(fit_h2_sadness, ci = FALSE))

# Anxiety-specific model ----
# Filter out IDs with outliers
outlier_ids <- read_csv("data/analysis/h2_anxiety_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h2_anx_no_outliers <- data |> 
                               filter(!ID %in% outlier_ids)

# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model 
# doesn't work with check_model() function from performance package
fit_h2_anxiety_check <- lm(depression_score ~ anxiety_maladaptive_score + anxiety_adaptive_score + sex, 
                           data = data_h2_anx_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_anxiety_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h2_anxiety_check))
dev.off()
print(check_normality(fit_h2_anxiety_check)) # not OK
print(check_heteroscedasticity(fit_h2_anxiety_check)) # not OK

# Given violation of residual normality and heteroscedasticity, determine best transformation
# method for depression_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h2_anx_no_outliers$depression_score)
# Fit on non-missing values only
bn_dep <- bestNormalize(data_h2_anx_no_outliers$depression_score[non_missing_idx], r = 100)
print(bn_dep)
# Yeo-Johnson transformation with lambda = -1.44637 is the best transformation
# Initialize and assign with length-matched predictions
data_h2_anx_no_outliers$depression_score_yeo <- NA_real_
data_h2_anx_no_outliers$depression_score_yeo[non_missing_idx] <- as.numeric(
  predict(bn_dep, newdata = data_h2_anx_no_outliers$depression_score[non_missing_idx])
)

# Check model assumptions again
fit_h2_anxiety_check_transformed <- lm(depression_score_yeo ~ anxiety_maladaptive_score + anxiety_adaptive_score + sex, 
                                      data = data_h2_anx_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_anxiety_transformed.svg", width = 12, height = 10)
print(check_model(fit_h2_anxiety_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h2_anxiety_check_transformed)) # now OK
print(check_heteroscedasticity(fit_h2_anxiety_check_transformed)) # now OK

# Now we can fit the lavaan model with the transformed outcome variable and bootstrap confidence intervals
model_h2_anxiety <- '
  # Depression (depression_score_yeo) ~ anxiety-specific emotion regulation strategy use + sex
  depression_score_yeo ~ anxiety_maladaptive_score + anxiety_adaptive_score + sex
'
fit_h2_anxiety <- sem(model_h2_anxiety, data = data_h2_anx_no_outliers, missing = "fiml", 
                      se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H2 (Anxiety-specific Emotion)***\n")
print(summary(fit_h2_anxiety, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H2 (Anxiety-specific Emotion)***\n")
print(parameterEstimates(fit_h2_anxiety, boot.ci.type = 'bca.simple'))

# Save p-values for later FDR correction by context
p_values_h2_anx <- parameterEstimates(fit_h2_anxiety) |>
  filter(lhs == "depression_score_yeo" & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H2 (Anxiety-specific Emotion)***\n")
print(standardizedSolution(fit_h2_anxiety, ci = FALSE))

# Anger-specific model ----
# Filter out IDs with outliers
outlier_ids <- read_csv("data/analysis/h2_anger_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h2_ang_no_outliers <- data |> 
                                 filter(!ID %in% outlier_ids)
# First check model assumptions
# Separate model for checking assumptions since lavaan's sem model 
# doesn't work with check_model() function from performance package
fit_h2_anger_check <- lm(depression_score ~ anger_maladaptive_score + anger_adaptive_score + sex, 
                         data = data_h2_ang_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_anger_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h2_anger_check))
dev.off()
print(check_normality(fit_h2_anger_check)) # not OK
print(check_heteroscedasticity(fit_h2_anger_check)) # not OK

# Given violation of residual normality and heteroscedasticity, determine best transformation
# method for depression_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h2_ang_no_outliers$depression_score)
# Fit on non-missing values only
bn_dep <- bestNormalize(data_h2_ang_no_outliers$depression_score[non_missing_idx], r = 100)
print(bn_dep)
# sqrt(x + a) with a = 0 is the best transformation
# Initialize and assign with length-matched predictions
data_h2_ang_no_outliers$depression_score_sqrt <- NA_real_
data_h2_ang_no_outliers$depression_score_sqrt[non_missing_idx] <- as.numeric(
  predict(bn_dep, newdata = data_h2_ang_no_outliers$depression_score[non_missing_idx])
)

# Check model assumptions again
fit_h2_anger_check_transformed <- lm(depression_score_sqrt ~ anger_maladaptive_score + anger_adaptive_score + sex, 
                                      data = data_h2_ang_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h2_anger_transformed.svg", width = 12, height = 10)
print(check_model(fit_h2_anger_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h2_anger_check_transformed)) # now OK
print(check_heteroscedasticity(fit_h2_anger_check_transformed)) # now OK

# Now we can fit the lavaan model with the transformed outcome variable and bootstrap confidence intervals
model_h2_anger <- '
  # Depression (depression_score_sqrt) ~ anger-specific emotion regulation strategy use + sex
  depression_score_sqrt ~ anger_maladaptive_score + anger_adaptive_score + sex
'
fit_h2_anger <- sem(model_h2_anger, data = data_h2_ang_no_outliers, missing = "fiml", 
                     se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H2 (Anger-specific Emotion)***\n")
print(summary(fit_h2_anger, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H2 (Anger-specific Emotion)***\n")
print(parameterEstimates(fit_h2_anger, boot.ci.type = 'bca.simple'))

# Save p-values for later FDR correction by context
p_values_h2_ang <- parameterEstimates(fit_h2_anger) |>
  filter(lhs == "depression_score_sqrt" & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H2 (Anger-specific Emotion)***\n")
print(standardizedSolution(fit_h2_anger, ci = FALSE))

# Define and fit the four multivariate models for hypothesis 3 ----
# Emotion-general model ----
# Filter out IDs with outliers
outlier_ids <- read_csv("data/analysis/h3_general_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h3_gen_no_outliers <- data |> 
                                 filter(!ID %in% outlier_ids)
# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model 
# doesn't work with check_model() function from performance package
# Also, now we are checking model assumptions univariatelly because 
# performance package doesn't support multivariate (tried manual checks but failed, so will just check univariate models for now)
fit_h3_mal_gen_check <- lm(maladaptive_score ~ extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_gen_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_mal_general_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_mal_gen_check)) # good
dev.off()
print(check_normality(fit_h3_mal_gen_check)) # OK
print(check_heteroscedasticity(fit_h3_mal_gen_check)) # OK

fit_h3_ad_gen_check <- lm(adaptive_score ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_gen_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_general_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_gen_check)) # a bit ambiguous
dev.off()
print(check_normality(fit_h3_ad_gen_check)) # not OK
print(check_heteroscedasticity(fit_h3_ad_gen_check)) # OK

# Given violation of residual normality, determine best transformation
# method for adaptive_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h3_gen_no_outliers$adaptive_score)
# Fit on non-missing values only
bn_ada <- bestNormalize(data_h3_gen_no_outliers$adaptive_score[non_missing_idx], r = 100)
print(bn_ada)
# Yeo-Johnson transformation with lambda = 2.360003 is the best transformation
# Initialize and assign with length-matched predictions
data_h3_gen_no_outliers$adaptive_score_yeo <- NA_real_
data_h3_gen_no_outliers$adaptive_score_yeo[non_missing_idx] <- as.numeric(
  predict(bn_ada, newdata = data_h3_gen_no_outliers$adaptive_score[non_missing_idx])
)

# Check again the assumptions with the transformed outcome variable
fit_h3_ad_gen_check_transformed <- lm(adaptive_score_yeo ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_gen_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_general_transformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_gen_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h3_ad_gen_check_transformed)) # now OK
print(check_heteroscedasticity(fit_h3_ad_gen_check_transformed)) # still OK  

# Now we can fit the multivariate lavaan model 
model_h3_general <- '
  # Maladaptive emotion regulation strategy use ~ personality + sex
  # Adaptive emotion regulation strategy use ~ personality + sex
    maladaptive_score ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
    adaptive_score_yeo ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
'
fit_h3_general <- sem(model_h3_general, data = data_h3_gen_no_outliers, missing = "fiml", 
                      se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H3 (General Emotion)***\n")
print(summary(fit_h3_general, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H3 (General Emotion)***\n")
print(parameterEstimates(fit_h3_general, boot.ci.type = 'bca.simple'))

# Save p-values for later FDR correction by context
p_values_h3_gen <- parameterEstimates(fit_h3_general) |>
  filter((lhs == "maladaptive_score" | lhs == "adaptive_score_yeo") & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H3 (General Emotion)***\n")
print(standardizedSolution(fit_h3_general, ci = FALSE))

# Sadness-specific model ----
# Filter out IDs with outliers
outlier_ids <- read_csv("data/analysis/h3_sadness_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h3_sad_no_outliers <- data |> 
                                 filter(!ID %in% outlier_ids)

# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model 
# doesn't work with check_model() function from performance package
# Also, now we are checking model assumptions univariatelly because 
# performance package doesn't support multivariate (tried manual checks but failed, so will just check univariate models for now)
fit_h3_mal_sad_check <- lm(sadness_maladaptive_score ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_sad_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_mal_sadness_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_mal_sad_check)) # good
dev.off()
print(check_normality(fit_h3_mal_sad_check)) # OK
print(check_heteroscedasticity(fit_h3_mal_sad_check)) # OK

fit_h3_ad_sad_check <- lm(sadness_adaptive_score ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_sadness_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_sad_check)) # a bit ambiguous
dev.off()
print(check_normality(fit_h3_ad_sad_check)) # not OK
print(check_heteroscedasticity(fit_h3_ad_sad_check)) # OK

# Given violation of residual normality, determine best transformation
# method for sadness_adaptive_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h3_sad_no_outliers$sadness_adaptive_score)
# Fit on non-missing values only
bn_ada_sad <- bestNormalize(data_h3_sad_no_outliers$sadness_adaptive_score[non_missing_idx], r = 100)
print(bn_ada_sad)
# Box-Cox transformation with lambda = 1.999933 is the best transformation
# Initialize and assign with length-matched predictions
data_h3_sad_no_outliers$sadness_adaptive_score_box <- NA_real_
data_h3_sad_no_outliers$sadness_adaptive_score_box[non_missing_idx] <- as.numeric(
  predict(bn_ada_sad, newdata = data_h3_sad_no_outliers$sadness_adaptive_score[non_missing_idx])
)

# Check again the assumptions with the transformed outcome variable
fit_h3_ad_sad_check_transformed <- lm(sadness_adaptive_score_box ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_sad_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_sadness_transformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_sad_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h3_ad_sad_check_transformed)) # still not OK, but above plots look better
print(check_heteroscedasticity(fit_h3_ad_sad_check_transformed)) # now OK 

# Now we can fit the multivariate lavaan model 
model_h3_sad <- '
  # Maladaptive sad emotion regulation strategy use ~ personality + sex
  # Adaptive sad emotion regulation strategy use ~ personality + sex
    sadness_maladaptive_score ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
    sadness_adaptive_score_box ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
'
fit_h3_sad <- sem(model_h3_sad, data = data_h3_sad_no_outliers, missing = "fiml", 
                      se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H3 (Sadness-specific Emotion)***\n")
print(summary(fit_h3_sad, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H3 (Sadness-specific Emotion)***\n")
print(parameterEstimates(fit_h3_sad, boot.ci.type = 'bca.simple'))

# Save p-values for later FDR correction by context
p_values_h3_sad <- parameterEstimates(fit_h3_sad) |>
  filter((lhs == "sadness_maladaptive_score" | lhs == "sadness_adaptive_score_box") & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H3 (Sadness-specific Emotion)***\n")
print(standardizedSolution(fit_h3_sad, ci = FALSE))

# Anxiety-specific model ----
# Filter out IDs with outliers
outlier_ids <- read_csv("data/analysis/h3_anxiety_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h3_anx_no_outliers <- data |> 
                                 filter(!ID %in% outlier_ids)

# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model 
# doesn't work with check_model() function from performance package
# Also, now we are checking model assumptions univariatelly because 
# performance package doesn't support multivariate (tried manual checks but failed, so will just check univariate models for now)
fit_h3_mal_anx_check <- lm(anxiety_maladaptive_score ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_anx_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_mal_anxiety_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_mal_anx_check)) # good
dev.off()
print(check_normality(fit_h3_mal_anx_check)) # OK
print(check_heteroscedasticity(fit_h3_mal_anx_check)) # OK

fit_h3_ad_anx_check <- lm(anxiety_adaptive_score ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_anx_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_anxiety_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_anx_check)) # not that good plots
dev.off()
print(check_normality(fit_h3_ad_anx_check)) # not OK
print(check_heteroscedasticity(fit_h3_ad_anx_check)) # OK

# Given violation of residual normality and heteroscedasticity, determine best transformation
# method for anxiety_adaptive_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h3_anx_no_outliers$anxiety_adaptive_score)
# Fit on non-missing values only
bn_ada_anx <- bestNormalize(data_h3_anx_no_outliers$anxiety_adaptive_score[non_missing_idx], r = 100)
print(bn_ada_anx)
# Box-Cox transformation with lambda = 1.999942 is the best transformation
# Initialize and assign with length-matched predictions
data_h3_anx_no_outliers$anxiety_adaptive_score_box <- NA_real_
data_h3_anx_no_outliers$anxiety_adaptive_score_box[non_missing_idx] <- as.numeric(
  predict(bn_ada_anx, newdata = data_h3_anx_no_outliers$anxiety_adaptive_score[non_missing_idx])
)

# Check again the assumptions with the transformed outcome variable
fit_h3_ad_anx_check_transformed <- lm(anxiety_adaptive_score_box ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_anx_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_anxiety_transformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_anx_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h3_ad_anx_check_transformed)) # now OK
print(check_heteroscedasticity(fit_h3_ad_anx_check_transformed)) # now OK  

# Now we can fit the multivariate lavaan model 
model_h3_anx <- '
  # Maladaptive anxiety emotion regulation strategy use ~ personality + sex
  # Adaptive anxiety emotion regulation strategy use ~ personality + sex
    anxiety_maladaptive_score ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
    anxiety_adaptive_score_box ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
'
fit_h3_anx <- sem(model_h3_anx, data = data_h3_anx_no_outliers, missing = "fiml", 
                      se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H3 (Anxiety-specific Emotion)***\n")
print(summary(fit_h3_anx, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H3 (Anxiety-specific Emotion)***\n")
print(parameterEstimates(fit_h3_anx, boot.ci.type = 'bca.simple'))

# Save p-values for later FDR correction by context
p_values_h3_anx <- parameterEstimates(fit_h3_anx) |>
  filter((lhs == "anxiety_maladaptive_score" | lhs == "anxiety_adaptive_score_box") & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H3 (Anxiety-specific Emotion)***\n")
print(standardizedSolution(fit_h3_anx, ci = FALSE))

# Anger-specific model ----
# Filter out IDs with outliers
outlier_ids <- read_csv("data/analysis/h3_anger_outliers.csv") |> 
               pull(ID)

# Remove outliers from the dataset
data_h3_ang_no_outliers <- data |> 
                                 filter(!ID %in% outlier_ids)
# Check model assumptions
# Separate model for checking assumptions since lavaan's sem model 
# doesn't work with check_model() function from performance package
# Also, now we are checking model assumptions univariatelly because 
# performance package doesn't support multivariate (tried manual checks but failed, so will just check univariate models for now)
fit_h3_mal_ang_check <- lm(anger_maladaptive_score ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_ang_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_mal_anger_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_mal_ang_check)) # good
dev.off()
print(check_normality(fit_h3_mal_ang_check)) # OK
print(check_heteroscedasticity(fit_h3_mal_ang_check)) # OK
fit_h3_ad_ang_check <- lm(anger_adaptive_score ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_ang_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_anger_untransformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_ang_check)) # not that good plots
dev.off()
print(check_normality(fit_h3_ad_ang_check)) # not OK
print(check_heteroscedasticity(fit_h3_ad_ang_check)) # OK

# Given violation of residual normality, determine best transformation
# method for anger_adaptive_score (outcome variable)
# We can use the bestNormalize package for this
non_missing_idx <- !is.na(data_h3_ang_no_outliers$anger_adaptive_score)
# Fit on non-missing values only
bn_ada_ang <- bestNormalize(data_h3_ang_no_outliers$anger_adaptive_score[non_missing_idx], r = 100)
print(bn_ada_ang)
# Yeo-Johnson transformation with lambda = 2.483021 is the best transformation
# Initialize and assign with length-matched predictions
data_h3_ang_no_outliers$anger_adaptive_score_yeo <- NA_real_
data_h3_ang_no_outliers$anger_adaptive_score_yeo[non_missing_idx] <- as.numeric(
  predict(bn_ada_ang, newdata = data_h3_ang_no_outliers$anger_adaptive_score[non_missing_idx])
)

# Check again the assumptions with the transformed outcome variable
fit_h3_ad_ang_check_transformed <- lm(anger_adaptive_score_yeo ~  extraversion + agreeableness + 
                   conscientiousness + neuroticism + openness + sex, data = data_h3_ang_no_outliers)
svg("reports/plots/lm_diagnostics/no_outliers_check_model_h3_ad_anger_transformed.svg", width = 12, height = 10)
print(check_model(fit_h3_ad_ang_check_transformed)) # looks better
dev.off()
print(check_normality(fit_h3_ad_ang_check_transformed)) # still not OK but better (and above plots look way better than before)
print(check_heteroscedasticity(fit_h3_ad_ang_check_transformed)) # now OK 

# Now we can fit the multivariate lavaan model 
model_h3_ang <- '
  # Maladaptive anger emotion regulation strategy use ~ personality + sex
  # Adaptive anger emotion regulation strategy use ~ personality + sex
    anger_maladaptive_score ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
    anger_adaptive_score_yeo ~ extraversion + agreeableness + conscientiousness + neuroticism + openness + sex
'
fit_h3_ang <- sem(model_h3_ang, data = data_h3_ang_no_outliers, missing = "fiml", 
                      se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model H3 (Anger-specific Emotion)***\n")
print(summary(fit_h3_ang, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates H3 (Anger-specific Emotion)***\n")
print(parameterEstimates(fit_h3_ang, boot.ci.type = 'bca.simple'))

# Save p-values for later FDR correction by context
p_values_h3_ang <- parameterEstimates(fit_h3_ang) |>
  filter((lhs == "anger_maladaptive_score" | lhs == "anger_adaptive_score_yeo") & op == "~" & rhs != "sex") |>
  pull(pvalue, name = rhs)

cat("***Standardized parameter estimates H3 (Anger-specific Emotion)***\n")
print(standardizedSolution(fit_h3_ang, ci = FALSE))

# End sink to stop capturing output in the text file ----
sink(file = NULL)

# FDR-correction of p-values by context ----
# Combine p-values from all models by context
p_values_gen <- c(p_values_h1, p_values_h2_gen, p_values_h3_gen)
p_values_sad <- c(p_values_h1, p_values_h2_sad, p_values_h3_sad)
p_values_anx <- c(p_values_h1, p_values_h2_anx, p_values_h3_anx)
p_values_ang <- c(p_values_h1, p_values_h2_ang, p_values_h3_ang)

# Apply FDR correction separately for each context
p_values_gen_adj <- p.adjust(p_values_gen, method = "fdr")
p_values_sad_adj <- p.adjust(p_values_sad, method = "fdr")
p_values_anx_adj <- p.adjust(p_values_anx, method = "fdr")
p_values_ang_adj <- p.adjust(p_values_ang, method = "fdr")

# Save adjusted p-values to a CSV file for reporting
adjusted_p_values <- tibble(
  context = c(rep("general", length(p_values_gen_adj)),
              rep("sadness", length(p_values_sad_adj)),
              rep("anxiety", length(p_values_anx_adj)),
              rep("anger", length(p_values_ang_adj))),
  p_value_adj = c(p_values_gen_adj, p_values_sad_adj, p_values_anx_adj, p_values_ang_adj),
  predictor = c(names(p_values_gen_adj), names(p_values_sad_adj), 
                names(p_values_anx_adj), names(p_values_ang_adj))
)

write_csv(adjusted_p_values, "data/analysis/no_outliers_lms_adjusted_p_values.csv")

# Merge and save datasets for each mediation model with outliers removed and transformed variables added ----
# For general emotion model ----
all_outlier_ids <- bind_rows(
  read_csv("data/analysis/h1_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h2_general_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h3_general_outliers.csv", show_col_types = FALSE)
) |>
  mutate(ID = trimws(as.character(ID))) |>
  distinct(ID)

data_med_gen <- data |>
  mutate(ID = trimws(as.character(ID))) |>
  anti_join(all_outlier_ids, by = "ID")

# Add transformed variables for the general emotion model
data_med_gen <- data_med_gen |>
  mutate(
    depression_score_yeo = data_h1_no_outliers$depression_score_yeo[match(ID, data_h1_no_outliers$ID)],
    adaptive_score_yeo = data_h3_gen_no_outliers$adaptive_score_yeo[match(ID, data_h3_gen_no_outliers$ID)]
  )
write_csv(data_med_gen, "data/analysis/no_outliers_data_med_gen.csv")

# For sadness-specific emotion model ----
all_outlier_ids <- bind_rows(
  read_csv("data/analysis/h1_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h2_sadness_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h3_sadness_outliers.csv", show_col_types = FALSE)
) |>
  mutate(ID = trimws(as.character(ID))) |>
  distinct(ID)

data_med_sad <- data |>
  mutate(ID = trimws(as.character(ID))) |>
  anti_join(all_outlier_ids, by = "ID")

# Add transformed variables for the sadness-specific emotion model
data_med_sad <- data_med_sad |>
  mutate(
    depression_score_sqrt = data_h2_sad_no_outliers$depression_score_sqrt[match(ID, data_h2_sad_no_outliers$ID)],
    sadness_adaptive_score_box = data_h3_sad_no_outliers$sadness_adaptive_score_box[match(ID, data_h3_sad_no_outliers$ID)]
  )
write_csv(data_med_sad, "data/analysis/no_outliers_data_med_sad.csv")

# For anxiety-specific emotion model ----
all_outlier_ids <- bind_rows(
  read_csv("data/analysis/h1_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h2_anxiety_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h3_anxiety_outliers.csv", show_col_types = FALSE)
) |>
  mutate(ID = trimws(as.character(ID))) |>
  distinct(ID)

data_med_anx <- data |>
  mutate(ID = trimws(as.character(ID))) |>
  anti_join(all_outlier_ids, by = "ID")

# Add transformed variables for the anxiety-specific emotion model
data_med_anx <- data_med_anx |>
  mutate(
    depression_score_yeo = data_h2_anx_no_outliers$depression_score_yeo[match(ID, data_h2_anx_no_outliers$ID)],
    anxiety_adaptive_score_box = data_h3_anx_no_outliers$anxiety_adaptive_score_box[match(ID, data_h3_anx_no_outliers$ID)]
  )
write_csv(data_med_anx, "data/analysis/no_outliers_data_med_anx.csv")

# For anger-specific emotion model ----
all_outlier_ids <- bind_rows(
  read_csv("data/analysis/h1_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h2_anger_outliers.csv", show_col_types = FALSE),
  read_csv("data/analysis/h3_anger_outliers.csv", show_col_types = FALSE)
) |>
  mutate(ID = trimws(as.character(ID))) |>
  distinct(ID)

data_med_ang <- data |>
  mutate(ID = trimws(as.character(ID))) |>
  anti_join(all_outlier_ids, by = "ID")

# Add transformed variables for the anger-specific emotion model
data_med_ang <- data_med_ang |>
  mutate(
    depression_score_sqrt = data_h2_ang_no_outliers$depression_score_sqrt[match(ID, data_h2_ang_no_outliers$ID)],
    anger_adaptive_score_yeo = data_h3_ang_no_outliers$anger_adaptive_score_yeo[match(ID, data_h3_ang_no_outliers$ID)]
  )
write_csv(data_med_ang, "data/analysis/no_outliers_data_med_ang.csv")