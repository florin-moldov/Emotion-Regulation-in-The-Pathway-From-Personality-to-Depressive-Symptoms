# Mediation models per emotion regulation context
#
# This script tests the fourth preregistered hypothesis by fitting mediation models for 
# each emotion regulation context (general, sadness, anxiety, anger) 
# and saving the results.
#
# Fourth hypothesis:
#  We will investigate whether the expected direct relations between adolescent personality 
#  traits and depressive symptoms in adulthood are at least partially explained 
#  (i.e., mediated) by the use of emotion-general and/or emotion-specific (mal)adaptive 
#  regulation strategies in early adulthood.
#
# Additionally, controls for sex effects.

# Set seed for reproducibility ----
set.seed(123)

# Load necessary libraries ----
library(tidyverse)
library(lavaan)

# Load the extended data with transformations ----
data <- read_csv("data/analysis/extended_data_with_transformations.csv")

# Set sink to capture all output in a text file ----
sink("data/analysis/mediation_output.txt")

# Mediation model 1 (general) ----
model_general <- "
  # Direct effect
  depression_score_yeo ~ c1*extraversion + c2*agreeableness + 
                         c3*conscientiousness + c4*neuroticism + 
                         c5*openness + c6*sex
  
  # Mediator
  maladaptive_score ~ a1*extraversion + a2*agreeableness + 
                      a3*conscientiousness + a4*neuroticism +
                      a5*openness + a6*sex
  adaptive_score_box ~ a7*extraversion + a8*agreeableness + 
                       a9*conscientiousness + a10*neuroticism +
                       a11*openness + a12*sex
  depression_score_yeo ~ b1*maladaptive_score + b2*adaptive_score_box

  # Covariance between mediators
  maladaptive_score ~~ adaptive_score_box
  
  # Predictor-specific indirect effects via each mediator
    ind_ext_mal := a1*b1
    ind_agr_mal := a2*b1
    ind_con_mal := a3*b1
    ind_neu_mal := a4*b1
    ind_ope_mal := a5*b1
    ind_sex_mal := a6*b1

    ind_ext_ada := a7*b2
    ind_agr_ada := a8*b2
    ind_con_ada := a9*b2
    ind_neu_ada := a10*b2
    ind_ope_ada := a11*b2
    ind_sex_ada := a12*b2

  # Predictor-specific total indirect effects
    ind_ext := ind_ext_mal + ind_ext_ada
    ind_agr := ind_agr_mal + ind_agr_ada
    ind_con := ind_con_mal + ind_con_ada
    ind_neu := ind_neu_mal + ind_neu_ada
    ind_ope := ind_ope_mal + ind_ope_ada
    ind_sex := ind_sex_mal + ind_sex_ada

  # Predictor-specific total effects (direct + indirect)
    tot_ext := c1 + ind_ext
    tot_agr := c2 + ind_agr
    tot_con := c3 + ind_con
    tot_neu := c4 + ind_neu
    tot_ope := c5 + ind_ope
    tot_sex := c6 + ind_sex
"

fit_general <- sem(model_general, data = data, missing = "fiml",
              se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model (General)***\n")
print(summary(fit_general, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates General ***\n")
print(parameterEstimates(fit_general, boot.ci.type = "bca.simple"))

# Save p-values for later FDR correction by context and path coefficients for reporting
p_values_general <- parameterEstimates(fit_general) |>
  filter((lhs == "depression_score_yeo" | lhs == "maladaptive_score" | lhs == "adaptive_score_box" | 
          lhs == "ind_ext_mal" | lhs == "ind_agr_mal" | lhs == "ind_con_mal" | lhs == "ind_neu_mal" | lhs == "ind_ope_mal" |
          lhs == "ind_ext_ada" | lhs == "ind_agr_ada" | lhs == "ind_con_ada" | lhs == "ind_neu_ada" | lhs == "ind_ope_ada" |
          lhs == "tot_ext" | lhs == "tot_agr" | lhs == "tot_con" | lhs == "tot_neu" | lhs == "tot_ope") & 
          (op == "~" | op == ":=") & rhs != "sex") |>
  pull(pvalue, name = label)

cat("***Standardized parameter estimates General***\n")
print(standardizedSolution(fit_general, ci = FALSE))

# Mediation model 2 (sadness) ----
model_sadness <- "
  # Direct effect
  depression_score_yeo ~ c1*extraversion + c2*agreeableness + 
                         c3*conscientiousness + c4*neuroticism + 
                         c5*openness + c6*sex
  
  # Mediator
  sadness_maladaptive_score ~ a1*extraversion + a2*agreeableness + 
                              a3*conscientiousness + a4*neuroticism +
                              a5*openness + a6*sex
  sadness_adaptive_score_ord ~ a7*extraversion + a8*agreeableness + 
                               a9*conscientiousness + a10*neuroticism +
                               a11*openness + a12*sex
  depression_score_yeo ~ b1*sadness_maladaptive_score + b2*sadness_adaptive_score_ord

  # Covariance between mediators
  sadness_maladaptive_score ~~ sadness_adaptive_score_ord
  
  # Predictor-specific indirect effects via each mediator
    ind_ext_sad_mal := a1*b1
    ind_agr_sad_mal := a2*b1
    ind_con_sad_mal := a3*b1
    ind_neu_sad_mal := a4*b1
    ind_ope_sad_mal := a5*b1
    ind_sex_sad_mal := a6*b1

    ind_ext_sad_ada := a7*b2
    ind_agr_sad_ada := a8*b2
    ind_con_sad_ada := a9*b2
    ind_neu_sad_ada := a10*b2
    ind_ope_sad_ada := a11*b2
    ind_sex_sad_ada := a12*b2

  # Predictor-specific total indirect effects
    ind_ext := ind_ext_sad_mal + ind_ext_sad_ada
    ind_agr := ind_agr_sad_mal + ind_agr_sad_ada
    ind_con := ind_con_sad_mal + ind_con_sad_ada
    ind_neu := ind_neu_sad_mal + ind_neu_sad_ada
    ind_ope := ind_ope_sad_mal + ind_ope_sad_ada
    ind_sex := ind_sex_sad_mal + ind_sex_sad_ada

  # Predictor-specific total effects (direct + indirect)
    tot_ext := c1 + ind_ext
    tot_agr := c2 + ind_agr
    tot_con := c3 + ind_con
    tot_neu := c4 + ind_neu
    tot_ope := c5 + ind_ope
    tot_sex := c6 + ind_sex
"

fit_sadness <- sem(model_sadness, data = data, missing = "fiml",
              se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model (Sadness)***\n")
print(summary(fit_sadness, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates Sadness ***\n")
print(parameterEstimates(fit_sadness, boot.ci.type = "bca.simple"))

# Save p-values for later FDR correction by context and path coefficients for reporting
p_values_sadness <- parameterEstimates(fit_sadness) |>
  filter((lhs == "depression_score_yeo" | lhs == "sadness_maladaptive_score" | lhs == "sadness_adaptive_score_ord" | 
          lhs == "ind_ext_sad_mal" | lhs == "ind_agr_sad_mal" | lhs == "ind_con_sad_mal" | lhs == "ind_neu_sad_mal" | lhs == "ind_ope_sad_mal" |
          lhs == "ind_ext_sad_ada" | lhs == "ind_agr_sad_ada" | lhs == "ind_con_sad_ada" | lhs == "ind_neu_sad_ada" | lhs == "ind_ope_sad_ada" |
          lhs == "tot_ext" | lhs == "tot_agr" | lhs == "tot_con" | lhs == "tot_neu" | lhs == "tot_ope") & 
          (op == "~" | op == ":=") & rhs != "sex") |>
  pull(pvalue, name = label)

cat("***Standardized parameter estimates Sadness***\n")
print(standardizedSolution(fit_sadness, ci = FALSE))

# Mediation model 3 (anxiety) ----
model_anxiety <- "
  # Direct effect
  depression_score_yeo ~ c1*extraversion + c2*agreeableness + 
                         c3*conscientiousness + c4*neuroticism + 
                         c5*openness + c6*sex
  
  # Mediator
  anxiety_maladaptive_score ~ a1*extraversion + a2*agreeableness + 
                              a3*conscientiousness + a4*neuroticism +
                              a5*openness + a6*sex
  anxiety_adaptive_score_box ~ a7*extraversion + a8*agreeableness + 
                               a9*conscientiousness + a10*neuroticism +
                               a11*openness + a12*sex
  depression_score_yeo ~ b1*anxiety_maladaptive_score + b2*anxiety_adaptive_score_box

  # Covariance between mediators
  anxiety_maladaptive_score ~~ anxiety_adaptive_score_box
  
  # Predictor-specific indirect effects via each mediator
    ind_ext_anx_mal := a1*b1
    ind_agr_anx_mal := a2*b1
    ind_con_anx_mal := a3*b1
    ind_neu_anx_mal := a4*b1
    ind_ope_anx_mal := a5*b1
    ind_sex_anx_mal := a6*b1

    ind_ext_anx_ada := a7*b2
    ind_agr_anx_ada := a8*b2
    ind_con_anx_ada := a9*b2
    ind_neu_anx_ada := a10*b2
    ind_ope_anx_ada := a11*b2
    ind_sex_anx_ada := a12*b2

  # Predictor-specific total indirect effects
    ind_ext := ind_ext_anx_mal + ind_ext_anx_ada
    ind_agr := ind_agr_anx_mal + ind_agr_anx_ada
    ind_con := ind_con_anx_mal + ind_con_anx_ada
    ind_neu := ind_neu_anx_mal + ind_neu_anx_ada
    ind_ope := ind_ope_anx_mal + ind_ope_anx_ada
    ind_sex := ind_sex_anx_mal + ind_sex_anx_ada

  # Predictor-specific total effects (direct + indirect)
    tot_ext := c1 + ind_ext
    tot_agr := c2 + ind_agr
    tot_con := c3 + ind_con
    tot_neu := c4 + ind_neu
    tot_ope := c5 + ind_ope
    tot_sex := c6 + ind_sex
"

fit_anxiety <- sem(model_anxiety, data = data, missing = "fiml",
              se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model (Anxiety)***\n")
print(summary(fit_anxiety, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates Anxiety ***\n")
print(parameterEstimates(fit_anxiety, boot.ci.type = "bca.simple"))

# Save p-values for later FDR correction by context and path coefficients for reporting
p_values_anxiety <- parameterEstimates(fit_anxiety) |>
  filter((lhs == "depression_score_yeo" | lhs == "anxiety_maladaptive_score" | lhs == "anxiety_adaptive_score_box" | 
          lhs == "ind_ext_anx_mal" | lhs == "ind_agr_anx_mal" | lhs == "ind_con_anx_mal" | lhs == "ind_neu_anx_mal" | lhs == "ind_ope_anx_mal" |
          lhs == "ind_ext_anx_ada" | lhs == "ind_agr_anx_ada" | lhs == "ind_con_anx_ada" | lhs == "ind_neu_anx_ada" | lhs == "ind_ope_anx_ada" |
          lhs == "tot_ext" | lhs == "tot_agr" | lhs == "tot_con" | lhs == "tot_neu" | lhs == "tot_ope") & 
          (op == "~" | op == ":=") & rhs != "sex") |>
  pull(pvalue, name = label)

cat("***Standardized parameter estimates Anxiety***\n")
print(standardizedSolution(fit_anxiety, ci = FALSE))

# Mediation model 4 (anger) ----
model_anger <- "
  # Direct effect
  depression_score_yeo ~ c1*extraversion + c2*agreeableness + 
                         c3*conscientiousness + c4*neuroticism + 
                         c5*openness + c6*sex
  
  # Mediator
  anger_maladaptive_score ~ a1*extraversion + a2*agreeableness + 
                              a3*conscientiousness + a4*neuroticism +
                              a5*openness + a6*sex
  anger_adaptive_score_yeo ~ a7*extraversion + a8*agreeableness + 
                               a9*conscientiousness + a10*neuroticism +
                               a11*openness + a12*sex
  depression_score_yeo ~ b1*anger_maladaptive_score + b2*anger_adaptive_score_yeo

  # Covariance between mediators
  anger_maladaptive_score ~~ anger_adaptive_score_yeo
  
  # Predictor-specific indirect effects via each mediator
    ind_ext_ang_mal := a1*b1
    ind_agr_ang_mal := a2*b1
    ind_con_ang_mal := a3*b1
    ind_neu_ang_mal := a4*b1
    ind_ope_ang_mal := a5*b1
    ind_sex_ang_mal := a6*b1

    ind_ext_ang_ada := a7*b2
    ind_agr_ang_ada := a8*b2
    ind_con_ang_ada := a9*b2
    ind_neu_ang_ada := a10*b2
    ind_ope_ang_ada := a11*b2
    ind_sex_ang_ada := a12*b2

  # Predictor-specific total indirect effects
    ind_ext := ind_ext_ang_mal + ind_ext_ang_ada
    ind_agr := ind_agr_ang_mal + ind_agr_ang_ada
    ind_con := ind_con_ang_mal + ind_con_ang_ada
    ind_neu := ind_neu_ang_mal + ind_neu_ang_ada
    ind_ope := ind_ope_ang_mal + ind_ope_ang_ada
    ind_sex := ind_sex_ang_mal + ind_sex_ang_ada

  # Predictor-specific total effects (direct + indirect)
    tot_ext := c1 + ind_ext
    tot_agr := c2 + ind_agr
    tot_con := c3 + ind_con
    tot_neu := c4 + ind_neu
    tot_ope := c5 + ind_ope
    tot_sex := c6 + ind_sex
"

fit_anger <- sem(model_anger, data = data, missing = "fiml",
              se = "bootstrap", bootstrap = 10000, fixed.x = FALSE)

cat("***Summary of the fitted model (Anger)***\n")
print(summary(fit_anger, standardized = FALSE, fit.measures = TRUE))

cat("***Parameter estimates Anger ***\n")
print(parameterEstimates(fit_anger, boot.ci.type = "bca.simple"))

# Save p-values for later FDR correction by context and path coefficients for reporting
p_values_anger <- parameterEstimates(fit_anger) |>
  filter((lhs == "depression_score_yeo" | lhs == "anger_maladaptive_score" | lhs == "anger_adaptive_score_yeo" | 
          lhs == "ind_ext_ang_mal" | lhs == "ind_agr_ang_mal" | lhs == "ind_con_ang_mal" | lhs == "ind_neu_ang_mal" | lhs == "ind_ope_ang_mal" |
          lhs == "ind_ext_ang_ada" | lhs == "ind_agr_ang_ada" | lhs == "ind_con_ang_ada" | lhs == "ind_neu_ang_ada" | lhs == "ind_ope_ang_ada" |
          lhs == "tot_ext" | lhs == "tot_agr" | lhs == "tot_con" | lhs == "tot_neu" | lhs == "tot_ope") & 
          (op == "~" | op == ":=") & rhs != "sex") |>
  pull(pvalue, name = label)

cat("***Standardized parameter estimates Anger***\n")
print(standardizedSolution(fit_anger, ci = FALSE))

# End sink to stop capturing output in the text file ----
sink(file = NULL)

# FDR-correction of p-values by context and path coefficients ----
# Combine p-values from all models by context and path type for correction
p_values_gen_a <- c(p_values_general["a1"], p_values_general["a2"], 
                    p_values_general["a3"], p_values_general["a4"], 
                    p_values_general["a5"], p_values_general["a7"], 
                    p_values_general["a8"], p_values_general["a9"], 
                    p_values_general["a10"], p_values_general["a11"])
p_values_gen_b <- c(p_values_general["b1"], p_values_general["b2"])
p_values_gen_c <- c(p_values_general["c1"], p_values_general["c2"], 
                    p_values_general["c3"], p_values_general["c4"], 
                    p_values_general["c5"])
p_values_gen_ind <- c(p_values_general["ind_ext_mal"], p_values_general["ind_agr_mal"], 
                      p_values_general["ind_con_mal"], p_values_general["ind_neu_mal"], 
                      p_values_general["ind_ope_mal"], p_values_general["ind_ext_ada"], 
                      p_values_general["ind_agr_ada"], p_values_general["ind_con_ada"], 
                      p_values_general["ind_neu_ada"], p_values_general["ind_ope_ada"])
p_values_gen_tot <- c(p_values_general["tot_ext"], p_values_general["tot_agr"], 
                      p_values_general["tot_con"], p_values_general["tot_neu"], 
                      p_values_general["tot_ope"])

p_values_sad_a <- c(p_values_sadness["a1"], p_values_sadness["a2"], 
                    p_values_sadness["a3"], p_values_sadness["a4"], 
                    p_values_sadness["a5"], p_values_sadness["a7"], 
                    p_values_sadness["a8"], p_values_sadness["a9"], 
                    p_values_sadness["a10"], p_values_sadness["a11"])
p_values_sad_b <- c(p_values_sadness["b1"], p_values_sadness["b2"])
p_values_sad_c <- c(p_values_sadness["c1"], p_values_sadness["c2"], 
                    p_values_sadness["c3"], p_values_sadness["c4"], 
                    p_values_sadness["c5"])
p_values_sad_ind <- c(p_values_sadness["ind_ext_sad_mal"], p_values_sadness["ind_agr_sad_mal"], 
                      p_values_sadness["ind_con_sad_mal"], p_values_sadness["ind_neu_sad_mal"], 
                      p_values_sadness["ind_ope_sad_mal"], p_values_sadness["ind_ext_sad_ada"], 
                      p_values_sadness["ind_agr_sad_ada"], p_values_sadness["ind_con_sad_ada"], 
                      p_values_sadness["ind_neu_sad_ada"], p_values_sadness["ind_ope_sad_ada"])
p_values_sad_tot <- c(p_values_sadness["tot_ext"], p_values_sadness["tot_agr"], 
                      p_values_sadness["tot_con"], p_values_sadness["tot_neu"], 
                      p_values_sadness["tot_ope"])

p_values_anx_a <- c(p_values_anxiety["a1"], p_values_anxiety["a2"], 
                    p_values_anxiety["a3"], p_values_anxiety["a4"], 
                    p_values_anxiety["a5"], p_values_anxiety["a7"], 
                    p_values_anxiety["a8"], p_values_anxiety["a9"], 
                    p_values_anxiety["a10"], p_values_anxiety["a11"])
p_values_anx_b <- c(p_values_anxiety["b1"], p_values_anxiety["b2"])
p_values_anx_c <- c(p_values_anxiety["c1"], p_values_anxiety["c2"], 
                    p_values_anxiety["c3"], p_values_anxiety["c4"], 
                    p_values_anxiety["c5"])
p_values_anx_ind <- c(p_values_anxiety["ind_ext_anx_mal"], p_values_anxiety["ind_agr_anx_mal"], 
                      p_values_anxiety["ind_con_anx_mal"], p_values_anxiety["ind_neu_anx_mal"], 
                      p_values_anxiety["ind_ope_anx_mal"], p_values_anxiety["ind_ext_anx_ada"], 
                      p_values_anxiety["ind_agr_anx_ada"], p_values_anxiety["ind_con_anx_ada"], 
                      p_values_anxiety["ind_neu_anx_ada"], p_values_anxiety["ind_ope_anx_ada"])
p_values_anx_tot <- c(p_values_anxiety["tot_ext"], p_values_anxiety["tot_agr"], 
                      p_values_anxiety["tot_con"], p_values_anxiety["tot_neu"], 
                      p_values_anxiety["tot_ope"])

p_values_ang_a <- c(p_values_anger["a1"], p_values_anger["a2"], 
                    p_values_anger["a3"], p_values_anger["a4"], 
                    p_values_anger["a5"], p_values_anger["a7"], 
                    p_values_anger["a8"], p_values_anger["a9"], 
                    p_values_anger["a10"], p_values_anger["a11"])
p_values_ang_b <- c(p_values_anger["b1"], p_values_anger["b2"])
p_values_ang_c <- c(p_values_anger["c1"], p_values_anger["c2"], 
                    p_values_anger["c3"], p_values_anger["c4"], 
                    p_values_anger["c5"])
p_values_ang_ind <- c(p_values_anger["ind_ext_ang_mal"], p_values_anger["ind_agr_ang_mal"], 
                      p_values_anger["ind_con_ang_mal"], p_values_anger["ind_neu_ang_mal"], 
                      p_values_anger["ind_ope_ang_mal"], p_values_anger["ind_ext_ang_ada"], 
                      p_values_anger["ind_agr_ang_ada"], p_values_anger["ind_con_ang_ada"], 
                      p_values_anger["ind_neu_ang_ada"], p_values_anger["ind_ope_ang_ada"])
p_values_ang_tot <- c(p_values_anger["tot_ext"], p_values_anger["tot_agr"], 
                      p_values_anger["tot_con"], p_values_anger["tot_neu"], 
                      p_values_anger["tot_ope"])


# Apply FDR correction separately for each context and path type
p_values_gen_a_adj <- p.adjust(p_values_gen_a, method = "fdr")
p_values_gen_b_adj <- p.adjust(p_values_gen_b, method = "fdr")
p_values_gen_c_adj <- p.adjust(p_values_gen_c, method = "fdr")
p_values_gen_ind_adj <- p.adjust(p_values_gen_ind, method = "fdr")
p_values_gen_tot_adj <- p.adjust(p_values_gen_tot, method = "fdr")

p_values_sad_a_adj <- p.adjust(p_values_sad_a, method = "fdr")
p_values_sad_b_adj <- p.adjust(p_values_sad_b, method = "fdr")
p_values_sad_c_adj <- p.adjust(p_values_sad_c, method = "fdr")
p_values_sad_ind_adj <- p.adjust(p_values_sad_ind, method = "fdr")
p_values_sad_tot_adj <- p.adjust(p_values_sad_tot, method = "fdr")

p_values_anx_a_adj <- p.adjust(p_values_anx_a, method = "fdr")
p_values_anx_b_adj <- p.adjust(p_values_anx_b, method = "fdr")
p_values_anx_c_adj <- p.adjust(p_values_anx_c, method = "fdr")
p_values_anx_ind_adj <- p.adjust(p_values_anx_ind, method = "fdr")
p_values_anx_tot_adj <- p.adjust(p_values_anx_tot, method = "fdr")

p_values_ang_a_adj <- p.adjust(p_values_ang_a, method = "fdr")
p_values_ang_b_adj <- p.adjust(p_values_ang_b, method = "fdr")
p_values_ang_c_adj <- p.adjust(p_values_ang_c, method = "fdr")
p_values_ang_ind_adj <- p.adjust(p_values_ang_ind, method = "fdr")
p_values_ang_tot_adj <- p.adjust(p_values_ang_tot, method = "fdr")

# Save adjusted p-values to a CSV file for reporting
make_adj_tbl <- function(values, context_label, path_type_label) {
  tibble(
    context = context_label,
    path_type = path_type_label,
    parameter = names(values),
    p_value_adj = as.numeric(values)
  )
}

adjusted_p_values <- bind_rows(
  make_adj_tbl(p_values_gen_a_adj, "general", "a"),
  make_adj_tbl(p_values_gen_b_adj, "general", "b"),
  make_adj_tbl(p_values_gen_c_adj, "general", "c"),
  make_adj_tbl(p_values_gen_ind_adj, "general", "ind"),
  make_adj_tbl(p_values_gen_tot_adj, "general", "tot"),
  make_adj_tbl(p_values_sad_a_adj, "sadness", "a"),
  make_adj_tbl(p_values_sad_b_adj, "sadness", "b"),
  make_adj_tbl(p_values_sad_c_adj, "sadness", "c"),
  make_adj_tbl(p_values_sad_ind_adj, "sadness", "ind"),
  make_adj_tbl(p_values_sad_tot_adj, "sadness", "tot"),
  make_adj_tbl(p_values_anx_a_adj, "anxiety", "a"),
  make_adj_tbl(p_values_anx_b_adj, "anxiety", "b"),
  make_adj_tbl(p_values_anx_c_adj, "anxiety", "c"),
  make_adj_tbl(p_values_anx_ind_adj, "anxiety", "ind"),
  make_adj_tbl(p_values_anx_tot_adj, "anxiety", "tot"),
  make_adj_tbl(p_values_ang_a_adj, "anger", "a"),
  make_adj_tbl(p_values_ang_b_adj, "anger", "b"),
  make_adj_tbl(p_values_ang_c_adj, "anger", "c"),
  make_adj_tbl(p_values_ang_ind_adj, "anger", "ind"),
  make_adj_tbl(p_values_ang_tot_adj, "anger", "tot")
)

write_csv(adjusted_p_values, "data/analysis/mediation_adjusted_p_values.csv")

