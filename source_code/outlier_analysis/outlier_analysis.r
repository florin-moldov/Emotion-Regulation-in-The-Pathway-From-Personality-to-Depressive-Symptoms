# Outlier Analysis
# 
# This script performs outlier analysis for all linear regression and 
# mediation models. It identifies model fit outliers using leverage values and 
# studentized deleted residuals, and identifies influential outliers 
# (prediction outliers) using Cook's distance and DFFITS. The script then creates 
# a summary table of the outliers for each model, including the number of 
# outliers identified and their characteristics, and visualizes the results.

# Set seed for reproducibility ----
set.seed(123)

# Load necessary libraries ----
library(tidyverse)

# Load data ----
data <- read_csv("data/analysis/extended_data_with_transformations.csv")

# Define a function to perform outlier analysis for a given model (and plot) ----
perform_outlier_analysis <- function(model, model_name, save_plot = TRUE,
                plot_dir = "reports/plots/lm_diagnostics") {
  is_mlm <- inherits(model, "mlm")

  # Get leverage values and studentized deleted residuals
  leverage <- hatvalues(model)
  studentized_residuals <- rstudent(model)

  # Get Cook's distance and DFFITS
  cooks_distance <- cooks.distance(model)
  dffits_values <- dffits(model)

  n_obs <- nrow(model.frame(model))
  p <- if (is_mlm) nrow(coef(model)) else length(coef(model))

  # Define design-specific thresholds for outliers
  leverage_threshold <- 2 * (p + 1) / n_obs
  studentized_threshold <- qt((1 - 0.05 / (2 * n_obs)), df = (n_obs - p - 1))
  cooks_threshold <- qf(0.5, (p + 1), (n_obs - p - 1))
  dffits_threshold <- 2 * sqrt((p + 1) / n_obs)

  # Identify outliers based on thresholds
  leverage_outliers <- which(leverage > leverage_threshold)

  if (is_mlm) {
    studentized_abs_max <- apply(abs(studentized_residuals), 1, max, na.rm = TRUE)
    cooks_max <- apply(cooks_distance, 1, max, na.rm = TRUE)
    dffits_abs_max <- apply(abs(dffits_values), 1, max, na.rm = TRUE)

    studentized_outliers <- which(studentized_abs_max > studentized_threshold)
    cooks_outliers <- which(cooks_max > cooks_threshold)
    dffits_outliers <- which(dffits_abs_max > dffits_threshold)

    studentized_plot_values <- studentized_abs_max
    cooks_plot_values <- cooks_max
    dffits_plot_values <- dffits_abs_max
  } else {
    studentized_outliers <- which(abs(studentized_residuals) > studentized_threshold)
    cooks_outliers <- which(cooks_distance > cooks_threshold)
    dffits_abs <- abs(dffits_values)
    dffits_outliers <- which(dffits_abs > dffits_threshold)

    studentized_plot_values <- studentized_residuals
    cooks_plot_values <- cooks_distance
    dffits_plot_values <- dffits_abs
  }

  if (save_plot) {
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    safe_model_name <- gsub("[^A-Za-z0-9_-]", "_", model_name)
    plot_path <- file.path(plot_dir, paste0("outlier_diagnostics_", safe_model_name, ".svg"))

    svg(filename = plot_path, width = 12, height = 10)
    old_par <- par(no.readonly = TRUE)
    on.exit({
   par(old_par)
   dev.off()
    }, add = TRUE)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

    plot(leverage, pch = 16, cex = 0.7, col = "grey40",
      main = paste0(model_name, ": Leverage"),
      xlab = "Observation", ylab = "Hat value")
    abline(h = leverage_threshold, col = "red", lwd = 2, lty = 2)
    points(leverage_outliers, leverage[leverage_outliers], pch = 16, col = "red")

    plot(studentized_plot_values, pch = 16, cex = 0.7, col = "grey40",
      main = if (is_mlm) paste0(model_name, ": max |Studentized residual|") else paste0(model_name, ": Studentized residuals"),
      xlab = "Observation", ylab = if (is_mlm) "max |Studentized residual|" else "Studentized residual")
    abline(h = if (is_mlm) studentized_threshold else c(-studentized_threshold, studentized_threshold), col = "red", lwd = 2, lty = 2)
    points(studentized_outliers, studentized_plot_values[studentized_outliers], pch = 16, col = "red")

    plot(cooks_plot_values, pch = 16, cex = 0.7, col = "grey40",
      main = if (is_mlm) paste0(model_name, ": max Cook's distance") else paste0(model_name, ": Cook's distance"),
      xlab = "Observation", ylab = if (is_mlm) "max Cook's distance" else "Cook's distance")
    abline(h = cooks_threshold, col = "red", lwd = 2, lty = 2)
    points(cooks_outliers, cooks_plot_values[cooks_outliers], pch = 16, col = "red")

    plot(dffits_plot_values, pch = 16, cex = 0.7, col = "grey40",
      main = if (is_mlm) paste0(model_name, ": max |DFFITS|") else paste0(model_name, ": |DFFITS|"),
      xlab = "Observation", ylab = if (is_mlm) "max |DFFITS|" else "|DFFITS|")
    abline(h = dffits_threshold, col = "red", lwd = 2, lty = 2)
    points(dffits_outliers, dffits_plot_values[dffits_outliers], pch = 16, col = "red")
  }

  # Combine unique outlier IDs into a single list
  all_outliers <- unique(c(leverage_outliers, studentized_outliers, 
                           cooks_outliers, dffits_outliers))
  model_data <- model.frame(model)
  used_rows <- as.integer(rownames(model_data))
  all_outlier_ids <- data$ID[used_rows[all_outliers]]

  # Create a summary table of outliers
  outlier_summary <- tibble(
    Model = model_name,
    Leverage_Outliers = length(leverage_outliers),
    Studentized_Outliers = length(studentized_outliers),
    Cooks_Outliers = length(cooks_outliers),
    DFFITS_Outliers = length(dffits_outliers)
 )
    
  return(list(all_outlier_ids = all_outlier_ids, outlier_summary = outlier_summary))
}

# Perform outlier analysis for each model ----
# H1 model ----
fit_h1 <- lm(depression_score_yeo ~ extraversion + agreeableness +
                                 conscientiousness + neuroticism + openness +
                                 sex, data = data)
results <- perform_outlier_analysis(fit_h1, "H1")

# Save the output 
# Outlier IDs
write_csv(tibble(ID = results$all_outlier_ids), "data/analysis/h1_outliers.csv")

# Outlier summary
write_csv(results$outlier_summary, "data/analysis/h1_outlier_summary.csv")

# H2 models ----
# Emotion-general ----
fit_h2_general <- lm(depression_score_yeo ~ maladaptive_score +
                                            adaptive_score +
                                            sex, data = data)
results_h2_general <- perform_outlier_analysis(fit_h2_general, "H2_General")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h2_general$all_outlier_ids), "data/analysis/h2_general_outliers.csv")

# Outlier summary
write_csv(results_h2_general$outlier_summary, "data/analysis/h2_general_outlier_summary.csv")

# Sadness-specific ----
fit_h2_sadness <- lm(depression_score_yeo ~ sadness_maladaptive_score +
                                            sadness_adaptive_score +
                                            sex, data = data)
results_h2_sadness <- perform_outlier_analysis(fit_h2_sadness, "H2_Sadness")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h2_sadness$all_outlier_ids), "data/analysis/h2_sadness_outliers.csv")

# Outlier summary
write_csv(results_h2_sadness$outlier_summary, "data/analysis/h2_sadness_outlier_summary.csv")

# Anxiety-specific ----
fit_h2_anxiety <- lm(depression_score_yeo ~ anxiety_maladaptive_score +
                                            anxiety_adaptive_score +
                                            sex, data = data)
results_h2_anxiety <- perform_outlier_analysis(fit_h2_anxiety, "H2_Anxiety")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h2_anxiety$all_outlier_ids), "data/analysis/h2_anxiety_outliers.csv")

# Outlier summary
write_csv(results_h2_anxiety$outlier_summary, "data/analysis/h2_anxiety_outlier_summary.csv")

# Anger-specific ----
fit_h2_anger <- lm(depression_score_yeo ~ anger_maladaptive_score +
                                            anger_adaptive_score +
                                            sex, data = data)
results_h2_anger <- perform_outlier_analysis(fit_h2_anger, "H2_Anger")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h2_anger$all_outlier_ids), "data/analysis/h2_anger_outliers.csv")

# Outlier summary
write_csv(results_h2_anger$outlier_summary, "data/analysis/h2_anger_outlier_summary.csv")

# H3 models ----
# General ----
fit_h3_general <- lm(cbind(maladaptive_score, adaptive_score_box) ~ extraversion + agreeableness +
                                            conscientiousness + neuroticism + openness + sex, data = data)
results_h3_general <- perform_outlier_analysis(fit_h3_general, "H3_General")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h3_general$all_outlier_ids), "data/analysis/h3_general_outliers.csv")

# Outlier summary
write_csv(results_h3_general$outlier_summary, "data/analysis/h3_general_outlier_summary.csv")

# Sadness-specific ----
fit_h3_sadness <- lm(cbind(sadness_maladaptive_score, sadness_adaptive_score_ord) ~ extraversion + agreeableness +
                                            conscientiousness + neuroticism + openness + sex, data = data)
results_h3_sadness <- perform_outlier_analysis(fit_h3_sadness, "H3_Sadness")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h3_sadness$all_outlier_ids), "data/analysis/h3_sadness_outliers.csv")

# Outlier summary
write_csv(results_h3_sadness$outlier_summary, "data/analysis/h3_sadness_outlier_summary.csv")

# Anxiety-specific ----
fit_h3_anxiety <- lm(cbind(anxiety_maladaptive_score, anxiety_adaptive_score_box) ~ extraversion + agreeableness +
                                            conscientiousness + neuroticism + openness + sex, data = data)
results_h3_anxiety <- perform_outlier_analysis(fit_h3_anxiety, "H3_Anxiety")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h3_anxiety$all_outlier_ids), "data/analysis/h3_anxiety_outliers.csv")

# Outlier summary
write_csv(results_h3_anxiety$outlier_summary, "data/analysis/h3_anxiety_outlier_summary.csv")

# Anger-specific ----
fit_h3_anger <- lm(cbind(anger_maladaptive_score, anger_adaptive_score_yeo) ~ extraversion + agreeableness +
                                            conscientiousness + neuroticism + openness + sex, data = data)
results_h3_anger <- perform_outlier_analysis(fit_h3_anger, "H3_Anger")

# Save the output
# Outlier IDs
write_csv(tibble(ID = results_h3_anger$all_outlier_ids), "data/analysis/h3_anger_outliers.csv")

# Outlier summary
write_csv(results_h3_anger$outlier_summary, "data/analysis/h3_anger_outlier_summary.csv")

# Combine all outlier summaries into a single table ----
all_outlier_summaries <- bind_rows(
  results$outlier_summary,
  results_h2_general$outlier_summary,
  results_h2_sadness$outlier_summary,
  results_h2_anxiety$outlier_summary,
  results_h2_anger$outlier_summary,
  results_h3_general$outlier_summary,
  results_h3_sadness$outlier_summary,
  results_h3_anxiety$outlier_summary,
  results_h3_anger$outlier_summary
)
write_csv(all_outlier_summaries, "data/analysis/all_outlier_summaries.csv")

# Get all unique outlier IDs across all models ----
# Each model's result is already a list with unique outlier IDs across the different methods for that model, so we can just combine those lists and get unique IDs across all models
all_outlier_ids <- unique(c(
  results$all_outlier_ids,
  results_h2_general$all_outlier_ids,
  results_h2_sadness$all_outlier_ids,
  results_h2_anxiety$all_outlier_ids,
  results_h2_anger$all_outlier_ids,
  results_h3_general$all_outlier_ids,
  results_h3_sadness$all_outlier_ids,
  results_h3_anxiety$all_outlier_ids,
  results_h3_anger$all_outlier_ids
))

print(paste("Total unique outliers across all models:", length(all_outlier_ids)))

print("Unique outlier IDs per emotion context:")
print(paste("General:", length(unique(c(results$all_outlier_ids, results_h2_general$all_outlier_ids, results_h3_general$all_outlier_ids)))))
print(paste("Sadness:", length(unique(c(results$all_outlier_ids, results_h2_sadness$all_outlier_ids, results_h3_sadness$all_outlier_ids)))))
print(paste("Anxiety:", length(unique(c(results$all_outlier_ids, results_h2_anxiety$all_outlier_ids, results_h3_anxiety$all_outlier_ids)))))
print(paste("Anger:", length(unique(c(results$all_outlier_ids, results_h2_anger$all_outlier_ids, results_h3_anger$all_outlier_ids)))))

write_csv(tibble(ID = all_outlier_ids), "data/analysis/all_unique_outliers.csv")