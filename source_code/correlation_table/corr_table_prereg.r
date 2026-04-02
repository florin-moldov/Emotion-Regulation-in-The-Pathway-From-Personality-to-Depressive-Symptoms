# Preliminary analysis (correlation table) ----
# Correlation table between HiPIC dimension,
# FEEL-E scores, and ASR Depression scores (preregistration version)

# Load necessary libraries ----
library(tidyverse)
library(GGally)
library(ggcorrplot)
library(psych)

# Load data ----
w6_hipic <- read_csv("data/preprocessed/preproc_w6_hipic.csv")
w10_feel <- read_csv("data/preprocessed/preproc_w10_feel.csv")
w11_asr <- read_csv("data/preprocessed/preproc_w11_asr.csv")

# Harmonize join key type
w6_hipic <- w6_hipic |> mutate(ID = trimws(as.character(ID)))
w10_feel <- w10_feel |> mutate(ID = trimws(as.character(ID)))
w11_asr <- w11_asr |> mutate(ID = trimws(as.character(ID)))

# Merge datasets on participant ID ----
# And only for the average scores of interest
merged_data <- w6_hipic |>
  select(ID, extraversion, agreeableness, conscientiousness,
         neuroticism, openness) |>
  full_join(w10_feel |>
              select(ID, sadness_adaptive_score, anxiety_adaptive_score,
                     anger_adaptive_score, adaptive_score,
                     sadness_maladaptive_score, anxiety_maladaptive_score,
                     anger_maladaptive_score,
                     maladaptive_score), by = "ID") |>
  full_join(w11_asr |>
              select(ID, depression_score), by = "ID")

# Delete those with empty IDs (identified in demographics.r)
merged_data <- merged_data |> 
                filter(!is.na(ID))

# Check nrows after merge
cat("Number of rows after merge:", nrow(merged_data), "\n")

# Save merged data for future use
write_csv(merged_data, "data/merged/merged_data.csv")

# Visualize relationships with pairwise scatterplots ----
# As initial exploration
# We can use ggpairs for a quick overview
p <- ggpairs(merged_data |> select(-ID),
             lower = list(continuous = wrap("points", alpha = 0.5)),
             diag = list(continuous = wrap("densityDiag")),
             upper = "blank") +
  theme_minimal() +
  ggtitle("Pairwise Scatterplots and Density Plots")

ggsave("reports/plots/pairwise_scatterplots.svg",
       plot = p, width = 20, height = 20)

# Checking normality of variables (for correlation method choice) ----
# We can use QQPlots and Shapiro-Wilk tests
numeric_vars <- merged_data |> select(-ID)
for (var in names(numeric_vars)) {
  cat("Variable:", var, "\n")
  # QQ Plot
  p <- ggplot(merged_data, aes_string(sample = var)) +
    geom_qq() +
    geom_qq_line() +
    ggtitle(paste("QQ Plot for", var)) +
    theme_minimal()

  ggsave(paste0("reports/plots/qqplots/qq_", var, ".svg"),
         plot = p, width = 6, height = 4)

  # Shapiro-Wilk test
  shapiro_result <- shapiro.test(merged_data[[var]])
  cat("Shapiro-Wilk p-value:", shapiro_result$p.value, "\n\n")
}
# Based on the normality checks, we decided to
# use Spearman's rank correlation for all variables.

# Compute correlation matrix ----
corr_results <- merged_data |>
  select(-ID) |>
  # no p-value adjustment here, we'll do it separately by context
  corr.test(method = "spearman", use = "complete", adjust = "none")

cor_matrix <- corr_results$r
pvalue_matrix <- corr_results$p

# Save correlation results for future use
write_csv(as.data.frame(cor_matrix), "data/analysis/correlation_matrix.csv")
write_csv(as.data.frame(pvalue_matrix), "data/analysis/correlation_pvalues.csv")

# Apply FDR correction to p-values ----
# Important to note: correction is applied within
# each emotion context (i.e., general, sadness, anxiety, anger)

# Get variable groups for each context
contexts <- list(
  general = c("extraversion", "agreeableness",
              "conscientiousness", "neuroticism", "openness",
              "adaptive_score", "maladaptive_score", "depression_score"),
  sadness = c("extraversion", "agreeableness",
              "conscientiousness", "neuroticism", "openness",
              "sadness_adaptive_score", "sadness_maladaptive_score",
              "depression_score"),
  anxiety = c("extraversion", "agreeableness",
              "conscientiousness", "neuroticism", "openness",
              "anxiety_adaptive_score", "anxiety_maladaptive_score",
              "depression_score"),
  anger = c("extraversion", "agreeableness",
            "conscientiousness", "neuroticism", "openness",
            "anger_adaptive_score", "anger_maladaptive_score",
            "depression_score")
)

# Extract and adjust p-values by context with explicit pair mapping
adjusted_pairs <- lapply(names(contexts), function(ctx) {
  vars <- contexts[[ctx]]
  submat <- pvalue_matrix[vars, vars, drop = FALSE]
  idx <- which(upper.tri(submat, diag = FALSE), arr.ind = TRUE)

  pair_tbl <- tibble(
    var1 = rownames(submat)[idx[, 1]],
    var2 = colnames(submat)[idx[, 2]],
    p_raw = submat[idx]
  )

  pair_tbl$p_adj <- p.adjust(pair_tbl$p_raw, method = "BH")
  pair_tbl
})
names(adjusted_pairs) <- names(contexts)

# Build one adjusted p-value matrix per context
adjusted_pvalue_matrices <- lapply(names(contexts), function(ctx) {
  vars <- contexts[[ctx]]
  pair_tbl <- adjusted_pairs[[ctx]]

  submat <- matrix(NA_real_, nrow = length(vars), ncol = length(vars),
                   dimnames = list(vars, vars))

  submat[cbind(pair_tbl$var1, pair_tbl$var2)] <- pair_tbl$p_adj
  submat[cbind(pair_tbl$var2, pair_tbl$var1)] <- pair_tbl$p_adj
  diag(submat) <- NA_real_
  submat
})
names(adjusted_pvalue_matrices) <- names(contexts)

# Save adjusted p-value matrices per context
invisible(lapply(names(adjusted_pvalue_matrices), function(ctx) {
  write_csv(as.data.frame(adjusted_pvalue_matrices[[ctx]]),
            paste0("data/analysis/correlation_pvalues_adjusted_", ctx, ".csv"))
}))

# Visualize correlation matrices with significance annotations ----
# We can use ggcorrplot for this purpose
invisible(lapply(names(contexts), function(ctx) {
  vars <- contexts[[ctx]]
  corr_submat <- cor_matrix[vars, vars, drop = FALSE]
  p_adj_submat <- adjusted_pvalue_matrices[[ctx]]
  p <- ggcorrplot(corr_submat, method = "square",
                  type = "upper", lab = TRUE,
                  p.mat = p_adj_submat, sig.level = 0.05,
                  insig = "pch", outline.color = "white",
                  colors = c("#1100ff", "white", "#ff0000")) +
    ggtitle(paste("Correlation Matrix -", ctx)) +
    labs(y = "Variables", x = "Variables") +
    scale_y_discrete(labels = function(x) stringr::str_wrap(stringr::str_replace_all(x, "_", " "), width = 10)) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(stringr::str_replace_all(x, "_", " "), width = 10)) +
    theme_minimal()
  ggsave(paste0("reports/plots/correlation_matrix_", ctx, ".svg"),
         plot = p, width = 10, height = 10)
}))