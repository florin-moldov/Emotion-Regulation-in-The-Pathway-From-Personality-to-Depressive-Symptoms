# W10 FEEL-E preprocessing ----
#
# This script preprocesses the FEEL-E data for Wave 10.
# It reads the raw data and prepares the dataset for analysis.

# Set seed for reproducibility ----
set.seed(123)

# Set sink to capture all output in a text file ----
sink("data/preprocessed/feel_preproc_output.txt")

# Load necessary libraries ----
library(haven)  # For reading SPSS files
library(jsonlite)  # For saving metadata as JSON
library(tidyverse)  # For data manipulation and visualization
library(psych)  # For psychometric properties like Cronbach's alpha

# Read the raw FEEL data for Wave 10
raw_w10_feel <- read_sav("data/raw/W10_FEEL+demo.sav")

# Compute FEEL-E general and emotion-specific scores ----
# Compute general (i.e., aggregated across the 3 emotions),
# sadness-specific, anxiety-specific, and anger-specific
# scores separately for adaptive and maladaptive emotion regulation
# strategy use

preproc_w10_feel <- raw_w10_feel |>
  select(ID, starts_with("IVFEEL")) |>
  mutate(
    sadness_adaptive_score = rowMeans(across(c(IVFEEL5_1, IVFEEL5_2, IVFEEL5_7,
                                               IVFEEL5_8, IVFEEL5_9, IVFEEL5_10,
                                               IVFEEL6_1, IVFEEL6_2, IVFEEL6_4,
                                               IVFEEL6_7, IVFEEL6_8,
                                               IVFEEL6_9)), na.rm = FALSE),
    anxiety_adaptive_score = rowMeans(across(c(IVFEEL3_1, IVFEEL3_2, IVFEEL3_7,
                                               IVFEEL3_8, IVFEEL3_9, IVFEEL3_10,
                                               IVFEEL4_1, IVFEEL4_2, IVFEEL4_4,
                                               IVFEEL4_7, IVFEEL4_8,
                                               IVFEEL4_9)), na.rm = FALSE),
    anger_adaptive_score = rowMeans(across(c(IVFEEL1_1, IVFEEL1_2, IVFEEL1_7,
                                             IVFEEL1_8, IVFEEL1_9, IVFEEL1_10,
                                             IVFEEL2_1, IVFEEL2_2, IVFEEL2_4,
                                             IVFEEL2_7, IVFEEL2_8,
                                             IVFEEL2_9)), na.rm = FALSE),
    adaptive_score = rowMeans(across(c(IVFEEL1_1, IVFEEL1_2, IVFEEL1_7,
                                       IVFEEL1_8, IVFEEL1_9, IVFEEL1_10,
                                       IVFEEL2_1, IVFEEL2_2, IVFEEL2_4,
                                       IVFEEL2_7, IVFEEL2_8, IVFEEL2_9,
                                       IVFEEL3_1, IVFEEL3_2, IVFEEL3_7,
                                       IVFEEL3_8, IVFEEL3_9, IVFEEL3_10,
                                       IVFEEL4_1, IVFEEL4_2, IVFEEL4_4,
                                       IVFEEL4_7, IVFEEL4_8, IVFEEL4_9,
                                       IVFEEL5_1, IVFEEL5_2, IVFEEL5_7,
                                       IVFEEL5_8, IVFEEL5_9, IVFEEL5_10,
                                       IVFEEL6_1, IVFEEL6_2, IVFEEL6_4,
                                       IVFEEL6_7, IVFEEL6_8, IVFEEL6_9)),
                              na.rm = FALSE),
    sadness_maladaptive_score = rowMeans(across(c(IVFEEL5_3, IVFEEL5_4,
                                                  IVFEEL5_5, IVFEEL5_6,
                                                  IVFEEL5_11, IVFEEL5_12,
                                                  IVFEEL6_3, IVFEEL6_5,
                                                  IVFEEL6_6, IVFEEL6_10,
                                                  IVFEEL6_11, IVFEEL6_12)),
                                         na.rm = FALSE),
    anxiety_maladaptive_score = rowMeans(across(c(IVFEEL3_3, IVFEEL3_4,
                                                  IVFEEL3_5, IVFEEL3_6,
                                                  IVFEEL3_11, IVFEEL3_12,
                                                  IVFEEL4_3, IVFEEL4_5,
                                                  IVFEEL4_6, IVFEEL4_10,
                                                  IVFEEL4_11, IVFEEL4_12)),
                                         na.rm = FALSE),
    anger_maladaptive_score = rowMeans(across(c(IVFEEL1_3, IVFEEL1_4,
                                                IVFEEL1_5, IVFEEL1_6,
                                                IVFEEL1_11, IVFEEL1_12,
                                                IVFEEL2_3, IVFEEL2_5,
                                                IVFEEL2_6, IVFEEL2_10,
                                                IVFEEL2_11, IVFEEL2_12)),
                                       na.rm = FALSE),
    maladaptive_score = rowMeans(across(c(IVFEEL1_3, IVFEEL1_4, IVFEEL1_5,
                                          IVFEEL1_6, IVFEEL1_11, IVFEEL1_12,
                                          IVFEEL2_3, IVFEEL2_5, IVFEEL2_6,
                                          IVFEEL2_10, IVFEEL2_11, IVFEEL2_12,
                                          IVFEEL3_3, IVFEEL3_4, IVFEEL3_5,
                                          IVFEEL3_6, IVFEEL3_11, IVFEEL3_12,
                                          IVFEEL4_3, IVFEEL4_5, IVFEEL4_6,
                                          IVFEEL4_10, IVFEEL4_11, IVFEEL4_12,
                                          IVFEEL5_3, IVFEEL5_4, IVFEEL5_5,
                                          IVFEEL5_6, IVFEEL5_11, IVFEEL5_12,
                                          IVFEEL6_3, IVFEEL6_5, IVFEEL6_6,
                                          IVFEEL6_10, IVFEEL6_11, IVFEEL6_12)),
                                 na.rm = FALSE)
  )

# Display the structure of the preprocessed dataset ----
str(preproc_w10_feel) # should be 81 columns
# (ID + 72 FEEL-E items + 8 computed scores) and 369 rows (participants)

# Simplify metadata before opening viewer (because
# otherwise viewer might crash due to too much metadata)
safe_w10_feel <- as.data.frame(raw_w10_feel)
safe_w10_feel[] <- lapply(safe_w10_feel, haven::zap_labels)

# Save the metadata (variable labels and value labels) to a JSON file
meta <- lapply(names(preproc_w10_feel), function(nm) {
  x <- preproc_w10_feel[[nm]]
  list(
    variable = nm,
    variable_label = attr(x, "label"),
    value_labels = as.list(attr(x, "labels"))
  )
})
write_json(meta, "data/preprocessed/preproc_w10_feel_metadata.json",
           pretty = TRUE, auto_unbox = TRUE)

# Save the preprocessed dataset for future use ----
write.csv(preproc_w10_feel, file = "data/preprocessed/preproc_w10_feel.csv",
          quote = FALSE,
          row.names = FALSE)

# Descriptive statistics for the FEEL-E scores ----
# Ofc, only for non-missing values, so we set na.rm = TRUE
# vector of dimension column names (as created in preproc)
dims <- c("sadness_adaptive_score", "anxiety_adaptive_score",
          "anger_adaptive_score", "adaptive_score",
          "sadness_maladaptive_score", "anxiety_maladaptive_score",
          "anger_maladaptive_score", "maladaptive_score")
feel_stats <- preproc_w10_feel |>
  summarise(
    across(
      all_of(dims),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        n = ~ sum(!is.na(.x)),
        missing = ~ sum(is.na(.x))
      ),
      .names = "{.col}_{.fn}"   # dimension_stat
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("dimension", "stat"),
    names_pattern = "^(.+)_(.+)$",
    values_to = "value"
  ) |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(dimension = factor(dimension, levels = dims))

print(feel_stats)

# Save the tidy table
write_csv(feel_stats, "data/descriptives/feel_stats_w10.csv")

# Add demographics to the preprocessed HiPIC dataset ----
preproc_w10_demo <- read_csv("data/preprocessed/preproc_w10_demographics.csv")
preproc_w10_feel <- preproc_w10_feel |>
  left_join(preproc_w10_demo, by = "ID")

# Save the updated dataset with demographics for future use
write.csv(preproc_w10_feel, file = "data/preprocessed/preproc_w10_feel.csv",
          quote = FALSE,
          row.names = FALSE)

# Determine number of NaNs in each column (missing values) ----
num_nans <- sapply(preproc_w10_feel, function(x) {
  if (is.character(x)) {
    sum(is.na(x) | trimws(x) == "")
  } else if (is.factor(x)) {
    x_chr <- as.character(x)
    sum(is.na(x_chr) | trimws(x_chr) == "")
  } else {
    sum(is.na(x))
  }
})
cat("Number of NaNs in each column:\n")
print(num_nans)
cat("Total number of NaNs:", sum(num_nans), "\n") # 1869 missing values in total
# (so across all columns and all participants) for ASR W10 data
# with 19 for sadness_adaptive_score, 18 for anxiety_adaptive_score,
# 18 for anger_adaptive_score, 19 for adaptive_score,
# 19 for sadness_maladaptive_score, 18 for anxiety_maladaptive_score,
# 18 for anger_maladaptive_score, 19 for maladaptive_score,
# 10 for parttime_edu, 1 for highest_edu,
# 369 for highest_edu_text, 1 for employment_status,
# 32 for biological sex, and 1308 for the individual item columns

# Psychometric properties ----
sadness_adaptive_items <- c("IVFEEL5_1", "IVFEEL5_2", "IVFEEL5_7",
                            "IVFEEL5_8", "IVFEEL5_9", "IVFEEL5_10",
                            "IVFEEL6_1", "IVFEEL6_2", "IVFEEL6_4",
                            "IVFEEL6_7", "IVFEEL6_8", "IVFEEL6_9")
print(
  preproc_w10_feel |>
    select(all_of(sadness_adaptive_items)) |>
    alpha(n.iter = 10000,
          title = "Sadness Adaptive Scale")
)

anxiety_adaptive_items <- c("IVFEEL3_1", "IVFEEL3_2", "IVFEEL3_7",
                            "IVFEEL3_8", "IVFEEL3_9", "IVFEEL3_10",
                            "IVFEEL4_1", "IVFEEL4_2", "IVFEEL4_4",
                            "IVFEEL4_7", "IVFEEL4_8", "IVFEEL4_9")
print(
  preproc_w10_feel |>
    select(all_of(anxiety_adaptive_items)) |>
    alpha(n.iter = 10000,
          title = "Anxiety Adaptive Scale")
)

anger_adaptive_items <- c("IVFEEL1_1", "IVFEEL1_2", "IVFEEL1_7",
                          "IVFEEL1_8", "IVFEEL1_9", "IVFEEL1_10",
                          "IVFEEL2_1", "IVFEEL2_2", "IVFEEL2_4",
                          "IVFEEL2_7", "IVFEEL2_8", "IVFEEL2_9")
print(
  preproc_w10_feel |>
    select(all_of(anger_adaptive_items)) |>
    alpha(n.iter = 10000,
          title = "Anger Adaptive Scale")
)

adaptive_items <- c("IVFEEL1_1", "IVFEEL1_2", "IVFEEL1_7",
                    "IVFEEL1_8", "IVFEEL1_9", "IVFEEL1_10",
                    "IVFEEL2_1", "IVFEEL2_2", "IVFEEL2_4",
                    "IVFEEL2_7", "IVFEEL2_8", "IVFEEL2_9",
                    "IVFEEL3_1", "IVFEEL3_2", "IVFEEL3_7",
                    "IVFEEL3_8", "IVFEEL3_9", "IVFEEL3_10",
                    "IVFEEL4_1", "IVFEEL4_2", "IVFEEL4_4",
                    "IVFEEL4_7", "IVFEEL4_8", "IVFEEL4_9",
                    "IVFEEL5_1", "IVFEEL5_2", "IVFEEL5_7",
                    "IVFEEL5_8", "IVFEEL5_9", "IVFEEL5_10",
                    "IVFEEL6_1", "IVFEEL6_2", "IVFEEL6_4",
                    "IVFEEL6_7", "IVFEEL6_8", "IVFEEL6_9")
print(
  preproc_w10_feel |>
    select(all_of(adaptive_items)) |>
    alpha(n.iter = 10000,
          title = "General Adaptive Scale")
)

sadness_maladaptive_items <- c("IVFEEL5_3", "IVFEEL5_4",
                               "IVFEEL5_5", "IVFEEL5_6",
                               "IVFEEL5_11", "IVFEEL5_12",
                               "IVFEEL6_3", "IVFEEL6_5",
                               "IVFEEL6_6", "IVFEEL6_10",
                               "IVFEEL6_11", "IVFEEL6_12")
print(
  preproc_w10_feel |>
    select(all_of(sadness_maladaptive_items)) |>
    alpha(n.iter = 10000,
          title = "Sadness Maladaptive Scale")
)

anxiety_maladaptive_items <- c("IVFEEL3_3", "IVFEEL3_4",
                               "IVFEEL3_5", "IVFEEL3_6",
                               "IVFEEL3_11", "IVFEEL3_12",
                               "IVFEEL4_3", "IVFEEL4_5",
                               "IVFEEL4_6", "IVFEEL4_10",
                               "IVFEEL4_11", "IVFEEL4_12")
print(
  preproc_w10_feel |>
    select(all_of(anxiety_maladaptive_items)) |>
    alpha(n.iter = 10000,
          title = "Anxiety Maladaptive Scale")
)

anger_maladaptive_items <- c("IVFEEL1_3", "IVFEEL1_4",
                             "IVFEEL1_5", "IVFEEL1_6",
                             "IVFEEL1_11", "IVFEEL1_12",
                             "IVFEEL2_3", "IVFEEL2_5",
                             "IVFEEL2_6", "IVFEEL2_10",
                             "IVFEEL2_11", "IVFEEL2_12")
print(
  preproc_w10_feel |>
    select(all_of(anger_maladaptive_items)) |>
    alpha(n.iter = 10000,
          title = "Anger Maladaptive Scale")
)

maladaptive_items <- c("IVFEEL1_3", "IVFEEL1_4", "IVFEEL1_5",
                       "IVFEEL1_6", "IVFEEL1_11", "IVFEEL1_12",
                       "IVFEEL2_3", "IVFEEL2_5", "IVFEEL2_6",
                       "IVFEEL2_10", "IVFEEL2_11", "IVFEEL2_12",
                       "IVFEEL3_3", "IVFEEL3_4", "IVFEEL3_5",
                       "IVFEEL3_6", "IVFEEL3_11", "IVFEEL3_12",
                       "IVFEEL4_3", "IVFEEL4_5", "IVFEEL4_6",
                       "IVFEEL4_10", "IVFEEL4_11", "IVFEEL4_12",
                       "IVFEEL5_3", "IVFEEL5_4", "IVFEEL5_5",
                       "IVFEEL5_6", "IVFEEL5_11", "IVFEEL5_12",
                       "IVFEEL6_3", "IVFEEL6_5", "IVFEEL6_6",
                       "IVFEEL6_10", "IVFEEL6_11", "IVFEEL6_12")
print(
  preproc_w10_feel |>
    select(all_of(maladaptive_items)) |>
    alpha(n.iter = 10000,
          title = "General Maladaptive Scale")
)

# End sink to stop capturing output in the text file ----
sink(file = NULL)

# Distribution of FEEL-E scores ----
# By sex
# First get the labels for the sex codes
# Read metadata JSON
meta <- fromJSON("data/preprocessed/preproc_w10_demographics_metadata.json",
                 simplifyVector = FALSE)

# Get value labels for the original W10 sex variable
sex_meta <- meta[[which(vapply(meta,
                               function(x) x$variable == "geslacht",
                               logical(1)))]]

# JSON stores value_labels as label -> code, so invert to code -> label
code_to_label <- setNames(
  names(sex_meta$value_labels),
  as.character(unlist(sex_meta$value_labels))
)

# Actual plotting
sadness_adaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(sadness_adaptive_score)),
    mapping = aes(x = sadness_adaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sadness_adaptive_score)),
    mapping = aes(x = sadness_adaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average Sadness Adaptive Scores (FEEL W10)",
    x = "Average Sadness Adaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_sadness_adaptive_sex_plot.svg",
       plot = sadness_adaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

anxiety_adaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(anxiety_adaptive_score)),
    mapping = aes(x = anxiety_adaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(anxiety_adaptive_score)),
    mapping = aes(x = anxiety_adaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average Anxiety Adaptive Scores (FEEL W10)",
    x = "Average Anxiety Adaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_anxiety_adaptive_sex_plot.svg",
       plot = anxiety_adaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

anger_adaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(anger_adaptive_score)),
    mapping = aes(x = anger_adaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(anger_adaptive_score)),
    mapping = aes(x = anger_adaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average Anger Adaptive Scores (FEEL W10)",
    x = "Average Anger Adaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_anger_adaptive_sex_plot.svg",
       plot = anger_adaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

adaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(adaptive_score)),
    mapping = aes(x = adaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(adaptive_score)),
    mapping = aes(x = adaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average General Adaptive Scores (FEEL W10)",
    x = "Average General Adaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_adaptive_sex_plot.svg",
       plot = adaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

sadness_maladaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(sadness_maladaptive_score)),
    mapping = aes(x = sadness_maladaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sadness_maladaptive_score)),
    mapping = aes(x = sadness_maladaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average Sadness Maladaptive Scores (FEEL W10)",
    x = "Average Sadness Maladaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_sadness_maladaptive_sex_plot.svg",
       plot = sadness_maladaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

anxiety_maladaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(anxiety_maladaptive_score)),
    mapping = aes(x = anxiety_maladaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(anxiety_maladaptive_score)),
    mapping = aes(x = anxiety_maladaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average Anxiety Maladaptive Scores (FEEL W10)",
    x = "Average Anxiety Maladaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_anxiety_maladaptive_sex_plot.svg",
       plot = anxiety_maladaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

anger_maladaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(anger_maladaptive_score)),
    mapping = aes(x = anger_maladaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(anger_maladaptive_score)),
    mapping = aes(x = anger_maladaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average Anger Maladaptive Scores (FEEL W10)",
    x = "Average Anger Maladaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_anger_maladaptive_sex_plot.svg",
       plot = anger_maladaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

maladaptive_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w10_feel, !is.na(sex) & !is.na(maladaptive_score)),
    mapping = aes(x = maladaptive_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w10_feel, !is.na(maladaptive_score)),
    mapping = aes(x = maladaptive_score, linetype = "Overall"),
    color = "black", show.legend = TRUE
  ) +
  # replace "1"/"2" with labels
  scale_fill_discrete(labels = function(x) code_to_label[x]) +
  # map the linetype name -> dashed and give the legend a clear name
  scale_linetype_manual(name = "Distribution",
                        values = c("Overall" = "dashed")) +
  # hide the duplicate color legend (we keep the fill legend for 'Sex')
  scale_color_discrete(guide = "none") +
  labs(
    title = "Distribution of Average General Maladaptive Scores (FEEL W10)",
    x = "Average General Maladaptive Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W10/FEEL10_maladaptive_sex_plot.svg",
       plot = maladaptive_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")