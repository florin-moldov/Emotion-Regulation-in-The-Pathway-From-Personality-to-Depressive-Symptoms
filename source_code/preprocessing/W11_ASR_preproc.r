# W11 ASR preprocessing ----
#
# This script preprocesses the ASR (Depression and Anxiety scales) data
# for Wave 11.
# It reads the raw data and prepares the dataset for analysis.

# Load necessary libraries ----
library(haven)  # For reading SPSS files
library(jsonlite)  # For saving metadata as JSON
library(tidyverse)  # For data manipulation and visualization
library(psych)  # For psychometric properties like Cronbach's alpha

# Read the raw ASR data for Wave 11 ----
raw_w11_asr <- read_sav("data/raw/W11_ASR+demo.sav")

# Compute ASR Depression and Anxiety scores ----
# The ASR Depression and Anxiety scales are computed by averaging the
# relevant items. The specific items for each scale are determined
# based on the ASR manual.
preproc_w11_asr <- raw_w11_asr |>
  rename(ID = PPcode) |>
  select(ID, ASR_14, ASR_18, ASR_35, ASR_24, ASR_52, ASR_54, ASR_60, ASR_77,
         ASR_78, ASR_91, ASR_102, ASR_100, ASR_103, ASR_107, ASR_22, ASR_29,
         ASR_45, ASR_50, ASR_56h, ASR_72, ASR_112) |>
  mutate(
    depression_score = rowMeans(across(c(ASR_14, ASR_18, ASR_35, ASR_24,
                                         ASR_52, ASR_54, ASR_60, ASR_77, ASR_78,
                                         ASR_91, ASR_102, ASR_100,
                                         ASR_103, ASR_107)), na.rm = FALSE),
    anxiety_score = rowMeans(across(c(ASR_22, ASR_29, ASR_45, ASR_50,
                                      ASR_56h, ASR_72, ASR_112)), na.rm = FALSE)
  )

# Display the structure of the preprocessed dataset ----
str(preproc_w11_asr) # should be 24 columns
# (ID + 21 ASR items + 2 computed scores) and 368 rows (actually 365 pps)

# Simplify metadata before opening viewer (because
# otherwise viewer might crash due to too much metadata)
safe_w11_asr <- as.data.frame(preproc_w11_asr)
safe_w11_asr[] <- lapply(safe_w11_asr, haven::zap_labels)

# Save the metadata (variable labels and value labels) to a JSON file
meta <- lapply(names(preproc_w11_asr), function(nm) {
  x <- preproc_w11_asr[[nm]]
  list(
    variable = nm,
    variable_label = attr(x, "label"),
    value_labels = as.list(attr(x, "labels"))
  )
})
write_json(meta, "data/preprocessed/preproc_w11_asr_metadata.json",
           pretty = TRUE, auto_unbox = TRUE)

# Save the preprocessed dataset for future use ----
write.csv(preproc_w11_asr, file = "data/preprocessed/preproc_w11_asr.csv",
          quote = FALSE,
          row.names = FALSE)

# Descriptive statistics for the ASR Depression and Anxiety scores ----
# Ofc, only for non-missing values, so we set na.rm = TRUE
# vector of dimension column names (as created in preproc)
dims <- c("depression_score", "anxiety_score")
hipic_stats <- preproc_w11_asr |>
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

print(hipic_stats)

# Save the tidy table
write_csv(hipic_stats, "data/descriptives/asr_stats_w11.csv")

# Add demographics to the preprocessed HiPIC dataset ----
preproc_w11_demo <- read_csv("data/preprocessed/preproc_w11_demographics.csv")
preproc_w11_asr <- preproc_w11_asr |>
  left_join(preproc_w11_demo, by = "ID")

# Save the updated dataset with demographics for future use
write.csv(preproc_w11_asr, file = "data/preprocessed/preproc_w11_asr.csv",
          quote = FALSE,
          row.names = FALSE)

# Determine number of NaNs in each column (missing values) ----
num_nans <- sapply(preproc_w11_asr, function(x) {
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
cat("Total number of NaNs:", sum(num_nans), "\n") # 954 missing values in total
# (so across all columns and all participants) for ASR W11 data
# with 3 for ID, 23 for depression_score, 23 for anxiety_score,
# 13 for highest_edu, 368 for highest_edu_text,
# 12 for employment_status, 29 for sex, and 483 for the individual item columns

# Psychometric properties ----
depression_items <- c("ASR_14", "ASR_18", "ASR_35", "ASR_24",
                      "ASR_52", "ASR_54", "ASR_60", "ASR_77",
                      "ASR_78", "ASR_91", "ASR_102", "ASR_100",
                      "ASR_103", "ASR_107")
print(
  preproc_w11_asr |>
    select(all_of(depression_items)) |>
    alpha(n.iter = 10000,
          title = "ASR Depression Scale")
)

anxiety_items <- c("ASR_22", "ASR_29", "ASR_45", "ASR_50",
                   "ASR_56h", "ASR_72", "ASR_112")
print(
  preproc_w11_asr |>
    select(all_of(anxiety_items)) |>
    alpha(n.iter = 10000,
          title = "ASR Anxiety Scale")
)

# Distribution of ASR Depression and Anxiety scores ----
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
depression_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w11_asr, !is.na(sex) & !is.na(depression_score)),
    mapping = aes(x = depression_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w11_asr, !is.na(depression_score)),
    mapping = aes(x = depression_score, linetype = "Overall"),
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
    title = "Distribution of Average Depression Scores (ASR W11)",
    x = "Average Depression Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W11/ASR11_depression_sex_plot.svg",
       plot = depression_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

anxiety_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w11_asr, !is.na(sex) & !is.na(anxiety_score)),
    mapping = aes(x = anxiety_score,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w11_asr, !is.na(anxiety_score)),
    mapping = aes(x = anxiety_score, linetype = "Overall"),
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
    title = "Distribution of Average Anxiety Scores (ASR W11)",
    x = "Average Anxiety Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W11/ASR11_anxiety_sex_plot.svg",
       plot = anxiety_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")