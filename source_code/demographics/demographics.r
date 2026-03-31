# Demographics analysis
#
# This script displays and analyses the demographics of all waves.
# IMPORTANT TO NOTE: AS OF YET, W10 SEX INFO IS USED TO FILL IN MISSING
# GENDER INFO IN W6 AND W11. THIS IS BECAUSE W10 HAS THE MOST COMPLETE
# GENDER INFO (for now, this will change after getting demographics from
# W1, where everyone is in there).

# Load necessary libraries ----
library(haven)  # For reading SPSS files
library(jsonlite) # For writing JSON files
library(tidyverse) # For data manipulation and visualization

# W10 demographics preprocessing ----
# First, need to extract the relevant columns
# from the W10_FEEL+demo.sav file (based on the questionnaire file)
raw_w10_all_data <- read_sav("data/raw/W10_FEEL+demo.sav")
print(colnames(raw_w10_all_data)) # To find the relevant columns

# Extract the relevant columns for demographics
w10_demographics <- raw_w10_all_data |>
  select(ID, geslacht, IEDU1, IEDU4, IEDU7, IEDU7_12_TEXT, IWORK3, Sex1_BioSex)

# Now sanitize the metadata for the demographics data just like we did for W6
safe_demographics_w10 <- as.data.frame(w10_demographics)
safe_demographics_w10[] <- lapply(safe_demographics_w10, haven::zap_labels)

# Convert the relevant columns to factors and rename
safe_demographics_w10 <- safe_demographics_w10 |>
  mutate(
    ID = trimws(as.character(ID)),
    geslacht = factor(geslacht),
    IEDU1 = factor(IEDU1),
    IEDU4 = factor(IEDU4),
    IEDU7 = factor(IEDU7),
    IEDU7_12_TEXT = factor(IEDU7_12_TEXT),
    IWORK3 = factor(IWORK3),
    Sex1_BioSex = factor(Sex1_BioSex)
  ) |>
  rename(
    sex = geslacht,
    fulltime_edu = IEDU1,
    parttime_edu = IEDU4,
    highest_edu = IEDU7,
    highest_edu_text = IEDU7_12_TEXT,
    employment_status = IWORK3,
    biological_sex = Sex1_BioSex
  )

# Save the metadata (variable labels and value labels) to a JSON file
meta <- lapply(names(w10_demographics), function(nm) {
  x <- w10_demographics[[nm]]
  list(
    variable = nm,
    variable_label = attr(x, "label"),
    value_labels = as.list(attr(x, "labels"))
  )
})
write_json(meta, "data/preprocessed/preproc_w10_demographics_metadata.json",
           pretty = TRUE, auto_unbox = TRUE)

# Check for missing values in the safe dataset
# Determine number of NaNs in each column (missing values) ----
num_nans <- sapply(safe_demographics_w10, function(x) {
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
cat("Total number of NaNs:", sum(num_nans), "\n") # 44 missing values in total
# (10 for parttime_edu, 1 for highest_edu, 1 for employment_status,
# 32 for biological_sex)

# Save the safe dataset for future use
write.csv(safe_demographics_w10,
            file = "data/preprocessed/preproc_w10_demographics.csv",
            quote = FALSE,
            row.names = FALSE)

# W6 demographics preprocessing ----
# Read descriptives for W6
demographics_w6 <- read_sav("data/raw/W6_Gezinsgegevens.sav")

# Simplify metadata before opening viewer (because
# otherwise viewer might crash due to too much metadata)
safe_demographics_w6 <- as.data.frame(demographics_w6)
safe_demographics_w6[] <- lapply(safe_demographics_w6, haven::zap_labels)

# Display the structure of the safe dataset
str(safe_demographics_w6)

# Select the relevant columns, convert to factors, and rename
safe_demographics_w6 <- safe_demographics_w6 |>
  select(ID, W6_schoolniveau) |>
  mutate(
    ID = trimws(as.character(ID)),
    W6_schoolniveau = factor(W6_schoolniveau)
  ) |>
  rename(
    highest_edu = W6_schoolniveau
  )

# Add gender information from W10 to W6 demographics dataset by merging on ID
safe_demographics_w6 <- safe_demographics_w6 |>
  left_join(safe_demographics_w10 |> select(ID, sex), by = "ID")

# Save the metadata (variable labels and value labels) to a JSON file
meta <- lapply(names(demographics_w6), function(nm) {
  x <- demographics_w6[[nm]]
  list(
    variable = nm,
    variable_label = attr(x, "label"),
    value_labels = as.list(attr(x, "labels"))
  )
})
write_json(meta, "data/preprocessed/preproc_w6_demographics_metadata.json",
           pretty = TRUE, auto_unbox = TRUE)

# Check for missing values in the safe dataset
# Determine number of NaNs in each column (missing values) ----
num_nans <- sapply(safe_demographics_w6, function(x) {
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
cat("Total number of NaNs:", sum(num_nans), "\n") # 113 missing values in total
# (5 for highest_degree, 108 for sex)

# Save the safe dataset for future use
write.csv(safe_demographics_w6,
            file = "data/preprocessed/preproc_w6_demographics.csv",
            quote = FALSE, 
            row.names = FALSE)

# W11 demographics preprocessing ----
# First, need to extract the relevant columns
# from the W11_ASR+demo.sav file (based on the questionnaire file)
raw_w11_all_data <- read_sav("data/raw/W11_ASR+demo.sav")
print(colnames(raw_w11_all_data)) # To find the relevant columns

# Extract the relevant columns for demographics
w11_demographics <- raw_w11_all_data |>
  select(PPcode, HighestDegree, HighestDegree_12_TEXT,
         JobSituation)

# Now sanitize the metadata for the demographics data just like we did for W6
safe_demographics_w11 <- as.data.frame(w11_demographics)
safe_demographics_w11[] <- lapply(safe_demographics_w11, haven::zap_labels)

# Display all the columns of the safe W11 demographics dataset
print(colnames(safe_demographics_w11))

# Convert the relevant columns to factors and rename
safe_demographics_w11 <- safe_demographics_w11 |>
  mutate(
    PPcode = trimws(as.character(PPcode)),
    HighestDegree = factor(HighestDegree),
    HighestDegree_12_TEXT = factor(HighestDegree_12_TEXT),
    JobSituation = factor(JobSituation)
  ) |>
  rename(
    ID = PPcode,
    highest_edu = HighestDegree,
    highest_edu_text = HighestDegree_12_TEXT,
    employment_status = JobSituation
  )

# Add gender information from W10 to W11 demographics dataset by merging on ID
safe_demographics_w11 <- safe_demographics_w11 |>
  left_join(safe_demographics_w10 |> select(ID, sex), by = "ID")

# Save the metadata (variable labels and value labels) to a JSON file
meta <- lapply(names(w11_demographics), function(nm) {
  x <- w11_demographics[[nm]]
  list(
    variable = nm,
    variable_label = attr(x, "label"),
    value_labels = as.list(attr(x, "labels"))
  )
})
write_json(meta, "data/preprocessed/preproc_w11_demographics_metadata.json",
           pretty = TRUE, auto_unbox = TRUE)

# Check for missing values in the safe dataset
# Determine number of NaNs in each column (missing values) ----
num_nans <- sapply(safe_demographics_w11, function(x) {
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
cat("Total number of NaNs:", sum(num_nans), "\n") # 54 missing values in total
# (13 for highest_edu, 12 for employment_status, 29 for sex)

# Save the safe dataset for future use
write.csv(safe_demographics_w11,
            file = "data/preprocessed/preproc_w11_demographics.csv",
            quote = FALSE, 
            row.names = FALSE)

# Overlap of participants across waves ----
# To check the overlap of participants across waves,
# we can look at the IDs in each wave (non-empty, non-missing)
valid_ids_w6 <- safe_demographics_w6$ID[!is.na(safe_demographics_w6$ID) &
                                          safe_demographics_w6$ID != ""]
valid_ids_w10 <- safe_demographics_w10$ID[!is.na(safe_demographics_w10$ID) &
                                            safe_demographics_w10$ID != ""]
valid_ids_w11 <- safe_demographics_w11$ID[!is.na(safe_demographics_w11$ID) &
                                            safe_demographics_w11$ID != ""]

# Sample sizes for each wave
cat("Sample size in W6:", length(valid_ids_w6), "\n") # 451
cat("Sample size in W10:", length(valid_ids_w10), "\n") # 369
cat("Sample size in W11:", length(valid_ids_w11), "\n") # 365

# Find the number of IDs that are present in all three waves
ids_all_waves <- Reduce(intersect, list(valid_ids_w6, valid_ids_w10,
                                        valid_ids_w11))
num_ids_all_waves <- length(ids_all_waves)
cat("Number of IDs present in all three waves:", num_ids_all_waves, "\n")
percent_attrition <- (1 - num_ids_all_waves / length(valid_ids_w6)) * 100
cat("Percentage of attrition from W6 to W11:", percent_attrition, "%\n")

# Descriptive plots and table ----
# W6 ----
# Highest education levels ----
# Read metadata JSON
meta <- fromJSON("data/preprocessed/preproc_w6_demographics_metadata.json",
                 simplifyVector = FALSE)

# Get value labels for the original W6 education variable
edu_meta <- meta[[which(vapply(meta,
                               function(x) x$variable == "W6_schoolniveau",
                               logical(1)))]]

# JSON stores value_labels as label -> code, so invert to code -> label
# e.g., "Primary school" : 1  becomes  "1" -> "Primary school"
code_to_label <- setNames(
  names(edu_meta$value_labels),
  as.character(unlist(edu_meta$value_labels))
)

# Plot with x-axis relabeled from metadata
w6_highest_edu_plot <- safe_demographics_w6 |>
  filter(!is.na(highest_edu)) |>
  ggplot(aes(x = highest_edu, fill = highest_edu)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = code_to_label) +
  labs(
    title = "Distribution of Highest Education Levels in W6",
    x = "Highest Education Level",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w6_highest_edu_plot.svg",
       plot = w6_highest_edu_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# Sex ----
# Note: For W6, we are using the sex information from W10, so we can
# use the same metadata for relabeling as for W10
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

# Plot with x-axis relabeled from metadata
w6_sex_plot <- safe_demographics_w6 |>
  filter(!is.na(sex)) |>
  ggplot(aes(x = sex, fill = sex)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = code_to_label) +
  labs(
    title = "Distribution of Sex in W6",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w6_sex_plot.svg",
       plot = w6_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# W10 ----
# Full-time education yes or no ----
# Define mapping of original codes to labels for fulltime_edu
# Note: This mapping is based on the questionnaire only, since
# no value labels were present in the original SPSS file
# (so also not in the metadata json file) for this variable
fulltime_edu_labels <- c(
  "1" = "Yes",
  "2" = "No"
)

w10_fulltime_plot <- safe_demographics_w10 |>
  filter(!is.na(fulltime_edu)) |>
  ggplot(aes(x = fulltime_edu, fill = fulltime_edu)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = fulltime_edu_labels) +
  labs(
    title = "Distribution of Full-time Education in W10",
    x = "Full-time Education",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w10_fulltime_plot.svg",
       plot = w10_fulltime_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# Part-time education yes or no ----
# Define mapping of original codes to labels for parttime_edu
# Note: This mapping is based on the questionnaire only, since
# no value labels were present in the original SPSS file
# (so also not in the metadata json file) for this variable
parttime_edu_labels <- c(
  "1" = "Yes",
  "2" = "No"
)

w10_parttime_plot <- safe_demographics_w10 |>
  filter(!is.na(parttime_edu)) |>
  ggplot(aes(x = parttime_edu, fill = parttime_edu)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = parttime_edu_labels) +
  labs(
    title = "Distribution of Part-time Education in W10",
    x = "Part-time Education",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w10_parttime_plot.svg",
       plot = w10_parttime_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# Highest education levels ----
# Note: For W10, we are using the highest education information
# from the IEDU7 variable, so we can use the same metadata for
# relabeling as for W11, since IEDU7 in W10 corresponds to HighestDegree in W11
meta <- fromJSON("data/preprocessed/preproc_w11_demographics_metadata.json",
                 simplifyVector = FALSE)

# Get value labels for the original W11 education variable
edu_meta <- meta[[which(vapply(meta,
                               function(x) x$variable == "HighestDegree",
                               logical(1)))]]

# JSON stores value_labels as label -> code, so invert to code -> label
code_to_label <- setNames(
  names(edu_meta$value_labels),
  as.character(unlist(edu_meta$value_labels))
)
w10_highest_edu_plot <- safe_demographics_w10 |>
  filter(!is.na(highest_edu)) |>
  ggplot(aes(x = highest_edu, fill = highest_edu)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(code_to_label[x], width = 30)) +
  labs(
    title = "Distribution of Highest Education Levels in W10",
    x = "Highest Education Level",
    y = "Count"
  ) +
  theme_minimal()

# For option 12, all text inputs are NAs,
# so we can just ignore that option for now
print(sum(is.na(safe_demographics_w10$highest_edu_text))) # 369 NAs,
# so all values are NA

ggsave("reports/plots/demographics/w10_highest_edu_plot.svg",
       plot = w10_highest_edu_plot,
       device = "svg",
       width = 20,
       height = 10,
       units = "in")

# Employment status ----
# Note: For W10, we are using the employment status information from
# the IWORK3 variable, so we can use the same metadata for relabeling
# as for W11, since IWORK3 in W10 corresponds to JobSituation in W11
# Read metadata JSON
meta <- fromJSON("data/preprocessed/preproc_w11_demographics_metadata.json",
                 simplifyVector = FALSE)

# Get value labels for the original W11 employment variable
employment_meta <- meta[[which(vapply(meta,
                                      function(x) x$variable == "JobSituation",
                                      logical(1)))]]

# JSON stores value_labels as label -> code, so invert to code -> label
code_to_label <- setNames(
  names(employment_meta$value_labels),
  as.character(unlist(employment_meta$value_labels))
)

w10_employment_plot <- safe_demographics_w10 |>
  filter(!is.na(employment_status)) |>
  ggplot(aes(x = employment_status, fill = employment_status)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(code_to_label[x], width = 20)) +
  labs(
    title = "Distribution of Employment Status in W10",
    x = "Employment Status",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w10_employment_plot.svg",
       plot = w10_employment_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# Sex ----
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

# Plot with x-axis relabeled from metadata
w10_sex_plot <- safe_demographics_w10 |>
  filter(!is.na(sex)) |>
  ggplot(aes(x = sex, fill = sex)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = code_to_label) +
  labs(
    title = "Distribution of Sex in W10",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w10_sex_plot.svg",
       plot = w10_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# W11 ----
# Highest education levels ----
meta <- fromJSON("data/preprocessed/preproc_w11_demographics_metadata.json",
                 simplifyVector = FALSE)

# Get value labels for the original W11 education variable
edu_meta <- meta[[which(vapply(meta,
                               function(x) x$variable == "HighestDegree",
                               logical(1)))]]

# JSON stores value_labels as label -> code, so invert to code -> label
code_to_label <- setNames(
  names(edu_meta$value_labels),
  as.character(unlist(edu_meta$value_labels))
)
w11_highest_edu_plot <- safe_demographics_w11 |>
  filter(!is.na(highest_edu)) |>
  ggplot(aes(x = highest_edu, fill = highest_edu)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(code_to_label[x], width = 30)) +
  labs(
    title = "Distribution of Highest Education Levels in W11",
    x = "Highest Education Level",
    y = "Count"
  ) +
  theme_minimal()

# For option 12, all cells are empty strings,
# so we can just ignore that option for now
print(sum(safe_demographics_w11$highest_edu_text == "")) # 365 empty strings,
# so all values are "" (365 if not counting the empty IDs, which are 3)

ggsave("reports/plots/demographics/w11_highest_edu_plot.svg",
       plot = w11_highest_edu_plot,
       device = "svg",
       width = 20,
       height = 10,
       units = "in")

# Employment status ----
# Read metadata JSON
meta <- fromJSON("data/preprocessed/preproc_w11_demographics_metadata.json",
                 simplifyVector = FALSE)

# Get value labels for the original W11 employment variable
employment_meta <- meta[[which(vapply(meta,
                               function(x) x$variable == "JobSituation",
                               logical(1)))]]

# JSON stores value_labels as label -> code, so invert to code -> label
code_to_label <- setNames(
  names(employment_meta$value_labels),
  as.character(unlist(employment_meta$value_labels))
)

w11_employment_plot <- safe_demographics_w11 |>
  filter(!is.na(employment_status)) |>
  ggplot(aes(x = employment_status, fill = employment_status)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(code_to_label[x], width = 20)) +
  labs(
    title = "Distribution of Employment Status in W11",
    x = "Employment Status",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w11_employment_plot.svg",
       plot = w11_employment_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# Sex ----
# Note: For W11, we are using the sex information from W10, so we
# can use the same metadata for relabeling as for W10, since the sex
# variable in W11 corresponds to the sex variable in W10
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

# Plot with x-axis relabeled from metadata
w11_sex_plot <- safe_demographics_w11 |>
  filter(!is.na(sex)) |>
  ggplot(aes(x = sex, fill = sex)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(labels = code_to_label) +
  labs(
    title = "Distribution of Sex in W11",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal()
ggsave("reports/plots/demographics/w11_sex_plot.svg",
       plot = w11_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

# Table with all occurences of each demographic variable across all waves ----
# Create a summary table for each demographic variable across all waves

# Variables to include
demo_vars <- c("sex", "fulltime_edu", "parttime_edu",
               "highest_edu", "employment_status")
waves <- c("W6", "W10", "W11")

# Helper: ensure all variables exist in each wave dataset
add_missing_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  df
}

# Reference levels from W10 (display once / stable order)
ref_levels <- lapply(demo_vars, function(v) {
  vals <- safe_demographics_w10[[v]]
  levels(droplevels(factor(as.character(vals))))
})
names(ref_levels) <- demo_vars

# Fix ordering for highest_edu: 1,2,3,... instead of 1,10,11,...
edu_vals <- as.character(safe_demographics_w10$highest_edu)
edu_vals <- edu_vals[!is.na(edu_vals) & trimws(edu_vals) != ""]

# numeric-aware ordering
ref_levels[["highest_edu"]] <- edu_vals |>
  unique() |>
  (\(x) x[order(as.numeric(x))])()

# Build a reference table: variable-level order (+ Missing)
ref_tbl <- bind_rows(lapply(demo_vars, function(v) {
  tibble(
    variable = v,
    level = c(ref_levels[[v]], "Missing"),
    level_order = seq_along(c(ref_levels[[v]], "Missing"))
  )
}))

# Stack all waves
all_demo <- bind_rows(
  add_missing_cols(safe_demographics_w6, demo_vars)  |> mutate(wave = "W6"),
  add_missing_cols(safe_demographics_w10, demo_vars) |> mutate(wave = "W10"),
  add_missing_cols(safe_demographics_w11, demo_vars) |> mutate(wave = "W11")
) |>
  select(wave, all_of(demo_vars)) |>
  pivot_longer(cols = all_of(demo_vars), names_to = "variable", values_to = "level") |>
  mutate(
    level = as.character(level),
    level = if_else(is.na(level) | trimws(level) == "", "Missing", level),
    # Keep only W10 reference levels per variable; everything else -> Missing
    level = if_else(
      mapply(function(lv, var) lv %in% ref_levels[[var]], level, variable),
      level, "Missing"
    )
  )

# Percentages per wave and variable:
# - non-missing levels: denominator = non-missing N
# - Missing row: denominator = total N
demo_pct_table <- all_demo |>
  count(wave, variable, level, name = "n") |>
  group_by(wave, variable) |>
  mutate(
    n_total = sum(n),
    n_missing = sum(n[level == "Missing"]),
    n_nonmissing = n_total - n_missing,
    percentage = case_when(
      # % missing (of total)
      level == "Missing" ~ 100 * n / n_total,
      # % among non-missing
      n_nonmissing > 0    ~ 100 * n / n_nonmissing,
      TRUE                ~ NA_real_
    )
  ) |>
  ungroup() |>
  select(wave, variable, level, percentage) |>
  # ensure every variable-level exists in every wave
  right_join(
    tidyr::crossing(wave = waves, ref_tbl),
    by = c("wave", "variable", "level")
  ) |>
  mutate(percentage = replace_na(percentage, 0)) |>
  pivot_wider(names_from = wave, values_from = percentage) |>
  arrange(variable, level_order) |>
  select(variable, level, W6, W10, W11)

# Rounded display
demo_pct_table_fmt <- demo_pct_table |>
  mutate(across(c(W6, W10, W11), ~ round(.x, 2)))

print(demo_pct_table_fmt)

# Save the table as CSV for future use
write_csv(demo_pct_table_fmt,
          "data/descriptives/demographics_summary_table.csv")