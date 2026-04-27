# W6 CBCL preprocessing ----
#
# This script preprocesses the CBCL data for Wave 6.
# It reads the raw data and prepares the dataset for analysis.

# Set seed for reproducibility ----
set.seed(123)

# Set sink to capture all output in a text file ----
sink("data/preprocessed/cbcl_preproc_output.txt")

# Load necessary libraries ----
library(haven)  # For reading SPSS files
library(jsonlite)  # For saving metadata as JSON
library(tidyverse)  # For data manipulation and visualization
library(psych)  # For psychometric properties like Cronbach's alpha

# Read the raw CBCL data for Wave 6 ----
raw_w6_cbcl <- read_sav("data/raw/W6_CBCL_A.sav")

# Select, save relevant columns, and compute the average scores ----
# (for internalizing problems, self-report by the kids)
# See 'Schalenboek W6_2009 1.pdf' for the list of relevant columns
preproc_w6_cbcl <- raw_w6_cbcl |>
  select(ID, CBCL6A5, CBCL6A12, CBCL6A14, CBCL6A18,
         CBCL6A29, CBCL6A30, CBCL6A31, CBCL6A32,
         CBCL6A33, CBCL6A34, CBCL6A35, CBCL6A42,
         CBCL6A45, CBCL6A47, CBCL6A50, CBCL6A51,
         CBCL6A52, CBCL6A54, CBCL6A56a, CBCL6A56b,
         CBCL6A56c, CBCL6A56d, CBCL6A56e, CBCL6A56f,
         CBCL6A56g, CBCL6A65, CBCL6A69, CBCL6A71,
         CBCL6A75, CBCL6A89, CBCL6A91, CBCL6A102,
         CBCL6A103, CBCL6A111, CBCL6A112) |>
  mutate(
    internalizing = rowMeans(across(-ID), na.rm = FALSE)
  )

# Display the structure of the preprocessed dataset ----
str(preproc_w6_cbcl) # should be 37 columns
# (ID + 35 CBCLA items + 1 average score) and 449 rows (participants)

# Simplify metadata before opening viewer (because
# otherwise viewer might crash due to too much metadata)
safe_w6_cbcl <- as.data.frame(preproc_w6_cbcl)
safe_w6_cbcl[] <- lapply(safe_w6_cbcl, haven::zap_labels)

# Save the metadata (variable labels and value labels) to a JSON file
meta <- lapply(names(preproc_w6_cbcl), function(nm) {
  x <- preproc_w6_cbcl[[nm]]
  list(
    variable = nm,
    variable_label = attr(x, "label"),
    value_labels = as.list(attr(x, "labels"))
  )
})
write_json(meta, "data/preprocessed/preproc_w6_cbcl_metadata.json",
           pretty = TRUE, auto_unbox = TRUE)

# Save the preprocessed dataset for future use ----
write.csv(preproc_w6_cbcl, file = "data/preprocessed/preproc_w6_cbcl.csv",
          quote = FALSE,
          row.names = FALSE)

# Descriptive statistics for the average internalizing scores ----
# Ofc, only for non-missing values, so we set na.rm = TRUE
# vector of dimension column names (as created in preproc)
dims <- c("internalizing")

cbcl_stats <- preproc_w6_cbcl |>
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

print(cbcl_stats)

# Save the tidy table
write_csv(cbcl_stats, "data/descriptives/cbcl_stats_w6.csv")

# Add demographics to the preprocessed CBCL dataset ----
preproc_w6_demo <- read_csv("data/preprocessed/preproc_w6_demographics.csv")
preproc_w6_cbcl <- preproc_w6_cbcl |>
  left_join(preproc_w6_demo, by = "ID")
# Save the updated dataset with demographics for future use
write.csv(preproc_w6_cbcl, file = "data/preprocessed/preproc_w6_cbcl.csv",
          quote = FALSE,
          row.names = FALSE)

# Determine number of NaNs in each column (missing values) ----
num_nans <- sapply(preproc_w6_cbcl, function(x) {
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
cat("Total number of NaNs:", sum(num_nans), "\n") # 189 missing values in total
# (so across all columns and all participants) for CBCL W6 data
# with 25 for the average score column and 51 for the individual item columns
# (so 26 missing values in the individual items, and 25 missing values in the
# average score column, which is expected since the average score is NA if any
# of the individual items are NA)
# and 6 for highest_edu and 107 for sex

# Psychometric properties ----
print(
  preproc_w6_cbcl |>
    select(starts_with("CBCL6A")) |> # only the individual items, not the average score
    alpha(n.iter = 10000) # bootstrap confidence intervals with 10000 iterations
)

# End sink to stop capturing output in the text file ----
sink(file = NULL)

# Distribution of the average internalizing scores ----
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
cbcl6a_internalizing_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w6_cbcl, !is.na(sex) & !is.na(internalizing)),
    mapping = aes(x = internalizing,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w6_cbcl, !is.na(internalizing)),
    mapping = aes(x = internalizing, linetype = "Overall"),
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
    title = "Distribution of Average Internalizing Scores (CBCL W6)",
    x = "Average Internalizing Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W6/CBCL6A_internalizing_sex_plot.svg",
       plot = cbcl6a_internalizing_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")
