# W6 HiPIC preprocessing ----
#
# This script preprocesses the HiPIC data for Wave 6.
# It reads the raw data and prepares the dataset for analysis.

# Set seed for reproducibility ----
set.seed(123)

# Set sink to capture all output in a text file ----
sink("data/preprocessed/hipic_preproc_output.txt")

# Load necessary libraries ----
library(haven)  # For reading SPSS files
library(jsonlite)  # For saving metadata as JSON
library(tidyverse)  # For data manipulation and visualization
library(psych)  # For psychometric properties like Cronbach's alpha

# Read the raw HiPIC data for Wave 6 ----
raw_w6_hipic <- read_sav("data/raw/W6_HiPIC_A.sav")

# Reverse and compute the average scores for the HiPIC dimensions ----
# See 'Schalenboek W6_2009 1.pdf' for the list of relevant columns
# and which ones to reverse
preproc_w6_hipic <- raw_w6_hipic |>
  mutate(
    # Reverse items if needed
    across(c(HiP6A1, HiP6A5, HiP6A8, HiP6A16, HiP6A17,
             HiP6A20, HiP6A27, HiP6A35, HiP6A37, HiP6A41,
             HiP6A42, HiP6A44, HiP6A49, HiP6A50, HiP6A53,
             HiP6A55, HiP6A60, HiP6A62, HiP6A63, HiP6A79,
             HiP6A80, HiP6A84, HiP6A87, HiP6A89, HiP6A99,
             HiP6A103, HiP6A107, HiP6A110, HiP6A120, HiP6A121,
             HiP6A122, HiP6A124, HiP6A129, HiP6A138, HiP6A139,
             HiP6A143), ~ 6 - as.numeric(.x)),
    # Compute the average scores for each dimension
    extraversion = rowMeans(across(c(HiP6A1, HiP6A19, HiP6A37,
                                     HiP6A55, HiP6A73, HiP6A91,
                                     HiP6A109, HiP6A127, HiP6A6,
                                     HiP6A24, HiP6A42, HiP6A60,
                                     HiP6A78, HiP6A96, HiP6A114,
                                     HiP6A132, HiP6A11, HiP6A29,
                                     HiP6A47, HiP6A65, HiP6A83,
                                     HiP6A101, HiP6A119, HiP6A137,
                                     HiP6A15, HiP6A33, HiP6A51,
                                     HiP6A69, HiP6A87, HiP6A105,
                                     HiP6A123, HiP6A141)), na.rm = FALSE),
    agreeableness = rowMeans(across(c(HiP6A2, HiP6A20, HiP6A38,
                                    HiP6A56, HiP6A74, HiP6A92,
                                    HiP6A110, HiP6A128, HiP6A7,
                                    HiP6A25, HiP6A43, HiP6A61,
                                    HiP6A79, HiP6A97, HiP6A115,
                                    HiP6A133, HiP6A12, HiP6A30,
                                    HiP6A48, HiP6A66, HiP6A84,
                                    HiP6A102, HiP6A120, HiP6A138,
                                    HiP6A16, HiP6A34, HiP6A52,
                                    HiP6A70, HiP6A88, HiP6A106,
                                    HiP6A124, HiP6A142, HiP6A18,
                                    HiP6A36, HiP6A54, HiP6A72,
                                    HiP6A90, HiP6A108, HiP6A126,
                                    HiP6A144)), na.rm = FALSE),
    conscientiousness = rowMeans(across(c(HiP6A3, HiP6A21, HiP6A39,
                                          HiP6A57, HiP6A75, HiP6A93,
                                          HiP6A111, HiP6A129, HiP6A8,
                                          HiP6A26, HiP6A44, HiP6A62,
                                          HiP6A80, HiP6A98, HiP6A116,
                                          HiP6A134, HiP6A13, HiP6A31,
                                          HiP6A49, HiP6A67, HiP6A85,
                                          HiP6A103, HiP6A121, HiP6A139,
                                          HiP6A17, HiP6A35, HiP6A53,
                                          HiP6A71, HiP6A89, HiP6A107,
                                          HiP6A125, HiP6A143)), na.rm = FALSE),
    emotional_stability = rowMeans(across(c(HiP6A4, HiP6A22, HiP6A40,
                                            HiP6A58, HiP6A76, HiP6A94,
                                            HiP6A112, HiP6A130, HiP6A9,
                                            HiP6A27, HiP6A45, HiP6A63,
                                            HiP6A81, HiP6A99, HiP6A117,
                                            HiP6A135)), na.rm = FALSE),
    openness = rowMeans(across(c(HiP6A5, HiP6A23, HiP6A41,
                                    HiP6A59, HiP6A77, HiP6A95,
                                    HiP6A113, HiP6A131, HiP6A10,
                                    HiP6A28, HiP6A46, HiP6A64,
                                    HiP6A82, HiP6A100, HiP6A118,
                                    HiP6A136, HiP6A14, HiP6A32,
                                    HiP6A50, HiP6A68, HiP6A86,
                                    HiP6A104, HiP6A122, HiP6A140)),
                           na.rm = FALSE)
  ) |>
  mutate(neuroticism = 6 - emotional_stability) # create neuroticism

# Display the structure of the preprocessed dataset ----
str(preproc_w6_hipic) # should be 150 columns
# (ID + 144 HiPIC items + 5 HiPIC dimensions) and 449 rows (participants)

# Simplify metadata before opening viewer (because
# otherwise viewer might crash due to too much metadata)
safe_w6_hipic <- as.data.frame(preproc_w6_hipic)
safe_w6_hipic[] <- lapply(safe_w6_hipic, haven::zap_labels)

# Save the metadata (variable labels and value labels) to a JSON file
meta <- lapply(names(preproc_w6_hipic), function(nm) {
  x <- preproc_w6_hipic[[nm]]
  list(
    variable = nm,
    variable_label = attr(x, "label"),
    value_labels = as.list(attr(x, "labels"))
  )
})
write_json(meta, "data/preprocessed/preproc_w6_hipic_metadata.json",
           pretty = TRUE, auto_unbox = TRUE)

# Save the preprocessed dataset for future use ----
write.csv(preproc_w6_hipic, file = "data/preprocessed/preproc_w6_hipic.csv",
          quote = FALSE,
          row.names = FALSE)

# Descriptive statistics for the HiPIC dimensions ----
# Ofc, only for non-missing values, so we set na.rm = TRUE
# vector of dimension column names (as created in preproc)
dims <- c("extraversion", "agreeableness", "conscientiousness",
          "neuroticism", "openness")

hipic_stats <- preproc_w6_hipic |>
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
write_csv(hipic_stats, "data/descriptives/hipic_stats_w6.csv")

# Add demographics to the preprocessed HiPIC dataset ----
preproc_w6_demo <- read_csv("data/preprocessed/preproc_w6_demographics.csv")
preproc_w6_hipic <- preproc_w6_hipic |>
  left_join(preproc_w6_demo, by = "ID")

# Save the updated dataset with demographics for future use
write.csv(preproc_w6_hipic, file = "data/preprocessed/preproc_w6_hipic.csv",
          quote = FALSE,
          row.names = FALSE)

# Determine number of NaNs in each column (missing values) ----
num_nans <- sapply(preproc_w6_hipic, function(x) {
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
cat("Total number of NaNs:", sum(num_nans), "\n") # 459 missing values in total
# (so across all columns and all participants) for HiPIC W6 data
# with 31 for extraversion, 52 for agreeableness, 27 for conscientiousness,
# 21 for neuroticism and 20 for openness, 5 for highest_edu,
# 107 for sex, and 175 for the individual item columns

# Psychometric properties ----
# Extraversion ----
extraversion_items <- c("HiP6A1", "HiP6A19", "HiP6A37",
                        "HiP6A55", "HiP6A73", "HiP6A91",
                        "HiP6A109", "HiP6A127", "HiP6A6",
                        "HiP6A24", "HiP6A42", "HiP6A60",
                        "HiP6A78", "HiP6A96", "HiP6A114",
                        "HiP6A132", "HiP6A11", "HiP6A29",
                        "HiP6A47", "HiP6A65", "HiP6A83",
                        "HiP6A101", "HiP6A119", "HiP6A137",
                        "HiP6A15", "HiP6A33", "HiP6A51",
                        "HiP6A69", "HiP6A87", "HiP6A105",
                        "HiP6A123", "HiP6A141")
extraversion_items_reverse <- c(-1, 1, -1,
                                -1, 1, 1,
                                1, 1, 1,
                                1, -1, -1,
                                1, 1, 1,
                                1, 1, 1,
                                1, 1, 1,
                                1, 1, 1,
                                1, 1, 1,
                                1, -1, 1,
                                1, 1) # -1 for reversed items
print(
  preproc_w6_hipic |>
    select(all_of(extraversion_items)) |>
    alpha(keys = extraversion_items_reverse,
          n.iter = 10000, # bootstrap confidence intervals with 10000 iterations
          title = "Extraversion")
)

# Agreeableness ----
agreeableness_items <- c("HiP6A2", "HiP6A20", "HiP6A38",
                          "HiP6A56", "HiP6A74", "HiP6A92",
                          "HiP6A110", "HiP6A128", "HiP6A7",
                          "HiP6A25", "HiP6A43", "HiP6A61",
                          "HiP6A79", "HiP6A97", "HiP6A115",
                          "HiP6A133", "HiP6A12", "HiP6A30",
                          "HiP6A48", "HiP6A66", "HiP6A84",
                          "HiP6A102", "HiP6A120", "HiP6A138",
                          "HiP6A16", "HiP6A34", "HiP6A52",
                          "HiP6A70", "HiP6A88", "HiP6A106",
                          "HiP6A124", "HiP6A142", "HiP6A18",
                          "HiP6A36", "HiP6A54", "HiP6A72",
                          "HiP6A90", "HiP6A108", "HiP6A126",
                          "HiP6A144")
agreeableness_items_reverse <- c(1, -1, 1,
                                 -1, 1, 1,
                                 -1, 1, 1,
                                 1, -1, -1,
                                 -1, 1, 1,
                                 1, 1, 1,
                                 1, 1, -1,
                                 1, -1, -1,
                                 -1, 1, 1,
                                 1, -1, 1,
                                 -1, 1, 1,
                                 1, 1, 1,
                                 1, 1, 1,
                                 1) # -1 for reversed items
print(
  preproc_w6_hipic |>
    select(all_of(agreeableness_items)) |>
    alpha(keys = agreeableness_items_reverse,
          n.iter = 10000, # bootstrap confidence intervals with 10000 iterations
          title = "Agreeableness")
)

# Conscientiousness ----
conscientiousness_items <- c("HiP6A3", "HiP6A21", "HiP6A39",
                             "HiP6A57", "HiP6A75", "HiP6A93",
                             "HiP6A111", "HiP6A129", "HiP6A8",
                             "HiP6A26", "HiP6A44", "HiP6A62",
                             "HiP6A80", "HiP6A98", "HiP6A116",
                             "HiP6A134", "HiP6A13", "HiP6A31",
                             "HiP6A49", "HiP6A67", "HiP6A85",
                             "HiP6A103", "HiP6A121", "HiP6A139",
                             "HiP6A17", "HiP6A35", "HiP6A53",
                             "HiP6A71", "HiP6A89", "HiP6A107",
                             "HiP6A125", "HiP6A143")
conscientiousness_items_reverse <- c(1, 1, 1,
                                     1, 1, 1,
                                     1, -1, -1,
                                     1, -1, -1,
                                     -1, 1, 1,
                                     1, 1, 1,
                                     -1, 1, 1,
                                     -1, -1, -1,
                                     -1, -1, -1,
                                     1, -1, -1,
                                     1, -1) # -1 for reversed items
print(
  preproc_w6_hipic |>
    select(all_of(conscientiousness_items)) |>
    alpha(keys = conscientiousness_items_reverse,
          n.iter = 10000, # bootstrap confidence intervals with 10000 iterations
          title = "Conscientiousness")
)

# Neuroticism ----
neuroticism_items <- c("HiP6A4", "HiP6A22", "HiP6A40",
                       "HiP6A58", "HiP6A76", "HiP6A94",
                       "HiP6A112", "HiP6A130", "HiP6A9",
                       "HiP6A27", "HiP6A45", "HiP6A63",
                       "HiP6A81", "HiP6A99", "HiP6A117",
                       "HiP6A135")
neuroticism_items_reverse <- c(1, 1, 1,
                               1, 1, 1,
                               1, 1, 1,
                               -1, 1, -1,
                               1, -1, 1,
                               1) # -1 for reversed items
print(
  preproc_w6_hipic |>
    select(all_of(neuroticism_items)) |>
    alpha(keys = neuroticism_items_reverse,
          n.iter = 10000, # bootstrap confidence intervals with 10000 iterations
          title = "Neuroticism")
)
openness_items <- c("HiP6A5", "HiP6A23", "HiP6A41",
                    "HiP6A59", "HiP6A77", "HiP6A95",
                    "HiP6A113", "HiP6A131", "HiP6A10",
                    "HiP6A28", "HiP6A46", "HiP6A64",
                    "HiP6A82", "HiP6A100", "HiP6A118",
                    "HiP6A136", "HiP6A14", "HiP6A32",
                    "HiP6A50", "HiP6A68", "HiP6A86",
                    "HiP6A104", "HiP6A122", "HiP6A140")
openness_items_reverse <- c(-1, 1, -1,
                            1, 1, 1,
                            1, 1, 1,
                            1, 1, 1,
                            1, 1, 1,
                            1, 1, 1,
                            -1, 1, 1,
                            1, -1, 1) # -1 for reversed items
print(
  preproc_w6_hipic |>
    select(all_of(openness_items)) |>
    alpha(keys = openness_items_reverse,
          n.iter = 10000, # bootstrap confidence intervals with 10000 iterations
          title = "Openness")
)

# End sink to stop capturing output in the text file ----
sink(file = NULL)

# Distribution of HiPIC scores ----
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
extraversion_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(sex) & !is.na(extraversion)),
    mapping = aes(x = extraversion,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(extraversion)),
    mapping = aes(x = extraversion, linetype = "Overall"),
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
    title = "Distribution of Average Extraversion Scores (HiPIC W6)",
    x = "Average Extraversion Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W6/HiPIC6A_extraversion_sex_plot.svg",
       plot = extraversion_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

agreeableness_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(sex) & !is.na(agreeableness)),
    mapping = aes(x = agreeableness,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(agreeableness)),
    mapping = aes(x = agreeableness, linetype = "Overall"),
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
    title = "Distribution of Average Agreeableness Scores (HiPIC W6)",
    x = "Average Agreeableness Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W6/HiPIC6A_agreeableness_sex_plot.svg",
       plot = agreeableness_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

conscientiousness_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(sex) & !is.na(conscientiousness)),
    mapping = aes(x = conscientiousness,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(conscientiousness)),
    mapping = aes(x = conscientiousness, linetype = "Overall"),
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
    title = "Distribution of Average Conscientiousness Scores (HiPIC W6)",
    x = "Average Conscientiousness Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W6/HiPIC6A_conscientiousness_sex_plot.svg",
       plot = conscientiousness_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

neuroticism_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(sex) & !is.na(neuroticism)),
    mapping = aes(x = neuroticism,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(neuroticism)),
    mapping = aes(x = neuroticism, linetype = "Overall"),
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
    title = "Distribution of Average Neuroticism Scores (HiPIC W6)",
    x = "Average Neuroticism Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W6/HiPIC6A_neuroticism_sex_plot.svg",
       plot = neuroticism_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")

openness_sex_plot <- ggplot() +
  # sex-specific densities (fill + colored outline)
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(sex) & !is.na(openness)),
    mapping = aes(x = openness,
                  fill = factor(sex), color = factor(sex)),
    alpha = 0.35, show.legend = TRUE
  ) +
  # overall density (dashed black), map a constant linetype
  # so it appears in legend
  geom_density(
    data = filter(preproc_w6_hipic, !is.na(openness)),
    mapping = aes(x = openness, linetype = "Overall"),
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
    title = "Distribution of Average Openness Scores (HiPIC W6)",
    x = "Average Openness Score",
    y = "Density",
    fill = "Sex"                       # name for the fill legend
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal()

ggsave("reports/plots/W6/HiPIC6A_openness_sex_plot.svg",
       plot = openness_sex_plot,
       device = "svg",
       width = 10,
       height = 10,
       units = "in")