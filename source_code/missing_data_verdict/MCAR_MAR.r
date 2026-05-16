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
result <- mcar_test(data) 
print(result)

write_csv(result, "data/analysis/mcar_test_results.csv")

# End sink to stop capturing output in the text file ----
sink(file = NULL)