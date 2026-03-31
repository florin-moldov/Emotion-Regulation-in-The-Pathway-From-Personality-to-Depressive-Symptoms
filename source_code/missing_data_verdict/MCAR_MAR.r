# Determining MCAR or MAR or not random at all nature of our data.
#
# This (short) script performs Little's  MCAR test on the dataset 
# and prints the results.

# Set seed for reproducibility ----
set.seed(123)

# Load libraries ----
library(tidyverse)
library(naniar)

# Load data ----
data <- read_csv("data/merged/merged_data_with_gender.csv")

# Perform missing data test ----
result <- mcar_test(data) # Hallelujah, (some) MCAR inclination 
# So FIML is appropriate
write_csv(result, "data/analysis/mcar_test_results.csv")