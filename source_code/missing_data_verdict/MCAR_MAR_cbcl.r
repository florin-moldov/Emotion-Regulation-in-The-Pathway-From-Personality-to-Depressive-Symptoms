# Determining MCAR or MAR or not random at all nature of our data.
#
#################################################################
# Now also for internalizing scores at W6 (CBCL)
##################################################################
#
# This (short) script performs Little's MCAR test and if needed logistic regression
# (MAR tests) on the dataset and prints the results.

# Set seed for reproducibility ----
set.seed(123)

# Set sink to capture all output in a text file ----
sink("data/analysis/MCAR_MAR_cbcl.txt")

# Load libraries ----
library(tidyverse)
library(naniar)

# Load data ----
data <- read_csv("data/merged/merged_data_with_gender_and_cbcl.csv")

# Perform MCAR test ----
result <- mcar_test(data) 
print(result)

# End sink to stop capturing output in the text file ----
sink(file = NULL)