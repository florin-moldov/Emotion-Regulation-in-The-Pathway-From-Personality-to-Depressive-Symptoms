# Determining MCAR or MAR or not random at all nature of our data.
#
#################################################################
# anxiety scores instead of depression scores at W11
##################################################################
#
# This (short) script performs Little's MCAR test
# on the dataset and prints the results.

# Set seed for reproducibility ----
set.seed(123)

# Set sink to capture all output in a text file ----
sink("data/analysis/MCAR_MAR_anx.txt")

# Load libraries ----
library(tidyverse)
library(naniar)

# Load data ----
data <- read_csv("data/merged/merged_data_with_gender_and_anx.csv")

# Perform MCAR test ----
result <- mcar_test(data) 
print(result)

# End sink to stop capturing output in the text file ----
sink(file = NULL)