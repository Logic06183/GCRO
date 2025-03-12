# Script to identify key variables in GCRO datasets
# This will help us find the appropriate column names for our comparison analysis

# Load necessary libraries
library(readr)
library(dplyr)
library(stringr)

# Load datasets
cat("Loading datasets...\n")
qol_2020 <- read.csv("qols-2020-2021-new-weights-v1.csv", stringsAsFactors = FALSE)
qol_2017 <- read.csv("qols-v-2017-2018-v1.1.csv", stringsAsFactors = FALSE)

# Display basic information about the datasets
cat("Dataset dimensions:\n")
cat("2020-2021 dataset:", dim(qol_2020)[1], "rows,", dim(qol_2020)[2], "columns\n")
cat("2017-2018 dataset:", dim(qol_2017)[1], "rows,", dim(qol_2017)[2], "columns\n")

# Function to find columns related to a specific topic
find_related_columns <- function(df, keywords, max_display = 10) {
  # Create a regex pattern with all keywords
  pattern <- paste0(keywords, collapse = "|")
  
  # Find columns that match the pattern
  matching_cols <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
  
  # Limit the number of columns to display
  if (length(matching_cols) > max_display) {
    cat("Found", length(matching_cols), "matching columns. Displaying first", max_display, ":\n")
    matching_cols_display <- matching_cols[1:max_display]
  } else {
    cat("Found", length(matching_cols), "matching columns:\n")
    matching_cols_display <- matching_cols
  }
  
  # Display the column names
  for (col in matching_cols_display) {
    cat("- ", col, "\n")
  }
  
  # Return all matching columns
  return(matching_cols)
}

# Function to examine the values in a column
examine_column_values <- function(df, col_name, max_values = 10) {
  if (!col_name %in% names(df)) {
    cat("Column '", col_name, "' not found in the dataset.\n")
    return(NULL)
  }
  
  # Get unique values
  unique_vals <- unique(df[[col_name]])
  
  # Count occurrences of each value
  value_counts <- table(df[[col_name]])
  
  # Sort by frequency (descending)
  sorted_counts <- sort(value_counts, decreasing = TRUE)
  
  # Limit the number of values to display
  if (length(sorted_counts) > max_values) {
    cat("Column '", col_name, "' has", length(unique_vals), "unique values. Displaying top", max_values, "by frequency:\n")
    display_counts <- sorted_counts[1:max_values]
  } else {
    cat("Column '", col_name, "' has", length(unique_vals), "unique values:\n")
    display_counts <- sorted_counts
  }
  
  # Display the values and their counts
  for (val in names(display_counts)) {
    cat("- '", val, "': ", display_counts[val], " occurrences\n", sep = "")
  }
  
  return(sorted_counts)
}

# Function to find ward ID columns
find_ward_columns <- function(df) {
  # Look for column names containing "ward" (case insensitive)
  ward_cols <- grep("ward", names(df), ignore.case = TRUE, value = TRUE)
  
  # Also look for columns with GT or ward codes
  gt_cols <- grep("^GT", names(df), value = TRUE)
  ward_code_cols <- grep("^[0-9]{5,}", names(df), value = TRUE)
  
  # Combine all potential ward identifier columns
  potential_cols <- unique(c(ward_cols, gt_cols, ward_code_cols))
  
  cat("Potential ward identifier columns:\n")
  for (col in potential_cols) {
    cat("- ", col, "\n")
  }
  
  return(potential_cols)
}

# Identify ward columns
cat("\n=== WARD IDENTIFIERS ===\n")
cat("2020-2021 dataset:\n")
ward_cols_2020 <- find_ward_columns(qol_2020)

cat("\n2017-2018 dataset:\n")
ward_cols_2017 <- find_ward_columns(qol_2017)

# Look for employment-related columns
cat("\n=== EMPLOYMENT STATUS ===\n")
cat("2020-2021 dataset:\n")
employment_cols_2020 <- find_related_columns(qol_2020, c("employ", "job", "work", "unemploy"))

cat("\n2017-2018 dataset:\n")
employment_cols_2017 <- find_related_columns(qol_2017, c("employ", "job", "work", "unemploy"))

# Look for food insecurity columns
cat("\n=== FOOD INSECURITY ===\n")
cat("2020-2021 dataset:\n")
food_cols_2020 <- find_related_columns(qol_2020, c("food", "hunger", "meal", "eat"))

cat("\n2017-2018 dataset:\n")
food_cols_2017 <- find_related_columns(qol_2017, c("food", "hunger", "meal", "eat"))

# Look for medical insurance columns
cat("\n=== MEDICAL INSURANCE ===\n")
cat("2020-2021 dataset:\n")
insurance_cols_2020 <- find_related_columns(qol_2020, c("medical", "insurance", "health", "healthcare"))

cat("\n2017-2018 dataset:\n")
insurance_cols_2017 <- find_related_columns(qol_2017, c("medical", "insurance", "health", "healthcare"))

# Look for housing/dwelling columns
cat("\n=== HOUSING/DWELLING CONDITIONS ===\n")
cat("2020-2021 dataset:\n")
housing_cols_2020 <- find_related_columns(qol_2020, c("house", "dwelling", "room", "crowded", "household"))

cat("\n2017-2018 dataset:\n")
housing_cols_2017 <- find_related_columns(qol_2017, c("house", "dwelling", "room", "crowded", "household"))

# Look for municipality/location columns
cat("\n=== MUNICIPALITY/LOCATION ===\n")
cat("2020-2021 dataset:\n")
location_cols_2020 <- find_related_columns(qol_2020, c("munic", "local", "region", "district", "city"))

cat("\n2017-2018 dataset:\n")
location_cols_2017 <- find_related_columns(qol_2017, c("munic", "local", "region", "district", "city"))

# Examine a sample of columns to understand their values
cat("\n=== EXAMINING COLUMN VALUES ===\n")

# Function to safely examine columns
safe_examine <- function(df, cols, dataset_name) {
  cat("\n", dataset_name, ":\n", sep = "")
  for (col in cols[1:min(3, length(cols))]) {
    cat("\nColumn: ", col, "\n", sep = "")
    examine_column_values(df, col)
  }
}

# Examine employment columns
cat("\nEMPLOYMENT STATUS VALUES:\n")
safe_examine(qol_2020, employment_cols_2020, "2020-2021 dataset")
safe_examine(qol_2017, employment_cols_2017, "2017-2018 dataset")

# Examine food insecurity columns
cat("\nFOOD INSECURITY VALUES:\n")
safe_examine(qol_2020, food_cols_2020, "2020-2021 dataset")
safe_examine(qol_2017, food_cols_2017, "2017-2018 dataset")

# Examine medical insurance columns
cat("\nMEDICAL INSURANCE VALUES:\n")
safe_examine(qol_2020, insurance_cols_2020, "2020-2021 dataset")
safe_examine(qol_2017, insurance_cols_2017, "2017-2018 dataset")

# Examine housing columns
cat("\nHOUSING/DWELLING VALUES:\n")
safe_examine(qol_2020, housing_cols_2020, "2020-2021 dataset")
safe_examine(qol_2017, housing_cols_2017, "2017-2018 dataset")

# Examine municipality/location columns
cat("\nMUNICIPALITY/LOCATION VALUES:\n")
safe_examine(qol_2020, location_cols_2020, "2020-2021 dataset")
safe_examine(qol_2017, location_cols_2017, "2017-2018 dataset")

# Save the results to a text file
sink("variable_identification_results.txt")

cat("GCRO SURVEY VARIABLE IDENTIFICATION RESULTS\n")
cat("===========================================\n\n")

cat("This file contains the results of our variable identification process for the GCRO surveys.\n")
cat("It lists potential columns in both datasets that correspond to our key variables of interest.\n\n")

cat("WARD IDENTIFIERS:\n")
cat("2020-2021 dataset:", paste(ward_cols_2020, collapse = ", "), "\n")
cat("2017-2018 dataset:", paste(ward_cols_2017, collapse = ", "), "\n\n")

cat("EMPLOYMENT STATUS:\n")
cat("2020-2021 dataset:", paste(employment_cols_2020, collapse = ", "), "\n")
cat("2017-2018 dataset:", paste(employment_cols_2017, collapse = ", "), "\n\n")

cat("FOOD INSECURITY:\n")
cat("2020-2021 dataset:", paste(food_cols_2020, collapse = ", "), "\n")
cat("2017-2018 dataset:", paste(food_cols_2017, collapse = ", "), "\n\n")

cat("MEDICAL INSURANCE:\n")
cat("2020-2021 dataset:", paste(insurance_cols_2020, collapse = ", "), "\n")
cat("2017-2018 dataset:", paste(insurance_cols_2017, collapse = ", "), "\n\n")

cat("HOUSING/DWELLING CONDITIONS:\n")
cat("2020-2021 dataset:", paste(housing_cols_2020, collapse = ", "), "\n")
cat("2017-2018 dataset:", paste(housing_cols_2017, collapse = ", "), "\n\n")

cat("MUNICIPALITY/LOCATION:\n")
cat("2020-2021 dataset:", paste(location_cols_2020, collapse = ", "), "\n")
cat("2017-2018 dataset:", paste(location_cols_2017, collapse = ", "), "\n\n")

sink()

cat("\nVariable identification complete. Results saved to 'variable_identification_results.txt'.\n")
cat("Use these results to update the main analysis script with the correct column names.\n")
