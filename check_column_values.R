# Script to check the actual values in key columns
# This will help us understand why we're getting 0% for some variables

# Load necessary libraries
library(readr)
library(dplyr)

# Load datasets
cat("Loading datasets...\n")
qol_2020 <- read.csv("qols-2020-2021-new-weights-v1.csv", stringsAsFactors = FALSE)
qol_2017 <- read.csv("qols-v-2017-2018-v1.1.csv", stringsAsFactors = FALSE)

# Filter for City of Johannesburg only
cat("\nFiltering for City of Johannesburg only...\n")
joburg_2020 <- qol_2020 %>% 
  filter(District_municipality == "City of Johannesburg" | municipality_coded == "City of Johannesburg")

joburg_2017 <- qol_2017 %>% 
  filter(munic == "City of Johannesburg" | munic_recode == "Johannesburg")

# Function to check column values
check_column_values <- function(df, col_name, max_values = 10) {
  if (!col_name %in% names(df)) {
    cat("Column '", col_name, "' not found in the dataset.\n")
    return(NULL)
  }
  
  # Get value counts
  value_counts <- table(df[[col_name]], useNA = "always")
  
  # Sort by frequency (descending)
  sorted_counts <- sort(value_counts, decreasing = TRUE)
  
  # Limit the number of values to display
  if (length(sorted_counts) > max_values) {
    cat("Column '", col_name, "' has", length(value_counts) - 1, "unique values (excluding NA). Displaying top", max_values, "by frequency:\n")
    display_counts <- sorted_counts[1:max_values]
  } else {
    cat("Column '", col_name, "' has", length(value_counts) - 1, "unique values (excluding NA):\n")
    display_counts <- sorted_counts
  }
  
  # Display the values and their counts
  for (val in names(display_counts)) {
    if (is.na(val)) {
      cat("- NA: ", display_counts[which(is.na(names(display_counts)))], " occurrences\n", sep = "")
    } else {
      cat("- '", val, "': ", display_counts[val], " occurrences\n", sep = "")
    }
  }
  
  # Calculate percentage of each value
  total <- sum(value_counts)
  cat("\nPercentages:\n")
  for (val in names(sorted_counts)[1:min(max_values, length(sorted_counts))]) {
    if (is.na(val)) {
      cat("- NA: ", round(sorted_counts[which(is.na(names(sorted_counts)))] / total * 100, 2), "%\n", sep = "")
    } else {
      cat("- '", val, "': ", round(sorted_counts[val] / total * 100, 2), "%\n", sep = "")
    }
  }
  
  return(sorted_counts)
}

# Check key columns in 2020 dataset
cat("\n=== 2020-2021 DATASET ===\n")

cat("\nUnemployment (q10_4_unemployed):\n")
check_column_values(joburg_2020, "q10_4_unemployed")

cat("\nFood Insecurity (q6_4_skip_meal):\n")
check_column_values(joburg_2020, "q6_4_skip_meal")

cat("\nMedical Insurance (q13_5_medical_aid):\n")
check_column_values(joburg_2020, "q13_5_medical_aid")

cat("\nHealthcare Use (q13_1_healthcare):\n")
check_column_values(joburg_2020, "q13_1_healthcare")

cat("\nDwelling Type (a3_dwelling_type_recode):\n")
check_column_values(joburg_2020, "a3_dwelling_type_recode")

# Check key columns in 2017 dataset
cat("\n=== 2017-2018 DATASET ===\n")

cat("\nUnemployment (Q11_15_unemployed):\n")
check_column_values(joburg_2017, "Q11_15_unemployed")

cat("\nFood Insecurity (Q7_04_meal):\n")
check_column_values(joburg_2017, "Q7_04_meal")

cat("\nMedical Insurance (Q14_11_health):\n")
check_column_values(joburg_2017, "Q14_11_health")

cat("\nHealthcare Use (Q14_01_healthcare_services):\n")
check_column_values(joburg_2017, "Q14_01_healthcare_services")

cat("\nDwelling Type (A3_dwelling_recode):\n")
check_column_values(joburg_2017, "A3_dwelling_recode")

# Check ward columns
cat("\n=== WARD COLUMNS ===\n")

cat("\n2020 Ward Column (ward_code):\n")
ward_2020_counts <- table(joburg_2020$ward_code)
cat("Number of unique wards in 2020 dataset:", length(ward_2020_counts), "\n")
cat("Average respondents per ward:", mean(ward_2020_counts), "\n")

cat("\n2017 Ward Column (ward):\n")
ward_2017_counts <- table(joburg_2017$ward)
cat("Number of unique wards in 2017 dataset:", length(ward_2017_counts), "\n")
cat("Average respondents per ward:", mean(ward_2017_counts), "\n")
