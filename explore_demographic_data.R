# Script to explore demographic data structure in ChAdOx dataset

# Load necessary libraries
library(dplyr)

# Load ChAdOx data
cat("Loading ChAdOx datasets...\n")
load("ChAdOx data 2024-07-19.Rdata")
load("ChAdOx supp visit data 2024-07-19.Rdata")
cat("Successfully loaded ChAdOx datasets\n")

# Explore demographic data structure
cat("\nExploring demographic data structure...\n")
if (exists("demographic_and_contact_information_v2_3")) {
  cat("Demographic data dimensions:", nrow(demographic_and_contact_information_v2_3), "rows,", 
      ncol(demographic_and_contact_information_v2_3), "columns\n")
  
  # Show first 20 column names
  cat("First 20 column names:", paste(names(demographic_and_contact_information_v2_3)[1:min(20, ncol(demographic_and_contact_information_v2_3))], collapse=", "), "...\n")
  
  # Check for demographic columns
  demo_cols <- names(demographic_and_contact_information_v2_3)[grepl("age|sex|gender|race|ethnicity", 
                                                                    names(demographic_and_contact_information_v2_3), ignore.case = TRUE)]
  cat("Potential demographic columns:", paste(demo_cols, collapse=", "), "\n")
  
  # Show sample data for key demographic columns
  if (length(demo_cols) > 0) {
    cat("\nSample data for demographic columns:\n")
    sample_data <- demographic_and_contact_information_v2_3 %>% 
      select(all_of(demo_cols)) %>% 
      head(10)
    print(sample_data)
  }
} else {
  cat("Demographic data not found\n")
}

# Check all available datasets for demographic information
cat("\nChecking all datasets for demographic information...\n")
all_datasets <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
for (dataset_name in all_datasets) {
  dataset <- get(dataset_name)
  demo_cols <- names(dataset)[grepl("age|sex|gender|race|ethnicity", names(dataset), ignore.case = TRUE)]
  if (length(demo_cols) > 0) {
    cat("\nDataset:", dataset_name, "has demographic columns:", paste(demo_cols, collapse=", "), "\n")
    sample_data <- dataset %>% 
      select(all_of(demo_cols)) %>% 
      head(5)
    print(sample_data)
  }
}
