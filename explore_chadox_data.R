# ChAdOx Data Exploration Script
# This script examines the structure of ChAdOx datasets to identify relevant variables

# Load necessary libraries
library(dplyr)
library(tidyr)

# Load ChAdOx data
cat("Loading ChAdOx datasets...\n")
load("ChAdOx data 2024-07-19.Rdata")
load("ChAdOx supp visit data 2024-07-19.Rdata")
cat("Successfully loaded ChAdOx datasets\n")

# List all loaded datasets
cat("\nAvailable ChAdOx datasets:\n")
chadox_datasets <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
print(chadox_datasets)

# Function to explore dataset structure
explore_dataset <- function(dataset_name) {
  if (!exists(dataset_name)) {
    cat("\nDataset", dataset_name, "not found\n")
    return(NULL)
  }
  
  dataset <- get(dataset_name)
  cat("\n==== Dataset:", dataset_name, "====\n")
  cat("Dimensions:", nrow(dataset), "rows,", ncol(dataset), "columns\n")
  
  # Show first few column names
  cat("First 10 columns:", paste(names(dataset)[1:min(10, ncol(dataset))], collapse=", "), "\n")
  
  # Show column types
  col_types <- sapply(dataset, function(x) class(x)[1])  # Take only first class
  cat("\nColumn types summary:\n")
  print(table(col_types))
  
  # Check for date columns
  date_cols <- names(dataset)[sapply(dataset, function(x) 
    inherits(x, "Date") || 
    (is.character(x) && any(grepl("^\\d{4}-\\d{2}-\\d{2}$", head(na.omit(x), 100)), na.rm = TRUE))
  )]
  cat("\nPotential date columns:", paste(date_cols, collapse=", "), "\n")
  
  # Check for health-related columns
  health_cols <- names(dataset)[grepl("fever|temp|cough|breath|smell|taste|fatigue|symptom|covid|ill|hosp|oxygen|respiratory", 
                                     names(dataset), ignore.case = TRUE)]
  cat("\nPotential health-related columns:", paste(health_cols, collapse=", "), "\n")
  
  # Check for demographic columns
  demo_cols <- names(dataset)[grepl("age|sex|gender|race|ethnicity|location|area|region", 
                                   names(dataset), ignore.case = TRUE)]
  cat("\nPotential demographic columns:", paste(demo_cols, collapse=", "), "\n")
  
  # Show sample data for key columns
  if (length(c(date_cols, health_cols, demo_cols)) > 0) {
    key_cols <- unique(c(date_cols, health_cols, demo_cols))
    key_cols <- key_cols[key_cols %in% names(dataset)]
    
    if (length(key_cols) > 0) {
      cat("\nSample data for key columns:\n")
      tryCatch({
        sample_data <- dataset %>% 
          select(all_of(key_cols)) %>% 
          head(5)
        print(sample_data)
      }, error = function(e) {
        cat("Error displaying sample data:", e$message, "\n")
      })
    }
  }
  
  return(invisible(NULL))
}

# Explore key datasets
key_datasets <- c("covid_cases", "illness_visit", "diary_card_illness_v2_3", 
                 "demographic_and_contact_information_v2_3")

for (dataset in key_datasets) {
  explore_dataset(dataset)
}

# Identify variables for heat-health analysis
cat("\n\n==== VARIABLES FOR HEAT-HEALTH ANALYSIS ====\n")

# COVID cases variables
if (exists("covid_cases")) {
  cat("\nCOVID cases variables:\n")
  covid_vars <- names(covid_cases)[grepl("date|result|hosp|severity|symptom", 
                                       names(covid_cases), ignore.case = TRUE)]
  for (var in covid_vars) {
    cat("- ", var, ": ", paste(unique(covid_cases[[var]])[1:min(5, length(unique(covid_cases[[var]])))], collapse=", "), "...\n")
  }
}

# Illness visit variables
if (exists("illness_visit")) {
  cat("\nIllness visit variables:\n")
  illness_vars <- names(illness_visit)[grepl("date|temp|oxy|resp|pulse", 
                                          names(illness_visit), ignore.case = TRUE)]
  for (var in illness_vars) {
    cat("- ", var, ": ", paste(unique(illness_visit[[var]])[1:min(5, length(unique(illness_visit[[var]])))], collapse=", "), "...\n")
  }
}

# Symptom variables
if (exists("diary_card_illness_v2_3")) {
  cat("\nSymptom variables:\n")
  symptom_vars <- names(diary_card_illness_v2_3)[grepl("date|fever|cough|sob|smell|taste|fatigue", 
                                                     names(diary_card_illness_v2_3), ignore.case = TRUE)]
  for (var in symptom_vars) {
    cat("- ", var, ": ", paste(unique(diary_card_illness_v2_3[[var]])[1:min(5, length(unique(diary_card_illness_v2_3[[var]])))], collapse=", "), "...\n")
  }
}

# Demographic variables
if (exists("demographic_and_contact_information_v2_3")) {
  cat("\nDemographic variables:\n")
  demo_vars <- names(demographic_and_contact_information_v2_3)[grepl("age|sex|race|area", 
                                                                  names(demographic_and_contact_information_v2_3), ignore.case = TRUE)]
  for (var in demo_vars) {
    cat("- ", var, ": ", paste(unique(demographic_and_contact_information_v2_3[[var]])[1:min(5, length(unique(demographic_and_contact_information_v2_3[[var]])))], collapse=", "), "...\n")
  }
}

# Save exploration results
sink("chadox_data_exploration_results.txt")
for (dataset in key_datasets) {
  explore_dataset(dataset)
}
sink()

cat("\nExploration complete. Results saved to 'chadox_data_exploration_results.txt'\n")
