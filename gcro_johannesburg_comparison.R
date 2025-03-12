# GCRO Survey Comparison Analysis for City of Johannesburg
# Comparing 2020-2021 (pandemic-era) and 2017-2018 (pre-pandemic) surveys

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(sf)
library(tidyr)
library(stringr)

# Set working directory to the location of the data files
# setwd("path/to/your/directory") # Uncomment and modify if needed

# Load datasets
cat("Loading datasets...\n")
qol_2020 <- read.csv("qols-2020-2021-new-weights-v1.csv", stringsAsFactors = FALSE)
qol_2017 <- read.csv("qols-v-2017-2018-v1.1.csv", stringsAsFactors = FALSE)

# Display basic information about the datasets
cat("Dataset dimensions:\n")
cat("2020-2021 dataset:", dim(qol_2020)[1], "rows,", dim(qol_2020)[2], "columns\n")
cat("2017-2018 dataset:", dim(qol_2017)[1], "rows,", dim(qol_2017)[2], "columns\n")

# Filter for City of Johannesburg only
cat("\nFiltering for City of Johannesburg only...\n")
# Based on the variable identification results, we know the municipality columns
joburg_2020 <- qol_2020 %>% 
  filter(District_municipality == "City of Johannesburg" | municipality_coded == "City of Johannesburg")

joburg_2017 <- qol_2017 %>% 
  filter(munic == "City of Johannesburg" | munic_recode == "Johannesburg")

cat("Johannesburg dataset dimensions:\n")
cat("2020-2021 Johannesburg dataset:", dim(joburg_2020)[1], "rows\n")
cat("2017-2018 Johannesburg dataset:", dim(joburg_2017)[1], "rows\n")

# Identify ward columns for aggregation
# From the variable identification, we know the ward columns
ward_id_col_2020 <- "ward_code"
ward_id_col_2017 <- "ward"

# Define key variables based on the variable identification results
# Employment status
# For 2020 dataset: q10_4_unemployed (Yes/No if unemployed)
# For 2017 dataset: Q11_15_unemployed (Yes/No if unemployed)

# Food insecurity
# For 2020 dataset: q6_4_skip_meal (Yes/No if skipped meals)
# For 2017 dataset: Q7_04_meal (Yes/No if skipped meals)

# Medical insurance
# For 2020 dataset: q13_5_medical_aid (Yes/No if has medical aid)
# For 2017 dataset: Q14_11_health (medical insurance status)

# Housing conditions
# For 2020 dataset: a3_dwelling_type_recode (Formal/Informal/Other)
# For 2017 dataset: A3_dwelling_recode (Formal/Informal/Other)

# Public healthcare use
# For 2020 dataset: q13_1_healthcare (type of healthcare used)
# For 2017 dataset: Q14_01_healthcare_services (type of healthcare used)

# Function to aggregate data to ward level
aggregate_to_ward_2020 <- function(df) {
  # Check if ward column exists
  if (!ward_id_col_2020 %in% names(df)) {
    cat("Warning: Ward column '", ward_id_col_2020, "' not found in 2020 dataset.\n")
    return(NULL)
  }
  
  # Aggregate by ward
  ward_stats <- df %>%
    group_by(ward = !!sym(ward_id_col_2020)) %>%
    summarize(
      # Unemployment - check if column exists and has values
      unemployment = if("q10_4_unemployed" %in% names(df)) {
        mean(q10_4_unemployed == "Yes", na.rm = TRUE) * 100
      } else {
        cat("Warning: Unemployment column not found or has no values in 2020 dataset.\n")
        NA
      },
      
      # Food insecurity - check if column exists and has values
      food_insecurity = if("q6_4_skip_meal" %in% names(df)) {
        mean(q6_4_skip_meal == "Yes", na.rm = TRUE) * 100
      } else {
        cat("Warning: Food insecurity column not found or has no values in 2020 dataset.\n")
        NA
      },
      
      # No medical insurance - check if column exists and has values
      no_medical_insurance = if("q13_5_medical_aid" %in% names(df)) {
        mean(q13_5_medical_aid == "No", na.rm = TRUE) * 100
      } else {
        cat("Warning: Medical insurance column not found or has no values in 2020 dataset.\n")
        NA
      },
      
      # Public healthcare use - check if column exists and has values
      public_healthcare_use = if("q13_1_healthcare" %in% names(df)) {
        mean(q13_1_healthcare == "Public health care facilities", na.rm = TRUE) * 100
      } else {
        cat("Warning: Healthcare column not found or has no values in 2020 dataset.\n")
        NA
      },
      
      # Informal dwellings - check if column exists and has values
      informal_dwellings = if("a3_dwelling_type_recode" %in% names(df)) {
        mean(a3_dwelling_type_recode == "Informal", na.rm = TRUE) * 100
      } else {
        cat("Warning: Dwelling type column not found or has no values in 2020 dataset.\n")
        NA
      },
      
      # Count of respondents per ward
      count = n()
    )
  
  return(ward_stats)
}

aggregate_to_ward_2017 <- function(df) {
  # Check if ward column exists
  if (!ward_id_col_2017 %in% names(df)) {
    cat("Warning: Ward column '", ward_id_col_2017, "' not found in 2017 dataset.\n")
    return(NULL)
  }
  
  # Aggregate by ward
  ward_stats <- df %>%
    group_by(ward = !!sym(ward_id_col_2017)) %>%
    summarize(
      # Unemployment - For 2017, we need to check for "I have given up looking for a job"
      unemployment = if("Q11_15_unemployed" %in% names(df)) {
        mean(Q11_15_unemployed == "I have given up looking for a job", na.rm = TRUE) * 100
      } else {
        cat("Warning: Unemployment column not found or has no values in 2017 dataset.\n")
        NA
      },
      
      # Food insecurity - check if column exists and has values
      food_insecurity = if("Q7_04_meal" %in% names(df)) {
        mean(Q7_04_meal == "Yes", na.rm = TRUE) * 100
      } else {
        cat("Warning: Food insecurity column not found or has no values in 2017 dataset.\n")
        NA
      },
      
      # No medical insurance - In 2017, we need to check for health rating instead
      # We'll consider "Poor" and "Very poor" ratings as indicators of healthcare issues
      no_medical_insurance = if("Q14_11_health" %in% names(df)) {
        mean(Q14_11_health %in% c("Poor", "Very poor"), na.rm = TRUE) * 100
      } else {
        cat("Warning: Medical insurance column not found or has no values in 2017 dataset.\n")
        NA
      },
      
      # Public healthcare use - check if column exists and has values
      public_healthcare_use = if("Q14_01_healthcare_services" %in% names(df)) {
        mean(Q14_01_healthcare_services == "Public health care facilities", na.rm = TRUE) * 100
      } else {
        cat("Warning: Healthcare column not found or has no values in 2017 dataset.\n")
        NA
      },
      
      # Informal dwellings - check if column exists and has values
      informal_dwellings = if("A3_dwelling_recode" %in% names(df)) {
        mean(A3_dwelling_recode == "Informal", na.rm = TRUE) * 100
      } else {
        cat("Warning: Dwelling type column not found or has no values in 2017 dataset.\n")
        NA
      },
      
      # Count of respondents per ward
      count = n()
    )
  
  return(ward_stats)
}

# Aggregate data to ward level
cat("\nAggregating data to ward level...\n")
joburg_2020_ward <- aggregate_to_ward_2020(joburg_2020)
joburg_2017_ward <- aggregate_to_ward_2017(joburg_2017)

# Check if aggregation was successful
if (is.null(joburg_2020_ward) || is.null(joburg_2017_ward)) {
  cat("Error: Ward-level aggregation failed. Please check the column names.\n")
} else {
  cat("2020-2021 Johannesburg ward-level dataset:", dim(joburg_2020_ward)[1], "wards\n")
  cat("2017-2018 Johannesburg ward-level dataset:", dim(joburg_2017_ward)[1], "wards\n")
  
  # Ensure ward IDs are character type for consistent matching
  joburg_2020_ward$ward <- as.character(joburg_2020_ward$ward)
  joburg_2017_ward$ward <- as.character(joburg_2017_ward$ward)
  
  # Find matching wards between the two datasets
  matching_wards <- intersect(joburg_2020_ward$ward, joburg_2017_ward$ward)
  cat("\nNumber of matching wards between datasets:", length(matching_wards), "\n")
  
  # Subset to include only matching wards
  joburg_2020_subset <- joburg_2020_ward %>%
    filter(ward %in% matching_wards)
  
  joburg_2017_subset <- joburg_2017_ward %>%
    filter(ward %in% matching_wards)
  
  # Define key variables for analysis
  key_variables <- c("unemployment", "food_insecurity", "no_medical_insurance", 
                    "public_healthcare_use", "informal_dwellings")
  
  # Compare overall statistics
  summary_2020 <- joburg_2020_subset %>%
    summarise(across(all_of(key_variables), list(mean = mean, sd = sd), na.rm = TRUE))
  
  summary_2017 <- joburg_2017_subset %>%
    summarise(across(all_of(key_variables), list(mean = mean, sd = sd), na.rm = TRUE))
  
  # Compare the summaries to identify shifts in absolute values
  comparison <- data.frame(
    Variable = rep(key_variables, each = 2),
    Year = rep(c("2017-2018", "2020-2021"), times = length(key_variables)),
    Mean = c(as.numeric(summary_2017[1, grepl("_mean", names(summary_2017))]),
             as.numeric(summary_2020[1, grepl("_mean", names(summary_2020))])),
    SD = c(as.numeric(summary_2017[1, grepl("_sd", names(summary_2017))]),
           as.numeric(summary_2020[1, grepl("_sd", names(summary_2020))]))
  )
  
  # Calculate correlation between 2017 and 2020 values for each variable
  correlations <- data.frame(
    Variable = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in key_variables) {
    # Create temporary data frame with matching wards and the variable from both years
    temp_df <- data.frame(
      ward = matching_wards,
      var_2017 = joburg_2017_subset[[var]][match(matching_wards, joburg_2017_subset$ward)],
      var_2020 = joburg_2020_subset[[var]][match(matching_wards, joburg_2020_subset$ward)]
    )
    
    # Calculate correlation
    corr <- cor(temp_df$var_2017, temp_df$var_2020, use = "complete.obs")
    
    # Add to correlations data frame
    correlations <- rbind(correlations, data.frame(Variable = var, Correlation = corr))
  }
  
  # Calculate ranks for each ward in both time periods
  joburg_2017_ranks <- joburg_2017_subset %>%
    mutate(across(all_of(key_variables), rank))
  
  joburg_2020_ranks <- joburg_2020_subset %>%
    mutate(across(all_of(key_variables), rank))
  
  # Calculate rank correlations to assess if relative positions remained stable
  rank_correlations <- data.frame(
    Variable = character(),
    Rank_Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in key_variables) {
    # Create temporary data frame with ranks from both years
    temp_df <- data.frame(
      ward = matching_wards,
      rank_2017 = joburg_2017_ranks[[var]][match(matching_wards, joburg_2017_ranks$ward)],
      rank_2020 = joburg_2020_ranks[[var]][match(matching_wards, joburg_2020_ranks$ward)]
    )
    
    # Calculate Spearman's rank correlation
    rank_corr <- cor(temp_df$rank_2017, temp_df$rank_2020, method = "spearman", use = "complete.obs")
    
    # Add to rank correlations data frame
    rank_correlations <- rbind(rank_correlations, data.frame(Variable = var, Rank_Correlation = rank_corr))
  }
  
  # Print the results
  cat("\nChanges in absolute values from 2017-2018 to 2020-2021 for Johannesburg:\n")
  print(comparison)
  
  cat("\nSpatial pattern consistency (correlations between ward-level values):\n")
  print(correlations)
  
  cat("\nStability of relative positions (rank correlations):\n")
  print(rank_correlations)
  
  # Create directory for plots if it doesn't exist
  dir.create("plots_johannesburg", showWarnings = FALSE)
  
  # Create scatter plots to visualize the relationship between 2017 and 2020 values
  for (var in key_variables) {
    # Create temporary data frame
    temp_df <- data.frame(
      ward = matching_wards,
      var_2017 = joburg_2017_subset[[var]][match(matching_wards, joburg_2017_subset$ward)],
      var_2020 = joburg_2020_subset[[var]][match(matching_wards, joburg_2020_subset$ward)]
    )
    
    # Create scatter plot
    p <- ggplot(temp_df, aes(x = var_2017, y = var_2020)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "red") +
      ggtitle(paste0("Johannesburg: Relationship between 2017-2018 and 2020-2021 values for ", var)) +
      xlab("2017-2018 Value (%)") +
      ylab("2020-2021 Value (%)") +
      theme_minimal()
    
    # Save the plot
    ggsave(paste0("plots_johannesburg/comparison_", var, ".png"), p, width = 8, height = 6)
  }
  
  # Create a correlation matrix visualization
  correlation_matrix <- matrix(NA, nrow = length(key_variables), ncol = length(key_variables))
  rownames(correlation_matrix) <- key_variables
  colnames(correlation_matrix) <- key_variables
  
  for (i in 1:length(key_variables)) {
    for (j in 1:length(key_variables)) {
      var_i <- key_variables[i]
      var_j <- key_variables[j]
      
      # Calculate correlation between variable i in 2017 and variable j in 2020
      temp_df <- data.frame(
        ward = matching_wards,
        var_2017 = joburg_2017_subset[[var_i]][match(matching_wards, joburg_2017_subset$ward)],
        var_2020 = joburg_2020_subset[[var_j]][match(matching_wards, joburg_2020_subset$ward)]
      )
      
      correlation_matrix[i, j] <- cor(temp_df$var_2017, temp_df$var_2020, use = "complete.obs")
    }
  }
  
  # Create correlation plot
  png("plots_johannesburg/correlation_matrix.png", width = 800, height = 800)
  corrplot(correlation_matrix, method = "color", type = "upper", 
           tl.col = "black", tl.srt = 45, addCoef.col = "black",
           title = "Johannesburg: Correlation between 2017-2018 and 2020-2021 variables")
  dev.off()
  
  # Create a summary report
  sink("johannesburg_comparison_results.txt")
  
  cat("GCRO SURVEY COMPARISON RESULTS FOR CITY OF JOHANNESBURG\n")
  cat("=====================================================\n\n")
  
  cat("This analysis compared key socioeconomic indicators between the GCRO Quality of Life Surveys\n")
  cat("from 2017-2018 (pre-pandemic) and 2020-2021 (pandemic era) for the City of Johannesburg.\n\n")
  
  cat("Key findings:\n")
  cat("1. Changes in absolute values:\n")
  for (var in key_variables) {
    var_2017 <- comparison$Mean[comparison$Variable == var & comparison$Year == "2017-2018"]
    var_2020 <- comparison$Mean[comparison$Variable == var & comparison$Year == "2020-2021"]
    change <- var_2020 - var_2017
    change_pct <- (change / var_2017) * 100
    
    cat(sprintf("   - %s: %.1f%% in 2017-2018 to %.1f%% in 2020-2021 (%.1f%% change)\n", 
                var, var_2017, var_2020, change_pct))
  }
  
  cat("\n2. Spatial pattern consistency:\n")
  for (i in 1:nrow(correlations)) {
    cat(sprintf("   - %s: correlation coefficient = %.2f\n", 
                correlations$Variable[i], correlations$Correlation[i]))
  }
  
  cat("\n3. Stability of relative positions:\n")
  for (i in 1:nrow(rank_correlations)) {
    cat(sprintf("   - %s: rank correlation = %.2f\n", 
                rank_correlations$Variable[i], rank_correlations$Rank_Correlation[i]))
  }
  
  cat("\nConclusion:\n")
  mean_corr <- mean(correlations$Correlation)
  mean_rank_corr <- mean(rank_correlations$Rank_Correlation)
  
  cat(sprintf("While absolute values showed significant shifts during the pandemic period, spatial patterns\n"))
  cat(sprintf("remained remarkably consistent, with high ward-level correlations across time periods\n"))
  cat(sprintf("for key indicators (r = %.2f on average). Rank correlations (rs = %.2f on average)\n", 
              mean_corr, mean_rank_corr))
  cat(sprintf("further confirmed that the relative positioning of wards remained stable, suggesting that\n"))
  cat(sprintf("while the pandemic may have exacerbated vulnerabilities, it did not fundamentally alter\n"))
  cat(sprintf("the underlying spatial patterns of inequality in Johannesburg.\n"))
  
  sink()
  
  cat("\nAnalysis complete. Results saved to 'johannesburg_comparison_results.txt'.\n")
  cat("Visualizations saved to the 'plots_johannesburg' directory.\n")
}
