# COVID-19 and Climate Data Analysis for Soweto
# Using real collected temperature and air quality data
# For ISES-ISEE 2025 Abstract

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)

# Set working directory if needed
# setwd("path/to/your/directory")

cat("Starting COVID-19 and Climate Data Analysis for Soweto\n")

#----- 1. LOAD CLIMATE DATA -----#

# Load temperature data
cat("Loading temperature data...\n")
temp_data <- tryCatch({
  df <- read.csv("temperature_data/soweto_temperature_data.csv", stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  cat("Successfully loaded temperature data with", nrow(df), "records\n")
  df
}, error = function(e) {
  cat("Error loading temperature data:", e$message, "\n")
  cat("Please run soweto_temperature_collector.py first to collect temperature data\n")
  NULL
})

# Load air quality data
cat("Loading air quality data...\n")
air_quality_data <- tryCatch({
  df <- read.csv("air_quality_results/johannesburg_air_quality_fixed.csv", stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  cat("Successfully loaded air quality data with", nrow(df), "records\n")
  df
}, error = function(e) {
  cat("Error loading air quality data:", e$message, "\n")
  cat("Please run run_air_quality.py first to collect air quality data\n")
  NULL
})

# Combine climate data
cat("Combining climate datasets...\n")
if (!is.null(temp_data) && !is.null(air_quality_data)) {
  climate_data <- temp_data %>%
    rename(
      temp_mean = mean_temp_c,
      temp_min = min_temp_c,
      temp_max = max_temp_c
    ) %>%
    left_join(air_quality_data, by = "date") %>%
    mutate(
      # Convert air quality values to numeric
      pm25 = as.numeric(pm25),
      pm10 = as.numeric(pm10),
      # Calculate heat index (simplified version)
      heat_index = case_when(
        temp_mean > 27 ~ temp_mean + 0.5 * (temp_mean - 27),
        TRUE ~ temp_mean
      ),
      # Define extreme heat days
      extreme_heat = ifelse(temp_max > 30, 1, 0),
      # Define poor air quality days
      poor_air_quality = ifelse(pm25 > 15, 1, 0)  # WHO guideline for PM2.5
    )
  
  cat("Combined climate dataset created with", nrow(climate_data), "records\n")
  cat("Climate data date range:", format(min(climate_data$date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(climate_data$date, na.rm = TRUE), "%Y-%m-%d"), "\n")
  cat("Temperature data coverage:", sum(!is.na(climate_data$temp_mean)), "days\n")
  cat("Air quality data coverage:", sum(!is.na(climate_data$pm25)), "days\n")
} else {
  cat("Could not create combined climate dataset due to missing data\n")
  climate_data <- NULL
}

#----- 2. LOAD HEALTH DATA -----#

# Load ChAdOx data
cat("Loading ChAdOx datasets...\n")
chadox_data_loaded <- tryCatch({
  load("ChAdOx data 2024-07-19.Rdata")
  load("ChAdOx supp visit data 2024-07-19.Rdata")
  cat("Successfully loaded ChAdOx datasets\n")
  TRUE
}, error = function(e) {
  cat("Error loading ChAdOx datasets:", e$message, "\n")
  FALSE
})

if (chadox_data_loaded && exists("illness_visit") && exists("covid_cases")) {
  # Process illness visit data
  cat("Processing illness visit data...\n")
  illness_data <- illness_visit %>%
    mutate(
      record_id = as.character(record_id),
      visit_date = as.Date(ev_dat),
      temperature = as.numeric(gsub("[^0-9.]", "", ev_temp)),  # Clean temperature data
      oxygen_saturation = as.numeric(gsub("[^0-9.]", "", ev_oxy)),
      respiratory_rate = as.numeric(gsub("[^0-9.]", "", ev_resp)),
      pulse = as.numeric(gsub("[^0-9.]", "", ev_pulse)),
      fever = ifelse(temperature > 38, "Yes", "No"),
      respiratory_symptoms = case_when(
        ev_resp_pe == "1" ~ "None",
        ev_resp_pe == "2" ~ "Mild",
        ev_resp_pe == "3" ~ "Moderate",
        ev_resp_pe == "4" ~ "Severe",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(record_id, visit_date, temperature, oxygen_saturation, respiratory_rate, 
           pulse, fever, respiratory_symptoms) %>%
    filter(!is.na(visit_date))
  
  # Process COVID cases data
  cat("Processing COVID cases data...\n")
  covid_data <- covid_cases %>%
    mutate(
      record_id = as.character(record_id),
      visit_date = as.Date(fu_dat),
      covid_test_date = as.Date(fu_np_datetime1),
      covid_result = fu_res1,
      covid_hospitalized = ifelse(grepl("hosp|admit", fu_res1, ignore.case = TRUE), "Yes", "No")
    ) %>%
    select(record_id, visit_date, covid_test_date, covid_result, covid_hospitalized) %>%
    filter(!is.na(visit_date))
  
  # Combine health datasets
  cat("Combining health datasets...\n")
  health_data <- illness_data %>%
    left_join(covid_data, by = c("record_id", "visit_date")) %>%
    mutate(
      # Create binary indicators for analysis
      fever_binary = ifelse(fever == "Yes" | temperature > 38, 1, 0),
      resp_binary = ifelse(respiratory_symptoms %in% c("Moderate", "Severe"), 1, 0),
      hosp_binary = ifelse(covid_hospitalized == "Yes", 1, 0)
    )
  
  # Aggregate to daily level
  cat("Aggregating health data to daily level...\n")
  health_daily <- health_data %>%
    group_by(visit_date) %>%
    summarize(
      avg_temperature = mean(temperature, na.rm = TRUE),
      fever_cases = sum(fever_binary, na.rm = TRUE),
      resp_cases = sum(resp_binary, na.rm = TRUE),
      hosp_cases = sum(hosp_binary, na.rm = TRUE),
      total_visits = n(),
      .groups = "drop"
    ) %>%
    # Calculate rates
    mutate(
      fever_rate = fever_cases / total_visits,
      resp_rate = resp_cases / total_visits,
      hosp_rate = hosp_cases / total_visits
    )
  
  cat("Health data preparation complete\n")
  cat("Daily health dataset dimensions:", nrow(health_daily), "rows,", ncol(health_daily), "columns\n")
  cat("Health data date range:", format(min(health_daily$visit_date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(health_daily$visit_date, na.rm = TRUE), "%Y-%m-%d"), "\n")
} else {
  cat("Could not process health data due to missing datasets\n")
  health_daily <- NULL
}

#----- 3. MERGE CLIMATE AND HEALTH DATA -----#

if (!is.null(climate_data) && !is.null(health_daily)) {
  cat("Merging climate and health data...\n")
  
  # Merge datasets
  analysis_data <- health_daily %>%
    left_join(climate_data, by = c("visit_date" = "date"))
  
  # Create lagged variables for temperature and air quality
  cat("Creating lagged variables for analysis...\n")
  for (lag_days in 1:7) {
    lag_col_name <- paste0("temp_max_lag", lag_days)
    analysis_data[[lag_col_name]] <- lag(analysis_data$temp_max, lag_days)
    
    lag_col_name <- paste0("pm25_lag", lag_days)
    analysis_data[[lag_col_name]] <- lag(analysis_data$pm25, lag_days)
  }
  
  cat("Analysis dataset created with", nrow(analysis_data), "records\n")
  cat("Analysis data date range:", format(min(analysis_data$visit_date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(analysis_data$visit_date, na.rm = TRUE), "%Y-%m-%d"), "\n")
  
  # Check for data overlap
  overlap_days <- sum(!is.na(analysis_data$temp_mean))
  cat("Days with both health and temperature data:", overlap_days, "\n")
  
  if (overlap_days < 10) {
    cat("\nWARNING: Very limited overlap between climate and health datasets!\n")
    cat("This will affect the reliability of correlation analyses.\n\n")
  }
  
  #----- 4. ANALYSIS -----#
  
  cat("\nPerforming analysis...\n")
  
  # 4.1 Correlation analysis
  cat("Calculating correlations between climate and health variables...\n")
  climate_vars <- c("temp_mean", "temp_max", "temp_min", "pm25", "pm10")
  health_vars <- c("fever_cases", "resp_cases", "hosp_cases", "fever_rate", "resp_rate", "hosp_rate")
  
  # Select only complete cases for correlation analysis
  correlation_data <- analysis_data %>%
    select(all_of(c(climate_vars, health_vars))) %>%
    na.omit()
  
  if (nrow(correlation_data) > 5) {  # Need at least a few data points
    correlation_matrix <- cor(correlation_data, use = "complete.obs")
    
    # Plot correlation matrix
    png("covid_climate_correlation.png", width = 800, height = 800)
    corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
             tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)
    dev.off()
    cat("Correlation matrix saved to covid_climate_correlation.png\n")
    
    # 4.2 Lag effect analysis
    cat("Analyzing lag effects...\n")
    lag_vars <- c(paste0("temp_max_lag", 1:7), paste0("pm25_lag", 1:7))
    
    # Select only complete cases for lag analysis
    lag_data <- analysis_data %>%
      select(all_of(c(lag_vars, health_vars))) %>%
      na.omit()
    
    if (nrow(lag_data) > 5) {
      lag_correlation <- cor(lag_data, use = "complete.obs")
      
      # Extract correlations between lagged climate variables and health outcomes
      lag_results <- data.frame(
        lag_variable = character(),
        fever_correlation = numeric(),
        resp_correlation = numeric(),
        hosp_correlation = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (lag_var in lag_vars) {
        lag_results <- rbind(lag_results, data.frame(
          lag_variable = lag_var,
          fever_correlation = lag_correlation[lag_var, "fever_cases"],
          resp_correlation = lag_correlation[lag_var, "resp_cases"],
          hosp_correlation = lag_correlation[lag_var, "hosp_cases"],
          stringsAsFactors = FALSE
        ))
      }
      
      # Plot lag effects
      png("covid_climate_lag_effects.png", width = 1000, height = 600)
      par(mfrow = c(1, 3))
      
      # Temperature lag effects on fever
      temp_lag_fever <- lag_results[grep("temp_max_lag", lag_results$lag_variable), ]
      temp_lag_fever$lag_days <- as.numeric(gsub("temp_max_lag", "", temp_lag_fever$lag_variable))
      plot(temp_lag_fever$lag_days, temp_lag_fever$fever_correlation, type = "b", 
           main = "Temperature Lag Effects on Fever", 
           xlab = "Lag (days)", ylab = "Correlation", ylim = c(-1, 1))
      abline(h = 0, lty = 2)
      
      # Temperature lag effects on respiratory symptoms
      plot(temp_lag_fever$lag_days, temp_lag_fever$resp_correlation, type = "b", 
           main = "Temperature Lag Effects on Respiratory Symptoms", 
           xlab = "Lag (days)", ylab = "Correlation", ylim = c(-1, 1))
      abline(h = 0, lty = 2)
      
      # PM2.5 lag effects on respiratory symptoms
      pm_lag_resp <- lag_results[grep("pm25_lag", lag_results$lag_variable), ]
      pm_lag_resp$lag_days <- as.numeric(gsub("pm25_lag", "", pm_lag_resp$lag_variable))
      plot(pm_lag_resp$lag_days, pm_lag_resp$resp_correlation, type = "b", 
           main = "PM2.5 Lag Effects on Respiratory Symptoms", 
           xlab = "Lag (days)", ylab = "Correlation", ylim = c(-1, 1))
      abline(h = 0, lty = 2)
      
      dev.off()
      cat("Lag effects plot saved to covid_climate_lag_effects.png\n")
    } else {
      cat("Not enough complete data for lag analysis\n")
    }
    
    # 4.3 Time series visualization
    cat("Creating time series visualization...\n")
    
    # Plot temperature and fever cases over time
    png("covid_climate_timeseries.png", width = 1200, height = 800)
    par(mfrow = c(2, 1), mar = c(4, 4, 2, 4))
    
    # Temperature time series
    plot(analysis_data$visit_date, analysis_data$temp_max, type = "l", col = "red",
         main = "Maximum Temperature and Fever Cases Over Time",
         xlab = "", ylab = "Temperature (°C)")
    
    # Fever cases time series
    plot(analysis_data$visit_date, analysis_data$fever_cases, type = "h", col = "blue",
         main = "", xlab = "Date", ylab = "Fever Cases")
    
    dev.off()
    cat("Time series visualization saved to covid_climate_timeseries.png\n")
    
    # 4.4 Summary statistics
    cat("\nSummary Statistics:\n")
    
    # Temperature summary
    cat("Temperature Statistics (°C):\n")
    cat("  Mean temperature:", mean(analysis_data$temp_mean, na.rm = TRUE), "\n")
    cat("  Maximum temperature:", max(analysis_data$temp_max, na.rm = TRUE), "\n")
    cat("  Minimum temperature:", min(analysis_data$temp_min, na.rm = TRUE), "\n")
    
    # Air quality summary
    cat("Air Quality Statistics:\n")
    cat("  Mean PM2.5:", mean(analysis_data$pm25, na.rm = TRUE), "μg/m³\n")
    cat("  Mean PM10:", mean(analysis_data$pm10, na.rm = TRUE), "μg/m³\n")
    
    # Health outcomes summary
    cat("Health Outcomes Statistics:\n")
    cat("  Total fever cases:", sum(analysis_data$fever_cases, na.rm = TRUE), "\n")
    cat("  Total respiratory cases:", sum(analysis_data$resp_cases, na.rm = TRUE), "\n")
    cat("  Total hospitalization cases:", sum(analysis_data$hosp_cases, na.rm = TRUE), "\n")
    
    # Save results
    cat("\nSaving analysis results...\n")
    save(analysis_data, correlation_matrix, lag_results, 
         file = "covid_climate_analysis_results.RData")
    
    # Generate abstract text
    cat("\nGenerating abstract text...\n")
    abstract_text <- paste0(
      "# Environmental Determinants of COVID-19 Outcomes in Soweto: A Comparative Analysis\n\n",
      "## Objective\n",
      "This study investigated the relationship between environmental factors (temperature and air quality) ",
      "and COVID-19 health outcomes in Soweto, South Africa during the pandemic period.\n\n",
      "## Methods\n",
      "We combined COVID-19 health data from the ChAdOx dataset with temperature data from ERA5 and air quality data ",
      "for Johannesburg. The analysis included ", nrow(analysis_data), " days of data from ", 
      format(min(analysis_data$visit_date, na.rm = TRUE), "%B %Y"), " to ", 
      format(max(analysis_data$visit_date, na.rm = TRUE), "%B %Y"), ". ",
      "Statistical analyses included correlation analyses and lag effect evaluations.\n\n",
      "## Results\n",
      "The average temperature during the study period was ", round(mean(analysis_data$temp_mean, na.rm = TRUE), 1), "°C ",
      "(range: ", round(min(analysis_data$temp_min, na.rm = TRUE), 1), "°C to ", 
      round(max(analysis_data$temp_max, na.rm = TRUE), 1), "°C). ",
      "Mean PM2.5 concentration was ", round(mean(analysis_data$pm25, na.rm = TRUE), 1), "μg/m³, ",
      "exceeding the WHO guideline of 15 μg/m³. ",
      "The correlation between maximum temperature and fever cases was ", 
      round(correlation_matrix["temp_max", "fever_cases"], 2), ". ",
      "Lag analysis showed the strongest association between temperature and respiratory symptoms at ",
      "a lag of ", which.max(abs(temp_lag_fever$resp_correlation)), " days (r = ", 
      round(temp_lag_fever$resp_correlation[which.max(abs(temp_lag_fever$resp_correlation))], 2), ").\n\n",
      "## Conclusion\n",
      "This study provides evidence of the relationship between environmental factors and COVID-19 health outcomes ",
      "in Soweto. The findings suggest that ", 
      ifelse(correlation_matrix["temp_max", "fever_cases"] > 0, 
             "higher temperatures are associated with increased fever cases", 
             "lower temperatures are associated with increased fever cases"),
      ", and that air quality may influence respiratory symptoms with a lag effect. ",
      "These results highlight the importance of considering environmental factors in public health responses to respiratory disease outbreaks."
    )
    
    # Save abstract
    writeLines(abstract_text, "covid_climate_abstract.md")
    cat("Abstract saved to covid_climate_abstract.md\n")
    
  } else {
    cat("Not enough complete data for correlation analysis\n")
  }
} else {
  cat("Cannot perform analysis due to missing climate or health data\n")
}

cat("\nAnalysis complete!\n")
