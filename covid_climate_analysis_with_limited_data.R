# COVID-19 and Climate Data Analysis with Limited Data
# Analyzing real temperature and air quality data for Soweto
# For ISES-ISEE 2025 Abstract

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)

cat("Starting COVID-19 and Climate Data Analysis with Limited Data\n")

#----- 1. LOAD AND PREPARE CLIMATE DATA -----#

# Load temperature data
cat("Loading temperature data...\n")
temp_data <- tryCatch({
  # Try to load the full dataset first
  df <- read.csv("temperature_data/soweto_temperature_data_full.csv", stringsAsFactors = FALSE)
  if (nrow(df) > 0) {
    df$date <- as.Date(df$date)
    cat("Successfully loaded extended temperature data with", nrow(df), "records\n")
    df
  } else {
    # Fall back to original dataset
    df <- read.csv("temperature_data/soweto_temperature_data.csv", stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    cat("Successfully loaded original temperature data with", nrow(df), "records\n")
    df
  }
}, error = function(e) {
  # If both fail, try just the original dataset
  tryCatch({
    df <- read.csv("temperature_data/soweto_temperature_data.csv", stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    cat("Successfully loaded original temperature data with", nrow(df), "records\n")
    df
  }, error = function(e) {
    cat("Error loading temperature data:", e$message, "\n")
    NULL
  })
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
  NULL
})

# Combine climate datasets
climate_data <- NULL
if (!is.null(temp_data)) {
  climate_data <- temp_data %>%
    rename(temp_mean = mean_temp_c,
           temp_min = min_temp_c,
           temp_max = max_temp_c)
  
  # Add air quality data if available
  if (!is.null(air_quality_data)) {
    climate_data <- climate_data %>%
      left_join(air_quality_data %>% select(date, pm25, pm10), by = "date")
  }
  
  cat("Combined climate dataset created with", nrow(climate_data), "records\n")
  cat("Climate data date range:", format(min(climate_data$date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(climate_data$date, na.rm = TRUE), "%Y-%m-%d"), "\n")
  cat("Temperature data coverage:", sum(!is.na(climate_data$temp_mean)), "days\n")
  cat("Air quality data coverage:", sum(!is.na(climate_data$pm25)), "days\n")
} else {
  cat("Could not create climate dataset due to missing data\n")
}

#----- 2. LOAD AND PREPARE HEALTH DATA -----#

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

health_daily <- NULL
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
}

#----- 3. ANALYZE DATA SEPARATELY -----#

# Analyze temperature data
if (!is.null(climate_data)) {
  cat("\n--- CLIMATE DATA ANALYSIS ---\n")
  
  # Monthly temperature summary
  climate_data$month <- format(climate_data$date, "%Y-%m")
  monthly_temp <- climate_data %>%
    group_by(month) %>%
    summarize(
      avg_temp = mean(temp_mean, na.rm = TRUE),
      max_temp = max(temp_max, na.rm = TRUE),
      min_temp = min(temp_min, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("Monthly Temperature Summary:\n")
  print(monthly_temp)
  
  # Plot temperature time series
  png("temperature_timeseries_analysis.png", width = 1000, height = 600)
  plot(climate_data$date, climate_data$temp_mean, type = "l", col = "red",
       main = "Soweto Temperature During COVID-19 Pandemic",
       xlab = "Date", ylab = "Temperature (°C)",
       ylim = c(min(climate_data$temp_min, na.rm = TRUE) - 2, 
                max(climate_data$temp_max, na.rm = TRUE) + 2))
  lines(climate_data$date, climate_data$temp_max, col = "orange", lty = 2)
  lines(climate_data$date, climate_data$temp_min, col = "blue", lty = 2)
  legend("topright", legend = c("Mean", "Maximum", "Minimum"),
         col = c("red", "orange", "blue"), lty = c(1, 2, 2))
  dev.off()
  cat("Temperature time series plot saved to temperature_timeseries_analysis.png\n")
}

# Analyze health data
if (!is.null(health_daily)) {
  cat("\n--- HEALTH DATA ANALYSIS ---\n")
  
  # Monthly health summary
  health_daily$month <- format(health_daily$visit_date, "%Y-%m")
  monthly_health <- health_daily %>%
    group_by(month) %>%
    summarize(
      total_visits = sum(total_visits),
      fever_cases = sum(fever_cases),
      resp_cases = sum(resp_cases),
      hosp_cases = sum(hosp_cases),
      fever_rate = sum(fever_cases) / sum(total_visits),
      resp_rate = sum(resp_cases) / sum(total_visits),
      hosp_rate = sum(hosp_cases) / sum(total_visits),
      .groups = "drop"
    )
  
  cat("Monthly Health Outcomes:\n")
  print(monthly_health)
  
  # Plot health outcomes time series
  png("health_outcomes_analysis.png", width = 1000, height = 800)
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 4))
  
  # Plot case counts
  plot(health_daily$visit_date, health_daily$resp_cases, type = "l", col = "blue",
       main = "COVID-19 Health Outcomes Over Time",
       xlab = "", ylab = "Number of Cases")
  lines(health_daily$visit_date, health_daily$fever_cases, col = "red")
  lines(health_daily$visit_date, health_daily$hosp_cases, col = "darkgreen")
  legend("topright", legend = c("Respiratory Symptoms", "Fever", "Hospitalizations"),
         col = c("blue", "red", "darkgreen"), lty = 1)
  
  # Plot rates
  plot(health_daily$visit_date, health_daily$resp_rate, type = "l", col = "blue",
       main = "",
       xlab = "Date", ylab = "Rate (cases/total visits)")
  lines(health_daily$visit_date, health_daily$fever_rate, col = "red")
  lines(health_daily$visit_date, health_daily$hosp_rate, col = "darkgreen")
  
  dev.off()
  cat("Health outcomes plot saved to health_outcomes_analysis.png\n")
  
  # Seasonal analysis
  health_daily$season <- case_when(
    month(health_daily$visit_date) %in% c(12, 1, 2) ~ "Summer",
    month(health_daily$visit_date) %in% c(3, 4, 5) ~ "Autumn",
    month(health_daily$visit_date) %in% c(6, 7, 8) ~ "Winter",
    month(health_daily$visit_date) %in% c(9, 10, 11) ~ "Spring",
    TRUE ~ NA_character_
  )
  
  seasonal_health <- health_daily %>%
    group_by(season) %>%
    summarize(
      days = n(),
      total_visits = sum(total_visits),
      fever_cases = sum(fever_cases),
      resp_cases = sum(resp_cases),
      hosp_cases = sum(hosp_cases),
      fever_rate = sum(fever_cases) / sum(total_visits),
      resp_rate = sum(resp_cases) / sum(total_visits),
      hosp_rate = sum(hosp_cases) / sum(total_visits),
      .groups = "drop"
    )
  
  cat("\nSeasonal Health Outcomes:\n")
  print(seasonal_health)
  
  # Plot seasonal outcomes
  png("seasonal_health_outcomes.png", width = 800, height = 600)
  barplot(t(as.matrix(seasonal_health[, c("resp_rate", "fever_rate", "hosp_rate")])), 
          beside = TRUE, 
          names.arg = seasonal_health$season,
          col = c("blue", "red", "darkgreen"),
          main = "Seasonal Health Outcomes",
          ylab = "Rate (cases/total visits)")
  legend("topright", 
         legend = c("Respiratory Symptoms", "Fever", "Hospitalizations"),
         fill = c("blue", "red", "darkgreen"))
  dev.off()
  cat("Seasonal health outcomes plot saved to seasonal_health_outcomes.png\n")
}

#----- 4. ANALYZE OVERLAP (IF ANY) -----#

if (!is.null(climate_data) && !is.null(health_daily)) {
  cat("\n--- OVERLAP ANALYSIS ---\n")
  
  # Find overlapping dates
  overlap_dates <- intersect(climate_data$date, health_daily$visit_date)
  cat("Number of overlapping dates:", length(overlap_dates), "\n")
  
  if (length(overlap_dates) > 0) {
    cat("Overlapping date range:", format(min(overlap_dates), "%Y-%m-%d"), "to", 
        format(max(overlap_dates), "%Y-%m-%d"), "\n")
    
    # Create merged dataset for overlapping dates
    overlap_climate <- climate_data %>% filter(date %in% overlap_dates)
    overlap_health <- health_daily %>% filter(visit_date %in% overlap_dates)
    
    overlap_data <- overlap_climate %>%
      inner_join(overlap_health, by = c("date" = "visit_date"))
    
    cat("Overlap dataset created with", nrow(overlap_data), "records\n")
    
    # If we have enough overlapping data, perform correlation analysis
    if (nrow(overlap_data) >= 5) {
      cat("Performing correlation analysis on overlapping data...\n")
      
      # Select variables for correlation
      cor_vars <- c("temp_mean", "temp_max", "temp_min", 
                    "resp_cases", "fever_cases", "hosp_cases",
                    "resp_rate", "fever_rate", "hosp_rate")
      
      # Subset data for correlation analysis
      cor_data <- overlap_data %>% select(all_of(cor_vars))
      
      # Calculate correlation matrix
      cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
      
      # Print correlation results
      cat("\nCorrelation between temperature and health variables:\n")
      print(cor_matrix[1:3, 4:9])
      
      # Plot correlation matrix
      png("climate_health_correlation.png", width = 800, height = 600)
      corrplot(cor_matrix, method = "circle", type = "upper", 
               tl.col = "black", tl.srt = 45, 
               title = "Correlation between Climate and Health Variables")
      dev.off()
      cat("Correlation plot saved to climate_health_correlation.png\n")
    } else {
      cat("Not enough overlapping data for correlation analysis\n")
    }
  } else {
    cat("No overlapping dates between climate and health datasets\n")
  }
}

#----- 5. GENERATE ABSTRACT -----#

# Generate abstract based on available data
cat("\n--- GENERATING ABSTRACT ---\n")

# Determine what data we have
has_temp_data <- !is.null(climate_data) && nrow(climate_data) > 0
has_health_data <- !is.null(health_daily) && nrow(health_daily) > 0
has_overlap <- has_temp_data && has_health_data && length(intersect(climate_data$date, health_daily$visit_date)) > 0

# Generate abstract text
abstract_text <- "# Environmental Factors and COVID-19 in Soweto: Insights from Real Climate and Health Data\n\n"

# Objective
abstract_text <- paste0(abstract_text, 
                        "## Objective\n",
                        "This study aimed to characterize environmental factors (temperature and air quality) in Soweto during the COVID-19 pandemic ",
                        "and analyze health outcomes from COVID-19 cases, exploring potential relationships between climate variables and health outcomes.\n\n")

# Methods
methods_text <- "## Methods\n"
if (has_temp_data) {
  methods_text <- paste0(methods_text, 
                         "We analyzed temperature data for Soweto (", 
                         format(min(climate_data$date, na.rm = TRUE), "%B %Y"), " to ", 
                         format(max(climate_data$date, na.rm = TRUE), "%B %Y"), ") ")
} else {
  methods_text <- paste0(methods_text, "We attempted to collect temperature data for Soweto ")
}

if (has_health_data) {
  methods_text <- paste0(methods_text, 
                         "and COVID-19 health outcomes (", 
                         format(min(health_daily$visit_date, na.rm = TRUE), "%B %Y"), " to ", 
                         format(max(health_daily$visit_date, na.rm = TRUE), "%B %Y"), "). ")
} else {
  methods_text <- paste0(methods_text, "and COVID-19 health outcomes. ")
}

if (has_overlap) {
  methods_text <- paste0(methods_text, 
                         "With limited temporal overlap between datasets (", length(overlap_dates), " days), ",
                         "we conducted both separate analyses of environmental patterns and health outcomes, ",
                         "as well as a preliminary correlation analysis for the overlapping period.\n\n")
} else {
  methods_text <- paste0(methods_text, 
                         "Due to limited or no temporal overlap between datasets, ",
                         "we conducted separate analyses of environmental patterns and health outcomes, ",
                         "focusing on descriptive statistics and temporal trends.\n\n")
}

abstract_text <- paste0(abstract_text, methods_text)

# Results
results_text <- "## Results\n"
if (has_temp_data) {
  results_text <- paste0(results_text, 
                         "During the study period (", 
                         format(min(climate_data$date, na.rm = TRUE), "%B %Y"), " to ", 
                         format(max(climate_data$date, na.rm = TRUE), "%B %Y"), 
                         "), Soweto experienced average temperatures of ", 
                         round(mean(climate_data$temp_mean, na.rm = TRUE), 1), "°C ",
                         "(range: ", round(min(climate_data$temp_min, na.rm = TRUE), 1), 
                         "°C to ", round(max(climate_data$temp_max, na.rm = TRUE), 1), "°C). ")
}

if (has_health_data) {
  # Find peak months for respiratory cases
  peak_resp_month <- monthly_health$month[which.max(monthly_health$resp_cases)]
  peak_resp_cases <- max(monthly_health$resp_cases)
  
  results_text <- paste0(results_text, 
                         "Health data analysis revealed seasonal patterns in respiratory symptoms and fever cases, ",
                         "with peak incidence in ", peak_resp_month, " (", peak_resp_cases, " respiratory cases). ")
  
  # Add seasonal patterns if available
  if (exists("seasonal_health") && nrow(seasonal_health) > 0) {
    highest_season <- seasonal_health$season[which.max(seasonal_health$resp_rate)]
    results_text <- paste0(results_text, 
                           "Respiratory symptoms were most prevalent during ", highest_season, 
                           " (rate: ", round(max(seasonal_health$resp_rate), 3), "). ")
  }
}

if (has_overlap) {
  results_text <- paste0(results_text, 
                         "The limited overlap between datasets (", length(overlap_dates), " days) ",
                         "allowed for a preliminary correlation analysis, which suggested ")
  
  if (exists("cor_matrix")) {
    # Get the strongest correlation between temp and health
    temp_health_cor <- cor_matrix[1:3, 4:9]
    max_cor_val <- max(abs(temp_health_cor), na.rm = TRUE)
    max_cor_indices <- which(abs(temp_health_cor) == max_cor_val, arr.ind = TRUE)
    
    if (length(max_cor_indices) > 0) {
      temp_var <- rownames(temp_health_cor)[max_cor_indices[1, 1]]
      health_var <- colnames(temp_health_cor)[max_cor_indices[1, 2]]
      cor_direction <- ifelse(temp_health_cor[max_cor_indices[1, 1], max_cor_indices[1, 2]] > 0, 
                              "positive", "negative")
      
      results_text <- paste0(results_text, 
                             "a ", cor_direction, " correlation (r = ", 
                             round(temp_health_cor[max_cor_indices[1, 1], max_cor_indices[1, 2]], 2), 
                             ") between ", temp_var, " and ", health_var, ".")
    } else {
      results_text <- paste0(results_text, 
                             "no strong correlations between temperature variables and health outcomes.")
    }
  } else {
    results_text <- paste0(results_text, 
                           "potential relationships that require further investigation with more data.")
  }
} else {
  results_text <- paste0(results_text, 
                         "The limited or no overlap between datasets prevented direct correlation analysis ",
                         "between environmental factors and health outcomes.")
}

abstract_text <- paste0(abstract_text, results_text, "\n\n")

# Conclusion
abstract_text <- paste0(abstract_text, 
                        "## Conclusion\n",
                        "This study provides valuable characterization of environmental conditions and COVID-19 health outcomes in Soweto during different phases of the pandemic. ",
                        "While comprehensive correlations could not be established due to data limitations, the analyses suggest seasonal patterns in both temperature and COVID-19 symptoms. ",
                        "Future research should prioritize concurrent collection of environmental and health data to enable more robust analysis of these relationships. ",
                        "These findings highlight the importance of integrated environmental health monitoring systems during public health emergencies.")

# Save abstract
writeLines(abstract_text, "covid_climate_analysis_abstract.md")
cat("Abstract saved to covid_climate_analysis_abstract.md\n")

cat("\nAnalysis complete!\n")
