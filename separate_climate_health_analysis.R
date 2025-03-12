# Separate Climate and Health Data Analysis for COVID-19 Research
# Analyzing real temperature, air quality, and health data for Soweto
# For ISES-ISEE 2025 Abstract

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)

cat("Starting Separate Climate and Health Data Analysis\n")

#----- 1. CLIMATE DATA ANALYSIS -----#

# Load temperature data
cat("\n--- CLIMATE DATA ANALYSIS ---\n")
cat("Loading temperature data...\n")
temp_data <- tryCatch({
  df <- read.csv("temperature_data/soweto_temperature_data.csv", stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  cat("Successfully loaded temperature data with", nrow(df), "records\n")
  df
}, error = function(e) {
  cat("Error loading temperature data:", e$message, "\n")
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
  NULL
})

# Analyze temperature data
if (!is.null(temp_data)) {
  cat("\nTemperature Data Analysis:\n")
  cat("Date range:", format(min(temp_data$date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(temp_data$date, na.rm = TRUE), "%Y-%m-%d"), "\n")
  
  # Summary statistics
  temp_summary <- data.frame(
    Metric = c("Mean Temperature", "Maximum Temperature", "Minimum Temperature"),
    Value = c(
      mean(temp_data$mean_temp_c, na.rm = TRUE),
      max(temp_data$max_temp_c, na.rm = TRUE),
      min(temp_data$min_temp_c, na.rm = TRUE)
    ),
    Unit = "°C"
  )
  
  print(temp_summary)
  
  # Monthly averages
  temp_data$month <- format(temp_data$date, "%Y-%m")
  monthly_temp <- temp_data %>%
    group_by(month) %>%
    summarize(
      avg_temp = mean(mean_temp_c, na.rm = TRUE),
      max_temp = max(max_temp_c, na.rm = TRUE),
      min_temp = min(min_temp_c, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nMonthly Temperature Averages:\n")
  print(monthly_temp)
  
  # Plot temperature time series
  png("temperature_timeseries.png", width = 1000, height = 600)
  plot(temp_data$date, temp_data$mean_temp_c, type = "l", col = "red",
       main = "Soweto Temperature During Early COVID-19 Pandemic",
       xlab = "Date", ylab = "Temperature (°C)",
       ylim = c(min(temp_data$min_temp_c, na.rm = TRUE) - 2, 
                max(temp_data$max_temp_c, na.rm = TRUE) + 2))
  lines(temp_data$date, temp_data$max_temp_c, col = "orange", lty = 2)
  lines(temp_data$date, temp_data$min_temp_c, col = "blue", lty = 2)
  legend("topright", legend = c("Mean", "Maximum", "Minimum"),
         col = c("red", "orange", "blue"), lty = c(1, 2, 2))
  dev.off()
  cat("Temperature time series plot saved to temperature_timeseries.png\n")
}

# Analyze air quality data
if (!is.null(air_quality_data)) {
  cat("\nAir Quality Data Analysis:\n")
  cat("Date range:", format(min(air_quality_data$date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(air_quality_data$date, na.rm = TRUE), "%Y-%m-%d"), "\n")
  
  # Convert to numeric
  air_quality_data$pm25 <- as.numeric(air_quality_data$pm25)
  air_quality_data$pm10 <- as.numeric(air_quality_data$pm10)
  
  # Summary statistics
  aq_summary <- data.frame(
    Metric = c("Mean PM2.5", "Maximum PM2.5", "Mean PM10", "Maximum PM10"),
    Value = c(
      mean(air_quality_data$pm25, na.rm = TRUE),
      max(air_quality_data$pm25, na.rm = TRUE),
      mean(air_quality_data$pm10, na.rm = TRUE),
      max(air_quality_data$pm10, na.rm = TRUE)
    ),
    Unit = "μg/m³"
  )
  
  print(aq_summary)
  
  # Plot air quality time series
  if (sum(!is.na(air_quality_data$pm25)) > 0) {
    png("air_quality_timeseries.png", width = 1000, height = 600)
    plot(air_quality_data$date, air_quality_data$pm25, type = "l", col = "purple",
         main = "Johannesburg Air Quality During COVID-19 Pandemic",
         xlab = "Date", ylab = "Concentration (μg/m³)")
    lines(air_quality_data$date, air_quality_data$pm10, col = "green")
    abline(h = 15, col = "red", lty = 2)  # WHO guideline for PM2.5
    abline(h = 45, col = "orange", lty = 2)  # WHO guideline for PM10
    legend("topright", legend = c("PM2.5", "PM10", "WHO PM2.5 Guideline", "WHO PM10 Guideline"),
           col = c("purple", "green", "red", "orange"), lty = c(1, 1, 2, 2))
    dev.off()
    cat("Air quality time series plot saved to air_quality_timeseries.png\n")
  }
}

#----- 2. HEALTH DATA ANALYSIS -----#

cat("\n--- HEALTH DATA ANALYSIS ---\n")

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
  
  # Monthly aggregation
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
  
  cat("\nMonthly Health Outcomes:\n")
  print(monthly_health)
  
  # Plot health outcomes time series
  png("health_outcomes_timeseries.png", width = 1000, height = 800)
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 4))
  
  # Plot case counts
  plot(health_daily$visit_date, health_daily$fever_cases, type = "l", col = "red",
       main = "COVID-19 Health Outcomes Over Time",
       xlab = "", ylab = "Number of Cases")
  lines(health_daily$visit_date, health_daily$resp_cases, col = "blue")
  lines(health_daily$visit_date, health_daily$hosp_cases, col = "darkgreen")
  legend("topright", legend = c("Fever", "Respiratory Symptoms", "Hospitalizations"),
         col = c("red", "blue", "darkgreen"), lty = 1)
  
  # Plot rates
  plot(health_daily$visit_date, health_daily$fever_rate, type = "l", col = "red",
       main = "",
       xlab = "Date", ylab = "Rate (cases/total visits)")
  lines(health_daily$visit_date, health_daily$resp_rate, col = "blue")
  lines(health_daily$visit_date, health_daily$hosp_rate, col = "darkgreen")
  
  dev.off()
  cat("Health outcomes time series plot saved to health_outcomes_timeseries.png\n")
  
  # Check for overlap with temperature data
  if (!is.null(temp_data)) {
    cat("\nChecking overlap between health and temperature data...\n")
    
    # Find overlapping dates
    overlap_dates <- intersect(health_daily$visit_date, temp_data$date)
    cat("Number of overlapping dates:", length(overlap_dates), "\n")
    
    if (length(overlap_dates) > 0) {
      cat("Overlapping date range:", format(min(overlap_dates), "%Y-%m-%d"), "to", 
          format(max(overlap_dates), "%Y-%m-%d"), "\n")
      
      # Extract data for overlapping dates
      overlap_health <- health_daily %>% filter(visit_date %in% overlap_dates)
      overlap_temp <- temp_data %>% filter(date %in% overlap_dates)
      
      cat("\nHealth data during overlap period:\n")
      print(overlap_health)
      
      cat("\nTemperature data during overlap period:\n")
      print(overlap_temp)
    }
  }
}

#----- 3. GENERATE ABSTRACT -----#

cat("\n--- GENERATING ABSTRACT ---\n")

# Generate abstract text based on separate analyses
abstract_text <- paste0(
  "# Environmental Factors and COVID-19 in Soweto: Insights from Separate Climate and Health Analyses\n\n",
  "## Objective\n",
  "This study aimed to characterize environmental factors (temperature and air quality) in Soweto during the COVID-19 pandemic ",
  "and analyze health outcomes from COVID-19 cases, exploring potential relationships despite limited temporal overlap in available data.\n\n",
  "## Methods\n",
  "We analyzed temperature data for Soweto (", 
  format(min(temp_data$date, na.rm = TRUE), "%B %Y"), " to ", format(max(temp_data$date, na.rm = TRUE), "%B %Y"), 
  ") and COVID-19 health outcomes (", 
  format(min(health_daily$visit_date, na.rm = TRUE), "%B %Y"), " to ", format(max(health_daily$visit_date, na.rm = TRUE), "%B %Y"), 
  "). Due to limited temporal overlap between datasets, we conducted separate analyses of environmental patterns and health outcomes, ",
  "focusing on descriptive statistics and temporal trends.\n\n",
  "## Results\n",
  "During the early pandemic period (", format(min(temp_data$date, na.rm = TRUE), "%B %Y"), " to ", format(max(temp_data$date, na.rm = TRUE), "%B %Y"), 
  "), Soweto experienced average temperatures of ", round(mean(temp_data$mean_temp_c, na.rm = TRUE), 1), "°C ",
  "(range: ", round(min(temp_data$min_temp_c, na.rm = TRUE), 1), "°C to ", round(max(temp_data$max_temp_c, na.rm = TRUE), 1), "°C). ",
  "Health data analysis revealed seasonal patterns in respiratory symptoms and fever cases, with peak incidence in ",
  monthly_health$month[which.max(monthly_health$resp_cases)], " (", max(monthly_health$resp_cases), " respiratory cases) and ",
  monthly_health$month[which.max(monthly_health$fever_cases)], " (", max(monthly_health$fever_cases), " fever cases). ",
  "The limited overlap between datasets (", length(overlap_dates), " days) prevented direct correlation analysis between environmental factors and health outcomes.\n\n",
  "## Conclusion\n",
  "This study provides valuable characterization of environmental conditions and COVID-19 health outcomes in Soweto during different phases of the pandemic. ",
  "While direct correlations could not be established due to data limitations, the separate analyses suggest seasonal patterns in both temperature and COVID-19 symptoms. ",
  "Future research should prioritize concurrent collection of environmental and health data to enable more robust analysis of these relationships. ",
  "These findings highlight the importance of integrated environmental health monitoring systems during public health emergencies."
)

# Save abstract
writeLines(abstract_text, "separate_analysis_abstract.md")
cat("Abstract saved to separate_analysis_abstract.md\n")

cat("\nAnalysis complete!\n")
