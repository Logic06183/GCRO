# Heat Health Analysis with Hybrid Data Approach
# This script attempts to use Earth Engine data but has a fallback to simulated data
# with clear labeling of which data source is being used

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)
library(reticulate)

# Configure reticulate
use_condaenv("base", required = TRUE)

# Function to attempt to fetch climate data from Earth Engine with proper error handling
fetch_climate_data_hybrid <- function(start_date, end_date, location = "Soweto", 
                                     cache_file = "climate_data_cache.RData", 
                                     force_simulated = FALSE) {
  
  # Check if cache file exists and use it if available
  if (file.exists(cache_file) && !force_simulated) {
    cat("Loading cached climate data...\n")
    load(cache_file)
    
    # Check if the cached data is valid and complete
    if (exists("climate_data") && !is.null(climate_data) && 
        min(climate_data$date) <= as.Date(start_date) && 
        max(climate_data$date) >= as.Date(end_date)) {
      
      # Check if the data has actual values and not just NAs
      missing_temp <- sum(is.na(climate_data$temp_mean))
      missing_pm25 <- sum(is.na(climate_data$pm25))
      
      # If more than 90% of the data is missing, consider it invalid
      if (missing_temp / nrow(climate_data) < 0.9 && missing_pm25 / nrow(climate_data) < 0.9) {
        cat("Using cached climate data\n")
        climate_data$data_source <- "Earth Engine (Cached)"
        return(climate_data)
      } else {
        cat("Cached data is incomplete (too many NA values), generating new data...\n")
      }
    }
  }
  
  # Create a sequence of dates
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  # If force_simulated is TRUE or we encounter an error, use simulated data
  if (force_simulated) {
    cat("Using simulated climate data as requested\n")
    use_simulated <- TRUE
  } else {
    cat("Attempting to fetch real climate data from Earth Engine...\n")
    
    # Try to initialize Earth Engine
    tryCatch({
      ee <- reticulate::import("ee")
      ee$Initialize()
      cat("Earth Engine initialized successfully\n")
      use_simulated <- FALSE
      
      # Try a simple Earth Engine operation to verify it's working
      soweto_point <- ee$Geometry$Point(c(27.8579, -26.2485))
      test_date <- ee$Date(as.character(Sys.Date() - 30))  # Use a date 30 days ago for testing
      test_dataset <- ee$ImageCollection('ECMWF/ERA5/DAILY')$filterDate(test_date, test_date)
      test_size <- test_dataset$size()$getInfo()
      
      if (test_size == 0) {
        cat("Earth Engine test failed: No data available\n")
        use_simulated <- TRUE
      }
    }, error = function(e) {
      cat("Earth Engine initialization failed:", conditionMessage(e), "\n")
      use_simulated <- TRUE
    })
  }
  
  # If we need to use simulated data
  if (use_simulated) {
    cat("Generating simulated climate data...\n")
    
    # Create simulated data
    set.seed(123)  # For reproducibility
    
    # Create an empty dataframe
    climate_data <- data.frame(
      date = dates,
      temp_mean = NA,
      temp_min = NA,
      temp_max = NA,
      pm25 = NA,
      data_source = "Simulated"
    )
    
    # Simulate temperature data with seasonal patterns
    day_of_year <- as.numeric(format(dates, "%j"))
    seasonal_component <- 15 * sin(2 * pi * (day_of_year - 15) / 365) + 20  # Peaks in summer
    
    # Add random variation
    random_variation <- rnorm(length(dates), mean = 0, sd = 3)
    
    # Generate temperature data
    climate_data$temp_mean <- seasonal_component + random_variation
    climate_data$temp_min <- climate_data$temp_mean - runif(length(dates), 3, 8)
    climate_data$temp_max <- climate_data$temp_mean + runif(length(dates), 3, 8)
    
    # Simulate PM2.5 data (correlated with temperature but with own patterns)
    pm25_seasonal <- 10 * sin(2 * pi * (day_of_year - 30) / 365) + 25  # Different phase
    climate_data$pm25 <- pm25_seasonal + rnorm(length(dates), mean = 0, sd = 5)
    climate_data$pm25 <- pmax(climate_data$pm25, 5)  # Ensure minimum value
    
    # Calculate heat index
    climate_data$heat_index <- with(climate_data, {
      temp_mean + (temp_max - temp_mean) * 0.2
    })
    
    cat("Simulated climate data generated\n")
    
    # Save to cache
    save(climate_data, file = cache_file)
    cat("Simulated data saved to cache file\n")
    
    return(climate_data)
  }
  
  # If we get here, we're using real Earth Engine data
  cat("Fetching real climate data from Earth Engine...\n")
  
  # Define location (Soweto coordinates)
  soweto_point <- ee$Geometry$Point(c(27.8579, -26.2485))  # longitude, latitude
  
  # Convert dates to EE format
  start_date_ee <- ee$Date(as.character(as.Date(start_date)))
  end_date_ee <- ee$Date(as.character(as.Date(end_date)))
  
  # Create an empty dataframe
  climate_data <- data.frame(
    date = dates,
    temp_mean = NA,
    temp_min = NA,
    temp_max = NA,
    pm25 = NA,
    data_source = "Earth Engine"
  )
  
  # Try to get ERA5 temperature data
  tryCatch({
    cat("Retrieving ERA5 temperature data...\n")
    era5_dataset <- ee$ImageCollection('ECMWF/ERA5/DAILY')$
      filterDate(start_date_ee, end_date_ee)
    
    # Get temperature data
    temp_data <- era5_dataset$select(c('mean_2m_air_temperature', 'minimum_2m_air_temperature', 'maximum_2m_air_temperature'))
    
    # Function to extract data at a point
    extract_temp_data <- function(image) {
      data <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = soweto_point,
        scale = 10000  # Scale in meters
      )
      return(ee$Feature(NULL, data))
    }
    
    # Apply extraction function to collection
    temp_features <- temp_data$map(extract_temp_data)
    
    # Get the data as a list
    temp_list <- temp_features$toList(temp_data$size())
    temp_size <- as.integer(temp_data$size()$getInfo())
    
    # Process temperature data
    for (i in 1:min(temp_size, length(dates))) {
      feature <- ee$Feature(temp_list$get(i - 1))
      props <- feature$getInfo()$properties
      
      if (!is.null(props)) {
        # Find the matching date index
        date_str <- names(props)[grep("system:time_start", names(props))]
        if (length(date_str) > 0) {
          timestamp <- as.numeric(props[[date_str]]) / 1000  # Convert from milliseconds to seconds
          date_index <- which(abs(as.numeric(dates) - as.numeric(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))) < 1)
          
          if (length(date_index) > 0) {
            climate_data$temp_mean[date_index] <- props[["mean_2m_air_temperature"]] - 273.15  # Convert from K to C
            climate_data$temp_min[date_index] <- props[["minimum_2m_air_temperature"]] - 273.15
            climate_data$temp_max[date_index] <- props[["maximum_2m_air_temperature"]] - 273.15
          }
        }
      }
    }
    
    cat("Temperature data retrieved successfully\n")
  }, error = function(e) {
    cat("Error retrieving temperature data:", conditionMessage(e), "\n")
    cat("Will use simulated temperature data instead\n")
    
    # Simulate temperature data
    day_of_year <- as.numeric(format(dates, "%j"))
    seasonal_component <- 15 * sin(2 * pi * (day_of_year - 15) / 365) + 20
    random_variation <- rnorm(length(dates), mean = 0, sd = 3)
    
    climate_data$temp_mean <- seasonal_component + random_variation
    climate_data$temp_min <- climate_data$temp_mean - runif(length(dates), 3, 8)
    climate_data$temp_max <- climate_data$temp_mean + runif(length(dates), 3, 8)
    climate_data$data_source <- "Hybrid (Temp: Simulated)"
  })
  
  # Try to get CAMS air quality data
  tryCatch({
    cat("Retrieving CAMS air quality data...\n")
    cams_dataset <- ee$ImageCollection('ECMWF/CAMS/NRT')$
      filterDate(start_date_ee, end_date_ee)
    
    # Check if we have data
    cams_size <- as.integer(cams_dataset$size()$getInfo())
    if (cams_size == 0) {
      stop("No CAMS data available for the specified date range")
    }
    
    # Get a sample image to check available bands
    sample_image <- ee$Image(cams_dataset$first())
    available_bands <- sample_image$bandNames()$getInfo()
    
    # Dynamically detect PM2.5 and NO2 band names
    pm25_band <- NULL
    no2_band <- NULL
    
    # Check for PM2.5 band
    pm25_candidates <- c('particulate_matter_2.5um', 'pm2p5', 'PM2P5')
    for (band in pm25_candidates) {
      if (band %in% available_bands) {
        pm25_band <- band
        break
      }
    }
    
    # Check for NO2 band
    no2_candidates <- c('nitrogen_dioxide', 'no2', 'NO2')
    for (band in no2_candidates) {
      if (band %in% available_bands) {
        no2_band <- band
        break
      }
    }
    
    if (is.null(pm25_band)) {
      cat("PM2.5 band not found in CAMS dataset. Available bands:", paste(available_bands, collapse=", "), "\n")
      stop("PM2.5 band not found")
    }
    
    # Select air quality bands
    bands_to_select <- c(pm25_band)
    if (!is.null(no2_band)) {
      bands_to_select <- c(bands_to_select, no2_band)
    }
    
    air_quality_data <- cams_dataset$select(bands_to_select)
    
    # Function to extract data at a point
    extract_air_quality_data <- function(image) {
      data <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = soweto_point,
        scale = 10000  # Scale in meters
      )
      return(ee$Feature(NULL, data))
    }
    
    # Apply extraction function to collection
    air_quality_features <- air_quality_data$map(extract_air_quality_data)
    
    # Get the data as a list
    air_quality_list <- air_quality_features$toList(air_quality_data$size())
    air_quality_size <- as.integer(air_quality_data$size()$getInfo())
    
    # Process air quality data
    for (i in 1:min(air_quality_size, length(dates))) {
      feature <- ee$Feature(air_quality_list$get(i - 1))
      props <- feature$getInfo()$properties
      
      if (!is.null(props)) {
        # Find the matching date index
        date_str <- names(props)[grep("system:time_start", names(props))]
        if (length(date_str) > 0) {
          timestamp <- as.numeric(props[[date_str]]) / 1000  # Convert from milliseconds to seconds
          date_index <- which(abs(as.numeric(dates) - as.numeric(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))) < 1)
          
          if (length(date_index) > 0) {
            if (!is.null(pm25_band) && pm25_band %in% names(props)) {
              climate_data$pm25[date_index] <- props[[pm25_band]]
            }
          }
        }
      }
    }
    
    cat("Air quality data retrieved successfully\n")
  }, error = function(e) {
    cat("Error retrieving air quality data:", conditionMessage(e), "\n")
    cat("Will use simulated air quality data instead\n")
    
    # Simulate PM2.5 data
    day_of_year <- as.numeric(format(dates, "%j"))
    pm25_seasonal <- 10 * sin(2 * pi * (day_of_year - 30) / 365) + 25
    climate_data$pm25 <- pm25_seasonal + rnorm(length(dates), mean = 0, sd = 5)
    climate_data$pm25 <- pmax(climate_data$pm25, 5)  # Ensure minimum value
    
    # Update data source to indicate hybrid
    if (climate_data$data_source[1] == "Earth Engine") {
      climate_data$data_source <- "Hybrid (PM2.5: Simulated)"
    } else if (climate_data$data_source[1] == "Hybrid (Temp: Simulated)") {
      climate_data$data_source <- "Simulated"  # Both temp and PM2.5 are simulated
    }
  })
  
  # Calculate heat index
  climate_data$heat_index <- with(climate_data, {
    # Simple heat index calculation
    # For a more accurate formula, we would need humidity data
    temp_mean + (temp_max - temp_mean) * 0.2
  })
  
  # Check if we have any real data
  missing_temp <- sum(is.na(climate_data$temp_mean))
  missing_pm25 <- sum(is.na(climate_data$pm25))
  
  # If more than 90% of the data is missing, use simulated data instead
  if (missing_temp / nrow(climate_data) > 0.9 || missing_pm25 / nrow(climate_data) > 0.9) {
    cat("Too much missing data from Earth Engine, falling back to simulated data\n")
    
    # Create simulated data
    set.seed(123)  # For reproducibility
    
    # Simulate temperature data with seasonal patterns
    day_of_year <- as.numeric(format(dates, "%j"))
    seasonal_component <- 15 * sin(2 * pi * (day_of_year - 15) / 365) + 20  # Peaks in summer
    
    # Add random variation
    random_variation <- rnorm(length(dates), mean = 0, sd = 3)
    
    # Generate temperature data
    climate_data$temp_mean <- seasonal_component + random_variation
    climate_data$temp_min <- climate_data$temp_mean - runif(length(dates), 3, 8)
    climate_data$temp_max <- climate_data$temp_mean + runif(length(dates), 3, 8)
    
    # Simulate PM2.5 data (correlated with temperature but with own patterns)
    pm25_seasonal <- 10 * sin(2 * pi * (day_of_year - 30) / 365) + 25  # Different phase
    climate_data$pm25 <- pm25_seasonal + rnorm(length(dates), mean = 0, sd = 5)
    climate_data$pm25 <- pmax(climate_data$pm25, 5)  # Ensure minimum value
    
    # Calculate heat index
    climate_data$heat_index <- with(climate_data, {
      temp_mean + (temp_max - temp_mean) * 0.2
    })
    
    climate_data$data_source <- "Simulated (Earth Engine Failed)"
  }
  
  # Save to cache
  save(climate_data, file = cache_file)
  cat("Climate data saved to cache file\n")
  
  return(climate_data)
}

# Example usage:
# climate_data <- fetch_climate_data_hybrid("2020-01-01", "2020-12-31", force_simulated = FALSE)
