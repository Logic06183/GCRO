# Script to create DLNM visualizations from saved model objects

# Load required libraries
library(dlnm)
library(splines)

# Load the saved model objects (if they were saved)
# load("dlnm_models.RData")

# If models weren't saved, you'll need to run the analysis again
source("covid_climate_analysis_with_limited_data.R")

# Create NO2 visualization
if (exists("cb_poll") && exists("dlnm_combined")) {
  # Create prediction for NO2 effects
  pred_no2 <- crosspred(
    cb_poll, dlnm_combined,
    at = seq(min(overlap_data[[pollutant]], na.rm = TRUE),
             max(overlap_data[[pollutant]], na.rm = TRUE),
             length.out = 50)
  )
  
  # Plot NO2 effects
  png("no2_dlnm_effect.png", width = 800, height = 600)
  plot(
    pred_no2,
    xlab = "NO2 Concentration",
    ylab = "Relative Risk",
    main = paste0("Overall effect of ", pollutant_name, " on respiratory cases")
  )
  dev.off()
  cat(paste0(pollutant_name, " DLNM analysis plot saved to no2_dlnm_effect.png\n"))
}

# Create combined visualization
# (code from above) 