# Script to create publication-quality DLNM spline visualizations
# For ISES-ISEE 2025 Abstract

# Load required libraries
library(dlnm)
library(splines)
library(ggplot2)
library(gridExtra)
library(viridis)  # For better color palettes

# Load the saved model objects instead of running the full analysis
if (file.exists("dlnm_models.RData")) {
  cat("Loading saved DLNM model objects...\n")
  load("dlnm_models.RData")
} else {
  cat("No saved DLNM models found. Running the main analysis...\n")
  source("covid_climate_analysis_with_limited_data.R")
}

# Check if the required objects exist
if (!exists("cb_temp") || !exists("dlnm_combined")) {
  stop("DLNM model objects not found. There may be an issue with the main analysis.")
}

# Create directory for visualizations if it doesn't exist
if (!dir.exists("dlnm_visualizations")) {
  dir.create("dlnm_visualizations")
}

# ----- TEMPERATURE EFFECTS -----

# Create prediction for temperature effects
cat("Creating temperature effect visualizations...\n")
pred_temp <- crosspred(
  cb_temp, dlnm_combined,
  at = seq(min(overlap_data$mean_temp_c, na.rm = TRUE),
           max(overlap_data$mean_temp_c, na.rm = TRUE),
           length.out = 50),
  cumul = TRUE  # Also compute cumulative effects
)

# 1. Overall cumulative exposure-response curve
png("dlnm_visualizations/temperature_overall_effect.png", width = 800, height = 600, res = 120)
plot(pred_temp, "overall", xlab = "Temperature (°C)", ylab = "Relative Risk", 
     lwd = 2, col = "blue", ci.arg = list(col = rgb(0, 0, 1, 0.2)), 
     main = "Overall effect of temperature on respiratory symptoms")
dev.off()
cat("- Temperature overall effect plot created\n")

# 2. Lag-response curves at specific temperatures
# FIX: Use values that exist in the prediction object
# Get the actual values used in the prediction
pred_values <- pred_temp$predvar
# Choose values at approximately 10th, 50th, and 90th percentiles
temp_indices <- round(c(0.1, 0.5, 0.9) * length(pred_values))
temp_values <- pred_values[temp_indices]
temp_labels <- c("Cold", "Moderate", "Hot")

png("dlnm_visualizations/temperature_lag_effects.png", width = 800, height = 600, res = 120)
par(mfrow = c(1, 1))
plot(pred_temp, "slices", var = temp_values, col = viridis(3), lwd = 2,
     xlab = "Lag (days)", ylab = "Relative Risk", ci = "n",
     main = "Effect of temperature at different lags")
legend("topright", temp_labels, col = viridis(3), lwd = 2, cex = 0.8)
dev.off()
cat("- Temperature lag effects plot created\n")

# 3. 3D plot
png("dlnm_visualizations/temperature_3d_effect.png", width = 800, height = 600, res = 120)
plot(pred_temp, "3d", xlab = "Temperature (°C)", ylab = "Lag (days)", zlab = "RR",
     col = viridis(100), border = NA,
     main = "Three-dimensional effect of temperature on respiratory symptoms")
dev.off()
cat("- Temperature 3D effect plot created\n")

# 4. Contour plot - COMPLETELY DIFFERENT APPROACH
# Use image() and contour() instead of filled.contour()
png("dlnm_visualizations/temperature_contour.png", width = 800, height = 600, res = 120)
# Get the matrix of relative risks
rr_matrix <- pred_temp$matRRfit

# Create a simple image plot
par(mar = c(5, 4, 4, 6))  # Adjust margins to make room for the legend
image(
  x = 1:ncol(rr_matrix),
  y = 1:nrow(rr_matrix),
  z = rr_matrix,
  col = viridis(100),
  xlab = "Temperature (°C)",
  ylab = "Lag (days)",
  main = "Contour plot of temperature effect on respiratory symptoms",
  axes = FALSE
)

# Add contour lines
contour(
  x = 1:ncol(rr_matrix),
  y = 1:nrow(rr_matrix),
  z = rr_matrix,
  add = TRUE,
  drawlabels = TRUE
)

# Add custom axes
axis(1, at = seq(1, ncol(rr_matrix), length.out = 5), 
     labels = round(seq(min(pred_temp$predvar), max(pred_temp$predvar), length.out = 5), 1))
axis(2, at = 1:nrow(rr_matrix), labels = 0:(nrow(rr_matrix)-1))

# Add a color legend
legend_image <- as.raster(matrix(viridis(100), ncol = 1))
rasterImage(legend_image, 
            xleft = ncol(rr_matrix) + 1, 
            ybottom = 1, 
            xright = ncol(rr_matrix) + 2, 
            ytop = nrow(rr_matrix))
text(ncol(rr_matrix) + 2.5, nrow(rr_matrix)/2, "RR", srt = 90)

dev.off()
cat("- Temperature contour plot created\n")

# ----- NO2 EFFECTS -----

# Create prediction for NO2 effects
if (exists("cb_poll") && exists("pollutant")) {
  cat("\nCreating NO2 effect visualizations...\n")
  pred_no2 <- crosspred(
    cb_poll, dlnm_combined,
    at = seq(min(overlap_data[[pollutant]], na.rm = TRUE),
             max(overlap_data[[pollutant]], na.rm = TRUE),
             length.out = 50),
    cumul = TRUE
  )
  
  # 1. Overall cumulative exposure-response curve
  png("dlnm_visualizations/no2_overall_effect.png", width = 800, height = 600, res = 120)
  plot(pred_no2, "overall", xlab = paste0(pollutant_name, " Concentration"), 
       ylab = "Relative Risk", lwd = 2, col = "darkgreen", 
       ci.arg = list(col = rgb(0, 0.5, 0, 0.2)),
       main = paste0("Overall effect of ", pollutant_name, " on respiratory symptoms"))
  dev.off()
  cat("- NO2 overall effect plot created\n")
  
  # 2. Lag-response curves at specific NO2 levels
  # FIX: Use values that exist in the prediction object
  pred_no2_values <- pred_no2$predvar
  no2_indices <- round(c(0.1, 0.5, 0.9) * length(pred_no2_values))
  no2_values <- pred_no2_values[no2_indices]
  no2_labels <- c("Low", "Medium", "High")
  
  png("dlnm_visualizations/no2_lag_effects.png", width = 800, height = 600, res = 120)
  plot(pred_no2, "slices", var = no2_values, col = viridis(3, begin = 0.3, end = 0.9), 
       lwd = 2, xlab = "Lag (days)", ylab = "Relative Risk", ci = "n",
       main = paste0("Effect of ", pollutant_name, " at different lags"))
  legend("topright", no2_labels, col = viridis(3, begin = 0.3, end = 0.9), lwd = 2, cex = 0.8)
  dev.off()
  cat("- NO2 lag effects plot created\n")
  
  # 3. 3D plot
  png("dlnm_visualizations/no2_3d_effect.png", width = 800, height = 600, res = 120)
  plot(pred_no2, "3d", xlab = paste0(pollutant_name, " Concentration"), 
       ylab = "Lag (days)", zlab = "RR", col = viridis(100, begin = 0.3, end = 0.9), 
       border = NA,
       main = paste0("Three-dimensional effect of ", pollutant_name, " on respiratory symptoms"))
  dev.off()
  cat("- NO2 3D effect plot created\n")
  
  # 4. Contour plot - COMPLETELY DIFFERENT APPROACH
  png("dlnm_visualizations/no2_contour.png", width = 800, height = 600, res = 120)
  # Get the matrix of relative risks
  no2_rr_matrix <- pred_no2$matRRfit
  
  # Create a simple image plot
  par(mar = c(5, 4, 4, 6))  # Adjust margins to make room for the legend
  image(
    x = 1:ncol(no2_rr_matrix),
    y = 1:nrow(no2_rr_matrix),
    z = no2_rr_matrix,
    col = viridis(100, begin = 0.3, end = 0.9),
    xlab = paste0(pollutant_name, " Concentration"),
    ylab = "Lag (days)",
    main = paste0("Contour plot of ", pollutant_name, " effect on respiratory symptoms"),
    axes = FALSE
  )
  
  # Add contour lines
  contour(
    x = 1:ncol(no2_rr_matrix),
    y = 1:nrow(no2_rr_matrix),
    z = no2_rr_matrix,
    add = TRUE,
    drawlabels = TRUE
  )
  
  # Add custom axes
  axis(1, at = seq(1, ncol(no2_rr_matrix), length.out = 5), 
       labels = round(seq(min(pred_no2$predvar), max(pred_no2$predvar), length.out = 5), 6))
  axis(2, at = 1:nrow(no2_rr_matrix), labels = 0:(nrow(no2_rr_matrix)-1))
  
  # Add a color legend
  legend_image <- as.raster(matrix(viridis(100, begin = 0.3, end = 0.9), ncol = 1))
  rasterImage(legend_image, 
              xleft = ncol(no2_rr_matrix) + 1, 
              ybottom = 1, 
              xright = ncol(no2_rr_matrix) + 2, 
              ytop = nrow(no2_rr_matrix))
  text(ncol(no2_rr_matrix) + 2.5, nrow(no2_rr_matrix)/2, "RR", srt = 90)
  
  dev.off()
  cat("- NO2 contour plot created\n")
}

# ----- COMBINED VISUALIZATION FOR PUBLICATION -----

cat("\nCreating combined publication figure...\n")
# Create a publication-ready figure with multiple panels
png("dlnm_visualizations/combined_dlnm_publication_figure.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))

# Panel A: Temperature overall effect
plot(pred_temp, "overall", xlab = "Temperature (°C)", ylab = "Relative Risk", 
     lwd = 2, col = "blue", ci.arg = list(col = rgb(0, 0, 1, 0.2)), 
     main = "A) Overall effect of temperature")

# Panel B: Temperature lag effects
plot(pred_temp, "slices", var = temp_values, col = viridis(3), lwd = 2,
     xlab = "Lag (days)", ylab = "Relative Risk", ci = "n",
     main = "B) Temperature effect by lag")
legend("topright", temp_labels, col = viridis(3), lwd = 2, cex = 0.8)

# Panel C and D: NO2 effects or temperature contour
if (exists("pred_no2")) {
  # Panel C: NO2 overall effect
  plot(pred_no2, "overall", xlab = paste0(pollutant_name, " Concentration"), 
       ylab = "Relative Risk", lwd = 2, col = "darkgreen", 
       ci.arg = list(col = rgb(0, 0.5, 0, 0.2)),
       main = paste0("C) Overall effect of ", pollutant_name))
  
  # Panel D: NO2 lag effects
  plot(pred_no2, "slices", var = no2_values, col = viridis(3, begin = 0.3, end = 0.9), 
       lwd = 2, xlab = "Lag (days)", ylab = "Relative Risk", ci = "n",
       main = paste0("D) ", pollutant_name, " effect by lag"))
  legend("topright", no2_labels, col = viridis(3, begin = 0.3, end = 0.9), lwd = 2, cex = 0.8)
} else {
  # If NO2 data not available, show temperature contour plot
  # Panel C: Temperature contour plot (simplified version)
  image(
    x = 1:ncol(rr_matrix),
    y = 1:nrow(rr_matrix),
    z = rr_matrix,
    col = viridis(100),
    xlab = "Temperature (°C)",
    ylab = "Lag (days)",
    main = "C) Temperature effect (contour)",
    axes = FALSE
  )
  contour(
    x = 1:ncol(rr_matrix),
    y = 1:nrow(rr_matrix),
    z = rr_matrix,
    add = TRUE,
    drawlabels = TRUE
  )
  axis(1, at = seq(1, ncol(rr_matrix), length.out = 5), 
       labels = round(seq(min(pred_temp$predvar), max(pred_temp$predvar), length.out = 5), 1))
  axis(2, at = 1:nrow(rr_matrix), labels = 0:(nrow(rr_matrix)-1))
  
  # Panel D: Cold vs. Hot temperature effects
  plot(
    pred_temp, "slices", var = temp_values[c(1,3)], col = c("blue", "red"), 
    lwd = 2, xlab = "Lag (days)", ylab = "Relative Risk", ci = "n",
    main = "D) Cold vs. Hot temperature effects"
  )
  legend("topright", c("Cold", "Hot"), col = c("blue", "red"), lwd = 2, cex = 0.8)
}

dev.off()
cat("- Combined publication figure created\n")

cat("\nDLNM spline visualizations created in the 'dlnm_visualizations' folder\n") 