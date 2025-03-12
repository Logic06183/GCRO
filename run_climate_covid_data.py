"""
Runner script for climate data collection for COVID-19 research in Soweto
This script collects temperature, precipitation, humidity, and air quality data 
for the Soweto region during the COVID-19 pandemic period
"""
from climate_data_collector import fetch_climate_data
import os
import pandas as pd
from datetime import datetime

# Create output directory for results
output_dir = "climate_covid_results"
os.makedirs(output_dir, exist_ok=True)

# Define Soweto coordinates (longitude, latitude)
soweto = {
    "name": "Soweto",
    "coordinates": [27.9300, -26.2585]  # [longitude, latitude]
}

# Define COVID-19 pandemic period
# Starting from first recorded case in South Africa to end of significant waves
start_date = "2020-03-01"  # First COVID-19 case in South Africa
end_date = "2021-12-31"    # Covering major pandemic waves

print(f"Starting climate data collection for {soweto['name']} from {start_date} to {end_date}")
print(f"This will capture the entire pandemic period in South Africa")

try:
    # Fetch comprehensive climate data
    climate_data = fetch_climate_data(
        region_name=soweto["name"],
        coordinates=soweto["coordinates"],
        start_date_str=start_date,
        end_date_str=end_date,
        output_dir=output_dir
    )
    
    # Calculate additional health-relevant metrics
    
    # 1. Calculate heat index (if temperature and relative humidity available)
    if 'temperature' in climate_data.columns and 'humidity' in climate_data.columns:
        print("Calculating heat index...")
        
        def calculate_heat_index(T, RH):
            """
            Calculate heat index based on temperature (°C) and relative humidity (%)
            Using the formula from the US National Weather Service
            """
            # Convert Celsius to Fahrenheit for the formula
            T_f = T * 9/5 + 32
            
            # Simple heat index formula
            hi = 0.5 * (T_f + 61.0 + ((T_f - 68.0) * 1.2) + (RH * 0.094))
            
            # Use more complex formula if heat index > 80°F
            if hi > 80:
                hi = -42.379 + 2.04901523 * T_f + 10.14333127 * RH - 0.22475541 * T_f * RH \
                    - 0.00683783 * T_f * T_f - 0.05481717 * RH * RH + 0.00122874 * T_f * T_f * RH \
                    + 0.00085282 * T_f * RH * RH - 0.00000199 * T_f * T_f * RH * RH
            
            # Convert back to Celsius
            hi_c = (hi - 32) * 5/9
            return hi_c
        
        # Apply heat index calculation
        climate_data['heat_index'] = climate_data.apply(
            lambda row: calculate_heat_index(row['temperature'], row['humidity']) 
            if pd.notna(row['temperature']) and pd.notna(row['humidity']) else None, 
            axis=1
        )
    
    # 2. Identify extreme heat days
    if 'temperature' in climate_data.columns:
        print("Identifying extreme heat days...")
        # Define threshold for extreme heat (adjust based on local context)
        # WHO and South African Weather Service consider days above 30°C as hot days
        extreme_heat_threshold = 30.0  # Celsius
        climate_data['extreme_heat_day'] = climate_data['temperature'] > extreme_heat_threshold
    
    # 3. Calculate combined air quality index if multiple pollutants available
    pollutants = [col for col in climate_data.columns if col in ['pm25', 'pm10', 'no2', 'o3']]
    if len(pollutants) > 0:
        print(f"Calculating air quality index based on available pollutants: {pollutants}")
        # Simple approach: normalize each pollutant by its WHO guideline and take maximum
        if 'pm25' in pollutants:
            climate_data['pm25_index'] = climate_data['pm25'] / 15.0  # WHO 24h guideline (2021)
        if 'pm10' in pollutants:
            climate_data['pm10_index'] = climate_data['pm10'] / 45.0  # WHO 24h guideline (2021)
        if 'no2' in pollutants:
            climate_data['no2_index'] = climate_data['no2'] / 25.0   # WHO 24h guideline (2021)
        if 'o3' in pollutants:
            climate_data['o3_index'] = climate_data['o3'] / 100.0    # WHO 8h guideline (2021)
        
        # Create indices columns list
        indices = [f"{p}_index" for p in pollutants if f"{p}_index" in climate_data.columns]
        
        if indices:
            # Take maximum of normalized values as the AQI indicator
            climate_data['air_quality_index'] = climate_data[indices].max(axis=1)
            
            # Categorize AQI
            climate_data['air_quality_category'] = pd.cut(
                climate_data['air_quality_index'],
                bins=[0, 0.5, 1.0, 1.5, 2.0, float('inf')],
                labels=['Very Good', 'Good', 'Moderate', 'Poor', 'Very Poor']
            )
    
    # Save enhanced climate data
    enhanced_file = os.path.join(output_dir, f"{soweto['name'].lower()}_covid_climate_data.csv")
    climate_data.to_csv(enhanced_file, index=False)
    print(f"\nEnhanced climate data saved to {enhanced_file}")
    
    # Generate summary statistics for the abstract
    print("\nSummary Statistics for Abstract:")
    
    # Temperature statistics
    if 'temperature' in climate_data.columns:
        temp_mean = climate_data['temperature'].mean()
        temp_max = climate_data['temperature'].max()
        temp_min = climate_data['temperature'].min()
        extreme_days = climate_data['extreme_heat_day'].sum() if 'extreme_heat_day' in climate_data.columns else 0
        
        print(f"Temperature: Mean={temp_mean:.1f}°C, Min={temp_min:.1f}°C, Max={temp_max:.1f}°C")
        print(f"Extreme heat days (>{extreme_heat_threshold}°C): {extreme_days} days")
    
    # Precipitation statistics
    if 'precipitation' in climate_data.columns:
        precip_mean = climate_data['precipitation'].mean()
        precip_days = (climate_data['precipitation'] > 0).sum()
        
        print(f"Precipitation: Mean={precip_mean:.1f}mm, Rainy days: {precip_days} days")
    
    # Air quality statistics
    for pollutant in pollutants:
        if pollutant in climate_data.columns:
            poll_mean = climate_data[pollutant].mean()
            poll_max = climate_data[pollutant].max()
            data_days = climate_data[pollutant].notna().sum()
            total_days = len(climate_data)
            coverage = (data_days / total_days) * 100
            
            print(f"{pollutant.upper()}: Mean={poll_mean:.2f}μg/m³, Max={poll_max:.2f}μg/m³, Coverage={coverage:.1f}%")
    
    # Air quality category distribution
    if 'air_quality_category' in climate_data.columns:
        aqi_distribution = climate_data['air_quality_category'].value_counts()
        print("\nAir Quality Distribution:")
        for category, count in aqi_distribution.items():
            percentage = (count / len(climate_data)) * 100
            print(f"  {category}: {count} days ({percentage:.1f}%)")
    
    # Generate abstract paragraph based on collected data
    abstract_template = """
Environmental Factors During COVID-19 Pandemic in Soweto, South Africa

Analysis of satellite-derived environmental data for Soweto from March 2020 to December 2021 revealed that 
during the COVID-19 pandemic, the region experienced average temperatures of {temp_mean:.1f}°C with {extreme_days} days 
exceeding {extreme_heat_threshold}°C (considered extreme heat days). Precipitation averaged {precip_mean:.1f}mm per day.

Air quality analysis showed mean PM2.5 levels of {pm25_mean:.2f}μg/m³ and PM10 levels of {pm10_mean:.2f}μg/m³. 
{aqi_statement}

These environmental factors may have influenced COVID-19 transmission and severity, particularly in socioeconomically 
vulnerable communities where limited access to air conditioning, indoor ventilation, and healthcare could have 
exacerbated adverse health outcomes. The collected data provides a foundation for examining relationships 
between environmental exposures, socioeconomic determinants, and COVID-19 health outcomes in Soweto.

[Generated on {current_date}]
"""
    
    # Prepare values for abstract
    pm25_mean = climate_data['pm25'].mean() if 'pm25' in climate_data.columns else float('nan')
    pm10_mean = climate_data['pm10'].mean() if 'pm10' in climate_data.columns else float('nan')
    
    # Determine AQI statement based on available data
    if 'air_quality_category' in climate_data.columns:
        most_common_category = aqi_distribution.idxmax()
        most_common_percentage = (aqi_distribution.max() / len(climate_data)) * 100
        aqi_statement = f"Overall air quality was predominantly classified as '{most_common_category}' ({most_common_percentage:.1f}% of days)."
    else:
        aqi_statement = "Air quality measures provide important context for respiratory health during the pandemic."
    
    # Generate abstract with actual values
    abstract_text = abstract_template.format(
        temp_mean=temp_mean if 'temperature' in climate_data.columns else float('nan'),
        extreme_days=extreme_days if 'extreme_heat_day' in climate_data.columns else 0,
        extreme_heat_threshold=extreme_heat_threshold,
        precip_mean=precip_mean if 'precipitation' in climate_data.columns else float('nan'),
        pm25_mean=pm25_mean,
        pm10_mean=pm10_mean,
        aqi_statement=aqi_statement,
        current_date=datetime.now().strftime('%B %d, %Y')
    )
    
    # Save abstract
    abstract_file = os.path.join(output_dir, "covid_climate_abstract.txt")
    with open(abstract_file, 'w') as f:
        f.write(abstract_text)
    
    print(f"\nAbstract text for COVID-climate interactions saved to {abstract_file}")
    print("\nAll climate data processing complete!")
    
except Exception as e:
    print(f"ERROR: {e}")
    print("Climate data collection failed. Please check error messages above for details.")
