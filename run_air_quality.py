"""
Runner script for air quality data collection
"""
from air_quality_gee import fetch_air_quality_data, analyze_air_quality
import os

# Create output directory for results
output_dir = "air_quality_results"
os.makedirs(output_dir, exist_ok=True)

# Define Johannesburg coordinates (longitude, latitude)
johannesburg = {
    "name": "Johannesburg",
    "coordinates": [28.0473, -26.2041]  # [longitude, latitude]
}

# Define date range
# Using a more recent time period for your abstract (adjust as needed)
start_date = "2024-01-01"
end_date = "2024-02-29"  # Two months of recent data

print(f"Starting air quality data collection for {johannesburg['name']} from {start_date} to {end_date}")

try:
    # Fetch air quality data
    air_quality_data = fetch_air_quality_data(
        region_name=johannesburg["name"],
        coordinates=johannesburg["coordinates"],
        start_date_str=start_date,
        end_date_str=end_date,
        output_dir=output_dir
    )
    
    # Analyze the collected data
    summary = analyze_air_quality(air_quality_data, output_dir=output_dir)
    
    # Print summary information
    print("\nAir Quality Data Summary:")
    print(f"Total days analyzed: {len(air_quality_data)}")
    
    pollutants = ['pm25', 'pm10', 'no2', 'o3', 'co']
    for pollutant in pollutants:
        if pollutant in air_quality_data.columns:
            data_available = air_quality_data[pollutant].notna().sum()
            total_days = len(air_quality_data)
            percent = (data_available / total_days) * 100
            print(f"{pollutant.upper()}: Data available for {data_available} out of {total_days} days ({percent:.1f}%)")
    
    # Display successful completion message
    print(f"\nAll processing complete. Results saved to {output_dir} directory.")
    
except Exception as e:
    print(f"ERROR: {e}")
    print("Data collection failed. Please check error messages above for details.")
