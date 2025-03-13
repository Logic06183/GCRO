# Function to calculate daily statistics for each dataset
def calculate_daily_stats(image):
    date = ee.Date(image.get('system:time_start')).format('YYYY-MM-dd')
    stats = image.reduceRegion(
        reducer=ee.Reducer.mean(),
        geometry=soweto_region,
        scale=10000,
        maxPixels=1e9
    )
    return ee.Feature(None, {
        'date': date,
        'properties': stats
    })

# After collecting all data, properly aggregate by date:
if all_data:
    # Combine all months
    air_quality_data = pd.concat(all_data)
    
    # Extract properties
    air_quality_data['no2'] = air_quality_data['properties'].apply(
        lambda x: x.get('NO2_column_number_density') if isinstance(x, dict) else None)
    air_quality_data['tropospheric_no2'] = air_quality_data['properties'].apply(
        lambda x: x.get('tropospheric_NO2_column_number_density') if isinstance(x, dict) else None)
    air_quality_data['o3'] = air_quality_data['properties'].apply(
        lambda x: x.get('O3_column_number_density') if isinstance(x, dict) else None)
    
    # Extract PM2.5 data
    try:
        air_quality_data['pm25'] = air_quality_data['properties'].apply(
            lambda x: x.get('particulate_matter_d_less_than_25_um_surface') if isinstance(x, dict) else None)
    except Exception as e:
        print(f"Error extracting PM2.5 data: {e}")
    
    # Get unique dates and aggregate data
    print("Aggregating data to daily values...")
    
    # Ensure date is treated as a string
    air_quality_data['date'] = air_quality_data['date'].astype(str)
    
    # Group by date and calculate mean of each variable
    daily_data = air_quality_data.groupby('date').agg({
        'no2': 'mean',
        'tropospheric_no2': 'mean',
        'o3': 'mean',
        'pm25': 'mean'
    }).reset_index()
    
    print(f"Successfully aggregated to {len(daily_data)} daily records")
    
    # Save to CSV
    daily_data.to_csv('soweto_air_quality_data_extended.csv', index=False)
    print(f"Extended air quality data saved to soweto_air_quality_data_extended.csv with {len(daily_data)} records") 