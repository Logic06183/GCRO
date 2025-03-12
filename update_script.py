"""
Script to update the air quality data collection section in fetch_climate_data.py
"""

import re

# Path to the original script
script_path = 'fetch_climate_data.py'

# New air quality data collection code
new_code = '''    # Fetch air quality data (try multiple sources)
    try:
        print("Fetching air quality data...")
        
        # Get CAMS NRT data for PM2.5 and PM10
        cams_collection = ee.ImageCollection('ECMWF/CAMS/NRT') \\
            .filterDate(start_date_ee, end_date_ee) \\
            .select(['particulate_matter_d_less_than_25_um_surface',
                     'particulate_matter_d_less_than_10_um_surface'])
        
        # Get Sentinel-5P data for NO2
        no2_collection = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_NO2') \\
            .filterDate(start_date_ee, end_date_ee) \\
            .select(['tropospheric_NO2_column_number_density'])
        
        # Get Sentinel-5P data for O3
        o3_collection = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_O3') \\
            .filterDate(start_date_ee, end_date_ee) \\
            .select(['O3_column_number_density'])
        
        # Check if we have data in each collection
        cams_size = cams_collection.size().getInfo()
        no2_size = no2_collection.size().getInfo()
        o3_size = o3_collection.size().getInfo()
        
        print(f"Found {cams_size} days of CAMS NRT data")
        print(f"Found {no2_size} days of Sentinel-5P NO2 data")
        print(f"Found {o3_size} days of Sentinel-5P O3 data")
        
        # Get a sample image to check available bands
        if cams_size > 0:
            sample_image = cams_collection.first()
            available_bands = sample_image.bandNames().getInfo()
            print(f"Available bands in CAMS dataset: {available_bands}")
        
        # Function to extract air quality data for a specific date
        def extract_air_quality_data(date_str):
            date_ee = ee.Date(date_str)
            next_day = date_ee.advance(1, 'day')
            
            result = {'date': date_str}
            
            # Get CAMS data for PM2.5 and PM10
            if cams_size > 0:
                cams_filtered = cams_collection.filterDate(date_ee, next_day)
                if cams_filtered.size().getInfo() > 0:
                    cams_image = cams_filtered.first()
                    
                    # Extract PM2.5 data
                    try:
                        pm25_data = cams_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('particulate_matter_d_less_than_25_um_surface').getInfo()
                        
                        if pm25_data is not None:
                            result['pm25'] = pm25_data
                    except Exception as e:
                        print(f"Error extracting PM2.5 data for {date_str}: {e}")
                    
                    # Extract PM10 data
                    try:
                        pm10_data = cams_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('particulate_matter_d_less_than_10_um_surface').getInfo()
                        
                        if pm10_data is not None:
                            result['pm10'] = pm10_data
                    except Exception as e:
                        print(f"Error extracting PM10 data for {date_str}: {e}")
            
            # Get NO2 data
            if no2_size > 0:
                no2_filtered = no2_collection.filterDate(date_ee, next_day)
                if no2_filtered.size().getInfo() > 0:
                    no2_image = no2_filtered.first()
                    
                    # Extract NO2 data
                    try:
                        no2_data = no2_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('tropospheric_NO2_column_number_density').getInfo()
                        
                        if no2_data is not None:
                            # Convert from mol/m^2 to μg/m^3 (approximate conversion)
                            # Multiply by 1e6 to convert from mol/m^2 to μmol/m^2
                            # Then multiply by 46 (molecular weight of NO2)
                            # Then divide by approximate column height (~10km = 10000m)
                            no2_value = (no2_data * 1e6 * 46) / 10000
                            result['no2'] = no2_value
                    except Exception as e:
                        print(f"Error extracting NO2 data for {date_str}: {e}")
            
            # Get O3 data
            if o3_size > 0:
                o3_filtered = o3_collection.filterDate(date_ee, next_day)
                if o3_filtered.size().getInfo() > 0:
                    o3_image = o3_filtered.first()
                    
                    # Extract O3 data
                    try:
                        o3_data = o3_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('O3_column_number_density').getInfo()
                        
                        if o3_data is not None:
                            # Convert from mol/m^2 to μg/m^3 (approximate conversion)
                            # Multiply by 1e6 to convert from mol/m^2 to μmol/m^2
                            # Then multiply by 48 (molecular weight of O3)
                            # Then divide by approximate column height (~10km = 10000m)
                            o3_value = (o3_data * 1e6 * 48) / 10000
                            result['o3'] = o3_value
                    except Exception as e:
                        print(f"Error extracting O3 data for {date_str}: {e}")
            
            if len(result) > 1:  # More than just date
                return result
            else:
                return None
        
        # Extract data for each date
        air_quality_success = {'pm25': 0, 'pm10': 0, 'no2': 0, 'o3': 0}
        for date_str in date_range:
            try:
                data = extract_air_quality_data(date_str)
                if data:
                    idx = climate_data[climate_data['date'] == pd.to_datetime(date_str)].index
                    if len(idx) > 0:
                        if 'pm25' in data:
                            climate_data.loc[idx, 'pm25'] = data['pm25']
                            air_quality_success['pm25'] += 1
                        if 'pm10' in data:
                            climate_data.loc[idx, 'pm10'] = data['pm10']
                            air_quality_success['pm10'] += 1
                        if 'no2' in data:
                            climate_data.loc[idx, 'no2'] = data['no2']
                            air_quality_success['no2'] += 1
                        if 'o3' in data:
                            climate_data.loc[idx, 'o3'] = data['o3']
                            air_quality_success['o3'] += 1
                # Add a small delay to avoid rate limiting
                time.sleep(0.1)
            except Exception as e:
                print(f"Error extracting air quality data for {date_str}: {e}")
        
        print(f"Successfully retrieved air quality data:")
        print(f"  PM2.5: {air_quality_success['pm25']} out of {len(date_range)} days ({air_quality_success['pm25']/len(date_range)*100:.1f}%)")
        print(f"  PM10: {air_quality_success['pm10']} out of {len(date_range)} days ({air_quality_success['pm10']/len(date_range)*100:.1f}%)")
        print(f"  NO2: {air_quality_success['no2']} out of {len(date_range)} days ({air_quality_success['no2']/len(date_range)*100:.1f}%)")
        print(f"  O3: {air_quality_success['o3']} out of {len(date_range)} days ({air_quality_success['o3']/len(date_range)*100:.1f}%)")
        
    except Exception as e:
        print(f"Error fetching air quality data: {e}")
        print("Will use simulated air quality data")'''

try:
    # Read the original script
    with open(script_path, 'r') as file:
        content = file.read()
    
    # Define the pattern to find the air quality section
    start_pattern = r'# Fetch air quality data \(try multiple sources\)'
    end_pattern = r'print\("Will use simulated air quality data"\)'
    
    # Find the air quality section
    match = re.search(f'{start_pattern}.*?{end_pattern}', content, re.DOTALL)
    
    if match:
        # Replace the air quality section with the new code
        updated_content = content.replace(match.group(0), new_code)
        
        # Write the updated content back to the file
        with open(script_path, 'w') as file:
            file.write(updated_content)
        
        print("Successfully updated the air quality data collection section in fetch_climate_data.py")
    else:
        print("Could not find the air quality data collection section in the script")
        
except Exception as e:
    print(f"Error updating the script: {e}")
