"""
Air Quality Data Collection Script for GEE

This script focuses specifically on collecting air quality metrics from Google Earth Engine
for a research abstract. It retrieves PM2.5, PM10, NO2, and O3 data for a specified region
and date range.
"""

import os
import time
import pandas as pd
from datetime import datetime, timedelta
import ee

# Initialize Earth Engine
try:
    ee.Initialize()
    print("Earth Engine initialized successfully")
except Exception as e:
    print(f"Error initializing Earth Engine: {e}")
    raise Exception("Earth Engine initialization failed. Please check your authentication.")

def fetch_air_quality_data(region_name, coordinates, start_date_str, end_date_str, output_dir="data"):
    """
    Fetch air quality data for a specified region and date range.
    
    Parameters:
    -----------
    region_name : str
        Name of the region for which to fetch data
    coordinates : list
        List of [lng, lat] coordinates defining the region
    start_date_str : str
        Start date in 'YYYY-MM-DD' format
    end_date_str : str
        End date in 'YYYY-MM-DD' format
    output_dir : str, optional
        Directory to save output files
    
    Returns:
    --------
    pandas.DataFrame
        DataFrame containing air quality data for the specified region and date range
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    print(f"Fetching air quality data for {region_name} from {start_date_str} to {end_date_str}")
    
    # Convert coordinates to Earth Engine geometry
    if isinstance(coordinates[0], list):  # Polygon
        area = ee.Geometry.Polygon(coordinates)
    else:  # Point
        area = ee.Geometry.Point(coordinates)
    
    # Parse dates
    start_date = datetime.strptime(start_date_str, '%Y-%m-%d')
    end_date = datetime.strptime(end_date_str, '%Y-%m-%d')
    
    # Generate date range
    date_range = []
    current_date = start_date
    while current_date <= end_date:
        date_range.append(current_date.strftime('%Y-%m-%d'))
        current_date += timedelta(days=1)
    
    # Convert dates to Earth Engine format
    start_date_ee = ee.Date(start_date_str)
    end_date_ee = ee.Date(end_date_str).advance(1, 'day')  # Add 1 day to include the end date
    
    # Create dataframe to store air quality data
    air_quality_data = pd.DataFrame({'date': pd.date_range(start=start_date, end=end_date)})
    air_quality_data['region'] = region_name
    
    # Fetch air quality data from multiple sources
    try:
        print("Fetching air quality data from GEE sources...")
        
        # Get CAMS NRT data for PM2.5 and PM10
        cams_collection = ee.ImageCollection('ECMWF/CAMS/NRT') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['particulate_matter_d_less_than_25_um_surface',
                     'particulate_matter_d_less_than_10_um_surface'])
        
        # Get Sentinel-5P data for NO2
        no2_collection = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_NO2') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['tropospheric_NO2_column_number_density'])
        
        # Get Sentinel-5P data for O3
        o3_collection = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_O3') \
            .filterDate(start_date_ee, end_date_ee) \
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
                    idx = air_quality_data[air_quality_data['date'] == pd.to_datetime(date_str)].index
                    if len(idx) > 0:
                        if 'pm25' in data:
                            air_quality_data.loc[idx, 'pm25'] = data['pm25']
                            air_quality_success['pm25'] += 1
                        if 'pm10' in data:
                            air_quality_data.loc[idx, 'pm10'] = data['pm10']
                            air_quality_success['pm10'] += 1
                        if 'no2' in data:
                            air_quality_data.loc[idx, 'no2'] = data['no2']
                            air_quality_success['no2'] += 1
                        if 'o3' in data:
                            air_quality_data.loc[idx, 'o3'] = data['o3']
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
        
        # Handle missing data strategy
        # Alternative 1: Fill missing values with spatial interpolation
        try:
            if air_quality_data['pm25'].isna().any():
                print("Attempting to get PM2.5 data from alternative source: MODIS AOD data")
                # MODIS Aerosol Optical Depth can be used as a proxy for PM2.5
                modis_collection = ee.ImageCollection('MODIS/006/MOD04_L2') \
                    .filterDate(start_date_ee, end_date_ee) \
                    .select(['Optical_Depth_Land_And_Ocean'])
                
                # Function to extract AOD data for a specific date
                def extract_aod_data(date_str):
                    date_ee = ee.Date(date_str)
                    
                    # MODIS data is usually every few days, so we need to get the closest image
                    # Get a 10-day window around the date to ensure we capture at least one image
                    window_start = date_ee.advance(-5, 'day')
                    window_end = date_ee.advance(5, 'day')
                    
                    modis_filtered = modis_collection.filterDate(window_start, window_end)
                    if modis_filtered.size().getInfo() > 0:
                        # Sort by distance from target date
                        def add_date_distance(image):
                            image_date = ee.Date(image.get('system:time_start'))
                            diff_days = image_date.difference(date_ee, 'day').abs()
                            return image.set('date_diff', diff_days)
                        
                        modis_filtered = modis_filtered.map(add_date_distance)
                        modis_sorted = modis_filtered.sort('date_diff')
                        modis_image = modis_sorted.first()
                        
                        try:
                            aod_data = modis_image.reduceRegion(
                                reducer=ee.Reducer.mean(),
                                geometry=area,
                                scale=10000
                            ).get('Optical_Depth_Land_And_Ocean').getInfo()
                            
                            if aod_data is not None:
                                # Convert AOD to PM2.5 using a simple linear model
                                # PM2.5 ≈ AOD * 30 (rough approximation)
                                return aod_data * 30
                        except Exception as e:
                            print(f"Error extracting AOD data for {date_str}: {e}")
                
                # Fill in missing PM2.5 data for each date
                missing_pm25_idx = air_quality_data['pm25'].isna()
                missing_dates = air_quality_data.loc[missing_pm25_idx, 'date'].dt.strftime('%Y-%m-%d').tolist()
                
                alt_success = 0
                for date_str in missing_dates:
                    try:
                        aod_pm25 = extract_aod_data(date_str)
                        if aod_pm25 is not None:
                            idx = air_quality_data[air_quality_data['date'] == pd.to_datetime(date_str)].index
                            if len(idx) > 0:
                                air_quality_data.loc[idx, 'pm25_alt'] = aod_pm25
                                alt_success += 1
                        time.sleep(0.1)
                    except Exception as e:
                        print(f"Error extracting alternative PM2.5 data for {date_str}: {e}")
                
                if alt_success > 0:
                    print(f"Successfully retrieved alternative PM2.5 data for {alt_success} days")
        
        except Exception as e:
            print(f"Could not obtain alternative PM2.5 data: {e}")
        
        # Alternative 2: Attempt to get data from TROPOMI for CO as another air quality indicator
        try:
            print("Fetching additional CO data from TROPOMI...")
            co_collection = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_CO') \
                .filterDate(start_date_ee, end_date_ee) \
                .select(['CO_column_number_density'])
            
            co_size = co_collection.size().getInfo()
            print(f"Found {co_size} days of CO data")
            
            # Function to extract CO data
            def extract_co_data(date_str):
                date_ee = ee.Date(date_str)
                next_day = date_ee.advance(1, 'day')
                
                co_filtered = co_collection.filterDate(date_ee, next_day)
                if co_filtered.size().getInfo() > 0:
                    co_image = co_filtered.first()
                    
                    try:
                        co_data = co_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000
                        ).get('CO_column_number_density').getInfo()
                        
                        if co_data is not None:
                            # Convert from mol/m^2 to mg/m^3
                            # Multiply by 1e6 to convert from mol/m^2 to μmol/m^2
                            # Then multiply by 28 (molecular weight of CO)
                            # Then divide by approximate column height (~10km = 10000m)
                            co_value = (co_data * 1e6 * 28) / 10000
                            return co_value
                    except Exception as e:
                        print(f"Error extracting CO data for {date_str}: {e}")
                
                return None
            
            # Extract CO data for each date
            co_success = 0
            for date_str in date_range:
                try:
                    co_value = extract_co_data(date_str)
                    if co_value is not None:
                        idx = air_quality_data[air_quality_data['date'] == pd.to_datetime(date_str)].index
                        if len(idx) > 0:
                            air_quality_data.loc[idx, 'co'] = co_value
                            co_success += 1
                    time.sleep(0.1)
                except Exception as e:
                    print(f"Error extracting CO data for {date_str}: {e}")
            
            if co_success > 0:
                print(f"Successfully retrieved CO data for {co_success} out of {len(date_range)} days ({co_success/len(date_range)*100:.1f}%)")
            
        except Exception as e:
            print(f"Could not obtain CO data: {e}")
        
    except Exception as e:
        print(f"Error fetching air quality data: {e}")
        raise Exception("Air quality data collection failed. Please check your Earth Engine setup.")
    
    # Save data to CSV
    output_file = os.path.join(output_dir, f"{region_name.replace(' ', '_').lower()}_air_quality_data.csv")
    air_quality_data.to_csv(output_file, index=False)
    print(f"Air quality data saved to {output_file}")
    
    return air_quality_data

def analyze_air_quality(air_quality_data, output_dir="data"):
    """
    Analyze air quality data and generate summary statistics.
    
    Parameters:
    -----------
    air_quality_data : pandas.DataFrame
        DataFrame containing air quality data
    output_dir : str, optional
        Directory to save output files
    
    Returns:
    --------
    dict
        Dictionary containing summary statistics
    """
    print("Analyzing air quality data...")
    
    # Create summary statistics
    summary = {}
    pollutants = ['pm25', 'pm10', 'no2', 'o3', 'co']
    
    for pollutant in pollutants:
        if pollutant in air_quality_data.columns:
            data = air_quality_data[pollutant].dropna()
            if len(data) > 0:
                summary[pollutant] = {
                    'count': len(data),
                    'mean': data.mean(),
                    'median': data.median(),
                    'min': data.min(),
                    'max': data.max(),
                    'std': data.std()
                }
    
    # Calculate AQI (Air Quality Index) if PM2.5 and PM10 data are available
    if 'pm25' in air_quality_data.columns and 'pm10' in air_quality_data.columns:
        # Simple method to calculate AQI
        def calculate_aqi(row):
            if pd.isna(row['pm25']) and pd.isna(row['pm10']):
                return None
            
            aqi_pm25 = None
            if not pd.isna(row['pm25']):
                # PM2.5 AQI calculation (simplified)
                pm25 = row['pm25']
                if pm25 <= 12:
                    aqi_pm25 = (50/12) * pm25
                elif pm25 <= 35.4:
                    aqi_pm25 = 50 + (50/(35.4-12)) * (pm25-12)
                elif pm25 <= 55.4:
                    aqi_pm25 = 100 + (50/(55.4-35.4)) * (pm25-35.4)
                elif pm25 <= 150.4:
                    aqi_pm25 = 150 + (50/(150.4-55.4)) * (pm25-55.4)
                else:
                    aqi_pm25 = 200 + (100/(250.4-150.4)) * (pm25-150.4)
            
            aqi_pm10 = None
            if not pd.isna(row['pm10']):
                # PM10 AQI calculation (simplified)
                pm10 = row['pm10']
                if pm10 <= 54:
                    aqi_pm10 = (50/54) * pm10
                elif pm10 <= 154:
                    aqi_pm10 = 50 + (50/(154-54)) * (pm10-54)
                elif pm10 <= 254:
                    aqi_pm10 = 100 + (50/(254-154)) * (pm10-154)
                elif pm10 <= 354:
                    aqi_pm10 = 150 + (50/(354-254)) * (pm10-254)
                else:
                    aqi_pm10 = 200 + (100/(424-354)) * (pm10-354)
            
            # Take the maximum AQI value (worst case)
            if aqi_pm25 is not None and aqi_pm10 is not None:
                return max(aqi_pm25, aqi_pm10)
            elif aqi_pm25 is not None:
                return aqi_pm25
            else:
                return aqi_pm10
        
        air_quality_data['aqi'] = air_quality_data.apply(calculate_aqi, axis=1)
        
        # Add AQI category
        def aqi_category(aqi):
            if pd.isna(aqi):
                return None
            elif aqi <= 50:
                return "Good"
            elif aqi <= 100:
                return "Moderate"
            elif aqi <= 150:
                return "Unhealthy for Sensitive Groups"
            elif aqi <= 200:
                return "Unhealthy"
            elif aqi <= 300:
                return "Very Unhealthy"
            else:
                return "Hazardous"
        
        air_quality_data['aqi_category'] = air_quality_data['aqi'].apply(aqi_category)
        
        # Add AQI statistics to summary
        aqi_data = air_quality_data['aqi'].dropna()
        if len(aqi_data) > 0:
            summary['aqi'] = {
                'count': len(aqi_data),
                'mean': aqi_data.mean(),
                'median': aqi_data.median(),
                'min': aqi_data.min(),
                'max': aqi_data.max(),
                'std': aqi_data.std()
            }
            
            # Count days in each AQI category
            category_counts = air_quality_data['aqi_category'].value_counts().to_dict()
            summary['aqi_categories'] = category_counts
    
    # Save summary to JSON
    import json
    summary_file = os.path.join(output_dir, "air_quality_summary.json")
    with open(summary_file, 'w') as f:
        json.dump(summary, f, indent=4)
    
    print(f"Air quality summary saved to {summary_file}")
    
    return summary

# Example usage
if __name__ == "__main__":
    # Define region of interest (example: Johannesburg)
    region_name = "Johannesburg"
    coordinates = [28.0473, -26.2041]  # [longitude, latitude]
    
    # Define date range for your abstract
    start_date = "2023-01-01"
    end_date = "2023-12-31"
    
    # Create output directory
    output_dir = "air_quality_data"
    os.makedirs(output_dir, exist_ok=True)
    
    try:
        # Fetch air quality data
        air_quality_data = fetch_air_quality_data(
            region_name=region_name,
            coordinates=coordinates,
            start_date_str=start_date,
            end_date_str=end_date,
            output_dir=output_dir
        )
        
        # Analyze the data
        summary = analyze_air_quality(air_quality_data, output_dir=output_dir)
        
        # Display summary
        print("\nAir Quality Data Summary:")
        print(f"Total days analyzed: {len(air_quality_data)}")
        for pollutant, stats in summary.items():
            if pollutant not in ['aqi_categories']:
                print(f"\n{pollutant.upper()} statistics:")
                for stat, value in stats.items():
                    print(f"  {stat}: {value}")
        
        # Display sample data
        print("\nSample data:")
        print(air_quality_data.head())
        
    except Exception as e:
        print(f"ERROR: {e}")
        print("Air quality data collection failed. Please check your Earth Engine setup and try again.")
