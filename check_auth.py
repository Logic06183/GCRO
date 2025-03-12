import ee

try:
    ee.Initialize()
    print('Successfully authenticated with Google Earth Engine')
except Exception as e:
    print(f'Authentication needed: {e}')
