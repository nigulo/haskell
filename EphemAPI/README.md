# Ephem API

A web API built with Scotty (Haskell web framework) that calculates sunrise and sunset times for given coordinates and date ranges using the Ephem astronomical library.

## Features

- Calculate sunrise and sunset times for any location on Earth
- Supports custom date ranges or defaults to current month
- Returns data in JSON format
- Includes azimuth angles for sunrise and sunset
- CORS enabled for cross-origin requests

## Building

```bash
cd EphemAPI
cabal build
```

## Running

```bash
cabal run EphemAPI
```

The server will start on port 3000.

## API Endpoints

### GET /api/sunrise-sunset

Calculate sunrise and sunset times for a given location and date range.

#### Query Parameters

- `lat` (required): Latitude in decimal degrees (-90 to 90, positive for North, negative for South)
- `lon` (required): Longitude in decimal degrees (-180 to 180, positive for East, negative for West)
- `startDate` (optional): Start date in YYYY-MM-DD format
- `endDate` (optional): End date in YYYY-MM-DD format

If `startDate` and `endDate` are omitted, the API returns data for the current month.

#### Example Requests

1. **Current month for Tallinn, Estonia:**
   ```
   http://localhost:3000/api/sunrise-sunset?lat=58.3780&lon=26.7290
   ```

2. **Custom date range for London, UK:**
   ```
   http://localhost:3000/api/sunrise-sunset?lat=51.5074&lon=-0.1278&startDate=2024-01-01&endDate=2024-01-31
   ```

3. **Current month for New York, USA:**
   ```
   http://localhost:3000/api/sunrise-sunset?lat=40.7128&lon=-74.0060
   ```

#### Response Format

The API returns a JSON array of objects, one for each day in the requested range:

```json
[
  {
    "date": "2024-01-01",
    "sunrise": "08:23:45",
    "sunset": "15:42:18",
    "sunriseAzimuth": 128.5,
    "sunsetAzimuth": 231.5
  },
  {
    "date": "2024-01-02",
    "sunrise": "08:23:30",
    "sunset": "15:43:45",
    "sunriseAzimuth": 128.3,
    "sunsetAzimuth": 231.7
  }
]
```

For dates where sunrise/sunset doesn't occur (e.g., polar regions):

```json
[
  {
    "date": "2024-01-01",
    "error": "No sunrise/sunset"
  }
]
```

## Using with curl

```bash
# Get current month data
curl "http://localhost:3000/api/sunrise-sunset?lat=58.3780&lon=26.7290"

# Get data for January 2024
curl "http://localhost:3000/api/sunrise-sunset?lat=58.3780&lon=26.7290&startDate=2024-01-01&endDate=2024-01-31"
```

## Notes

- Times are returned in GMT/UTC
- Azimuth angles are in degrees (0° = North, 90° = East, 180° = South, 270° = West)
- The API uses the Earth2000 orbital elements model from the Ephem library
- Atmospheric refraction is accounted for in the calculations
