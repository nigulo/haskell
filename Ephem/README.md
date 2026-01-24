# Ephem

A Haskell library for astronomical ephemeris calculations, including positions of the Sun, Moon, and planets, as well as rise/set times for celestial objects.

## Features

- **Sun calculations**: Position, rise/set times, twilight times
- **Moon calculations**: Position, phases, illumination, rise/set times, distance, angular diameter
- **Planet calculations**: Positions and rise/set times for Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, and Pluto
- **Coordinate transformations**: Ecliptic, equatorial, horizontal coordinate systems
- **Time conversions**: Julian Date, sidereal time, UTC/UT conversions
- **Comet and asteroid support**: Parabolic and elliptical orbit calculations

## Modules

| Module | Description |
|--------|-------------|
| `Ephem.Sun` | Solar position and rise/set calculations |
| `Ephem.Moon` | Lunar position, phases, and rise/set calculations |
| `Ephem.CelestialBody` | Generic celestial body calculations (planets, comets, asteroids) |
| `Ephem.OrbitalElements` | Orbital elements for planets and the Moon |
| `Ephem.Coords` | Coordinate system transformations |
| `Ephem.Time` | Time and date conversions |
| `Ephem.Types` | Core data types (angles, distances, coordinates) |
| `Ephem.Utils` | Utility functions |

## Algorithms and References

This library implements algorithms from two authoritative sources in computational astronomy:

### Jean Meeus, "Astronomical Algorithms"

**Full citation**: Meeus, Jean. *Astronomical Algorithms*. 2nd Edition. Richmond, Virginia: Willmann-Bell, Inc., 1998. ISBN 0-943396-61-1.

Algorithms from this book are used for high-precision calculations:

| Chapter | Algorithm | Function |
|---------|-----------|----------|
| Chapter 15 | Rising, Transit, and Setting | `calcMoonRiseSetMeeus` |
| Chapter 22 | Nutation and the Obliquity of the Ecliptic | `calcObliquityOfEcliptic` |
| Chapter 25 | Solar Coordinates | Various sun position calculations |
| Chapter 47 | Position of the Moon | `calcMoonMeeus` |
| Table 47.A | Periodic terms for lunar longitude and distance | `table47A` |
| Table 47.B | Periodic terms for lunar latitude | `table47B` |

The lunar position algorithm (`calcMoonMeeus`) is based on the ELP-2000/82 theory and provides accuracy of approximately 10 arcseconds in longitude and 4 arcseconds in latitude.

### Peter Duffett-Smith, "Practical Astronomy with your Calculator"

**Full citation**: Duffett-Smith, Peter. *Practical Astronomy with your Calculator*. 3rd Edition. Cambridge: Cambridge University Press, 1988. ISBN 0-521-35699-7.

This book provides the foundation for many of the simpler calculations and the 1980-epoch orbital elements:

- Basic coordinate transformations (ecliptic to equatorial, equatorial to horizontal)
- Rise and set time calculations
- Moon phase calculations
- Orbital elements for epoch 1980 (`earth1980`, `moon1980`, `mercury1980`, `jupiter1980`, etc.)

## Orbital Elements

The library includes orbital elements for multiple epochs:

| Epoch | Elements | Source |
|-------|----------|--------|
| J1980.0 | Sun, Moon, planets | Duffett-Smith |
| J2000.0 | Planets | NASA JPL (Standish & Williams 1992) |
| J2010.0 | Moon | NASA JPL / Hackage astro |
| J2020.0 | Planets | JPL Horizons DE441 (Park et al. 2021) |

### J2020 Planetary Elements

The J2020 osculating orbital elements provide improved accuracy for dates near 2020 and beyond. They were obtained from the [JPL Horizons system](https://ssd.jpl.nasa.gov/horizons/) using the DE441 planetary ephemeris.

**Reference**: Park, R.S., et al. (2021). "The JPL Planetary and Lunar Ephemerides DE440 and DE441". *The Astronomical Journal*, 161:105. [DOI: 10.3847/1538-3881/abd414](https://doi.org/10.3847/1538-3881/abd414)

The DE440/DE441 ephemeris includes:
- Juno radio ranging data for improved Jupiter orbit
- Cassini mission data for improved Saturn orbit
- Post-New Horizons encounter data for Pluto (PLU060 solution)

**Query parameters used**:
```
EPHEM_TYPE='ELEMENTS'
CENTER='500@10' (Sun, body center)
START_TIME='2020-01-01'
```

## Usage Example

```haskell
import Ephem.Sun
import Ephem.Moon
import Ephem.CelestialBody
import Ephem.OrbitalElements
import Ephem.Types

-- Calculate sunrise and sunset for a location
let date = YMD 2026 1 24
    lat = Lat (Deg 58.3743) N   -- Tartu, Estonia
    lon = Long (Deg 26.6893) E

-- Sun rise/set (using J2020 Earth elements for best accuracy)
sunRiseSet = calcSunRiseSet date earth2020 lat lon

-- Moon position (high precision Meeus algorithm)
(moonLong, moonLat, moonDist) = calcMoonMeeus date

-- Moon rise/set
moonRiseSet = calcMoonRiseSetMeeus date lat lon 3

-- Planet rise/set (Saturn example with J2020 elements)
saturnRiseSet = calcPlanetRiseSet date saturn2020 earth2020 lat lon
```

## Building

```bash
cabal build
```

## Testing

The test suite includes verification against:
- Example calculations from Meeus's book
- NASA JPL Horizons ephemeris data

```bash
cabal test
```

## Related Projects

- **EphemAPI**: A REST API wrapper around this library for web applications

## License

This is free and unencumbered software released into the public domain (The Unlicense <https://unlicense.org>).

## Author

nigulo
