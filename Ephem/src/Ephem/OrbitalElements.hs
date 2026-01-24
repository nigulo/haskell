module Ephem.OrbitalElements (
    Planet (..),
    OrbitalElements (..),
    ElementsByOrbitType (..),
    PeriapsisOrientation (..),
    EpochData (..),
    mercury1980,
    earth1980,
    jupiter1980,
    halley,
    kohler1977m,
    mcNaughtC2009R1,
    panstarrsC2011L4,
    moon1980,
    moon2000,
    moon2010,
    mercury2000,
    venus2000,
    earth2000,
    mars2000,
    jupiter2000,
    saturn2000,
    uranus2000,
    neptune2000,
    pluto2000,
    planets2000,
    -- J2020 elements from JPL Horizons DE441 (more accurate for 2020+ dates)
    -- Source: https://ssd.jpl.nasa.gov/horizons/
    mercury2020,
    venus2020,
    earth2020,
    mars2020,
    jupiter2020,
    saturn2020,
    uranus2020,
    neptune2020,
    pluto2020,
    planets2020
    ) where
    
import Ephem.Types
import Ephem.Coords

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune | Pluto

data OrbitalElements = OrbitalElements {
    eccentricity :: (Double, Double),
    inclination :: (Angle, Angle),
    longitudeOfAscedingNode :: (Angle, Angle),
    periapsisOrientation :: PeriapsisOrientation,
    elementsByOrbitType :: ElementsByOrbitType,
    epoch :: Date,
    epochData :: EpochData,
    angularDiameter :: Angle, -- ^ angular diameter at 1 A.U. 
    albedo :: Double -- astronomical albedo
}

data ElementsByOrbitType = 
    ClosedOrbitElements {
        semiMajorAxis :: (Distance, Distance), -- ^ a, a / cty
        period :: Double -- ^ period in tropical years
    }
    | OpenOrbitElements {
        distanceOfPeriapsis :: Double
    }

data PeriapsisOrientation = LongitudeOfPeriapsis (Angle, Angle) | ArgumentOfPeriapsis (Angle, Angle)

-- | In case epoch data does not define mean longitude period must always be defined for closed orbits
data EpochData = LongitudeAtEpoch (Angle, Angle) | MeanLongitude (Angle, Angle) | PerihelionPassage Date   

mercury1980 = OrbitalElements {
    eccentricity = (0.2056306, 0),
    inclination = (Deg 7.0043579, Deg 0),
    longitudeOfAscedingNode = (Deg 48.0941733, Deg 0),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 77.1442128, Deg 0),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 0.3870986, AU 0),
        period = 0.24085 -- ^ period in tropical years
    }, 
    epoch = ymd 1980 1 0,
    epochData = LongitudeAtEpoch (Deg 231.2973, Deg 0),
    angularDiameter = Sec 6.74,
    albedo = 1.918e-6
}

earth1980 = OrbitalElements {
    eccentricity = (0.016718, 0),
    inclination = (Deg 0, Deg 0),
    longitudeOfAscedingNode = (Deg 0, Deg 0),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 102.596403, Deg 0),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 1, AU 0), -- ^ in A.U.
        period = 1.00004
    },
    epoch = ymd 1980 1 0,
    epochData = LongitudeAtEpoch (Deg 98.833540, Deg 0)
}

jupiter1980 = OrbitalElements {
    eccentricity = (0.0484658, 0),
    inclination = (Deg 1.3041819, Deg 0),
    longitudeOfAscedingNode = (Deg 100.2520175, Deg 0),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 14.0095493, Deg 0),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 5.202561, AU 0), -- ^ in A.U.
        period = 11.86224 -- ^ period in tropical years
    },
    epoch = ymd 1980 1 0,
    epochData = LongitudeAtEpoch (Deg 146.966365, Deg 0),
    angularDiameter = Sec 196.74,
    albedo = 1.994e-4
}

halley = OrbitalElements {
    eccentricity = (0.9673, 0),
    inclination = (Deg 162.2384, Deg 0),
    longitudeOfAscedingNode = (Deg 58.1540, Deg 0),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 170.0110, Deg 0),
    epoch = ymd 1980 1 0,
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 17.9435, AU 0), -- ^ in A.U.
        period = 76.0081 -- ^ period in tropical years
    },
    epochData = PerihelionPassage (TropicalYears 1986.112)
}

kohler1977m = OrbitalElements {
    eccentricity = (1, 0),
    inclination = (Deg 48.7196, Deg 0),
    longitudeOfAscedingNode = (Deg 181.8175, Deg 0),
    periapsisOrientation = ArgumentOfPeriapsis (Deg 163.4799, Deg 0),
    epoch = ymd 1980 1 0,
    elementsByOrbitType = OpenOrbitElements {
        distanceOfPeriapsis = 0.990662 -- ^ in A.U.
    },
    epochData = PerihelionPassage (YMD 1977 11 10.5659)
}

panstarrsC2011L4 = OrbitalElements {
    eccentricity = (1.000028, 0),
    inclination = (Deg 84.2072, Deg 0),
    longitudeOfAscedingNode = (Deg 65.6658, Deg 0),
    periapsisOrientation = ArgumentOfPeriapsis (Deg 333.6517, Deg 0),
    epoch = ymd 2013 4 18, 
    elementsByOrbitType = OpenOrbitElements {
        distanceOfPeriapsis = 0.301541 -- ^ in A.U.
    },
    epochData = PerihelionPassage (YMD 2013 3 10.1696)
} 

mcNaughtC2009R1 = OrbitalElements {
    eccentricity = (1.000307, 0),
    inclination = (Deg 77.0765, Deg 0),
    longitudeOfAscedingNode = (Deg 322.4281, Deg 0),
    periapsisOrientation = ArgumentOfPeriapsis (Deg 130.7273, Deg 0),
    epoch = ymd 2013 4 18,
    elementsByOrbitType = OpenOrbitElements {
        distanceOfPeriapsis = 0.404991 -- ^ in A.U.
    },
    epochData = PerihelionPassage (YMD 2010 7 2.6603)
} 

-- cannot be used for calcMoon2
moon1980 = OrbitalElements {
    eccentricity = (0.0549, 0),
    inclination = (Deg 5.145396, Deg 0),
    longitudeOfAscedingNode = (Deg 151.950429, Deg (-0.0529539)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 349.383063, Deg 0.1114041),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (Km 384401, Km 0), -- ^ in km
        period = 27.322 -- ^ period in days
    },
    epoch = ymd 1980 1 0,
    epochData = MeanLongitude (Deg 64.975464, Deg 13.176396),
    angularDiameter = Deg 0.5181 -- ^ at the distance of a (semi-major axis)
    
}

-- cannot be used for calcMoon
moon2000 = OrbitalElements {
    eccentricity = (0.0549, 0),
    inclination = (Deg 5.1454, Deg 0),
    longitudeOfAscedingNode = (Deg 125.1228, Deg (-0.0529538083)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 318.0634, Deg 0.1643573223),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (EarthRadii 60.2666, EarthRadii 0),
        period = 27.322 -- ^ period in days
    },
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 115.3654, Deg 13.0649929509),
    angularDiameter = Deg 0.5181 -- ^ at the distance of a (semi-major axis)

}

-- | Moon orbital elements for J2010 epoch
-- Base values from NASA JPL: https://ssd.jpl.nasa.gov/sats/elem/
-- Rates from NASA eclipse page: https://eclipse.gsfc.nasa.gov/SEhelp/moonorbit.html
-- Node rate: -0.05295°/day (westward), Perigee rate: +0.11140°/day (eastward)
-- Values calculated from J2000 epoch (2000-01-01.5) to J2010 epoch (2010-01-01.0)
moon2010 = OrbitalElements {
    eccentricity = (0.0549, 0),
    inclination = (Deg 5.145396, Deg 0),
    -- J2000 node: 125.08° - (0.05295 * 3651.5 days) = 291.68°
    longitudeOfAscedingNode = (Deg 291.682547, Deg (-0.0529538083)),
    -- J2000 perigee longitude: 83.23° + (0.11140 * 3651.5 days) = 130.14°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 130.143076, Deg 0.1114040800),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (Km 384401, Km 0),
        period = 27.322 -- ^ period in days
    },
    epoch = ymd 2010 1 0,
    -- J2000 mean longitude: ~100° + (13.176396 * 3651.5 days) = 91.93° (mod 360)
    epochData = MeanLongitude (Deg 91.929336, Deg 13.176396),
    angularDiameter = Deg 0.5181 -- ^ at the distance of a (semi-major axis)
}

-- | Keplerian elements and their rates, with respect to the mean ecliptic
-- | and equinox of J2000, valid for the time-interval 1800 AD - 2050 AD.

mercury2000 = OrbitalElements {
    eccentricity = (0.20563593, 0.00001906),
    inclination = (Deg 7.00497902, Deg (-0.00594749)),
    longitudeOfAscedingNode = (Deg 48.33076593, Deg (-0.12534081)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 77.45779628, Deg 0.16047689),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 0.38709927, AU 0.00000037)
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 252.25032350, Deg 149472.67411175),
    angularDiameter = Sec 6.74,
    albedo = 1.918e-6
}

venus2000 = OrbitalElements {
    eccentricity = (0.00677672, (-0.00004107)),
    inclination = (Deg 3.39467605, Deg (-0.00078890)),
    longitudeOfAscedingNode = (Deg 76.67984255, Deg (-0.27769418)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 131.60246718, Deg 0.00268329),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 0.72333566, AU 0.00000390)
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 181.97909950, Deg 58517.81538729),
    angularDiameter = Sec 16.92,
    albedo = 1.721e-5
}

earth2000 = OrbitalElements {
    eccentricity = (0.01671022, (-0.00003804)),
    inclination = (Deg 0.00005, Deg (-0.0130388888889)),
    longitudeOfAscedingNode = (Deg (-11.26064), Deg (-5.06340277778)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 102.94719, Deg 0.388331481481),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 1.00000011, AU (-0.00000005))
    }, 
    epoch = ymd 2000 1 0.5,
    epochData = MeanLongitude (Deg 100.46435, Deg 35999.3723972)
}

mars2000 = OrbitalElements {
    eccentricity = (0.09339410, 0.00007882),
    inclination = (Deg 1.84969142, Deg (-0.00813131)),
    longitudeOfAscedingNode = (Deg 49.55953891, Deg (-0.29257343)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg (-23.94362959), Deg 0.44441088),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 1.52371034, AU 0.00001847)
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg (-4.55343205), Deg 19140.30268499),
    angularDiameter = Sec 9.36,
    albedo = 4.539e-6
}

jupiter2000 = OrbitalElements {
    eccentricity = (0.04838624, -0.00013253),
    inclination = (Deg 1.30439695, Deg (-0.00183714)),
    longitudeOfAscedingNode = (Deg 100.47390909, Deg 0.20469106),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 14.72847983, Deg 0.21252668),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 5.20288700, AU  (-0.00011607))
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 34.39644051, Deg 3034.74612775),
    angularDiameter = Sec 196.74,
    albedo = 1.994e-4
}

saturn2000 = OrbitalElements {
    eccentricity = (0.05386179, -0.00050991),
    inclination = (Deg 2.48599187, Deg 0.00193609),
    longitudeOfAscedingNode = (Deg 113.66242448, Deg (-0.28867794)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 92.59887831, Deg (-0.41897216)),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 9.53667594, AU (-0.00125060))
    },
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 49.95424423, Deg 1222.49362201),
    angularDiameter = Sec 165.60,
    albedo = 1.740e-4
}

-- ===========================================================================
-- J2020 Orbital Elements from JPL Horizons DE441
-- Source: https://ssd.jpl.nasa.gov/horizons/
-- Ephemeris: DE441 (released April 2021)
-- Query parameters: EPHEM_TYPE='ELEMENTS', CENTER='500@10', START_TIME='2020-01-01'
-- Reference: Park et al. 2021, "The JPL Planetary and Lunar Ephemerides DE440 and DE441"
-- ===========================================================================

-- | Mercury osculating orbital elements for J2020.0 epoch
mercury2020 :: OrbitalElements
mercury2020 = OrbitalElements {
    eccentricity = (0.2056503, 0.00001906),  -- rate from J2000
    inclination = (Deg 7.0038, Deg (-0.00594749)),
    longitudeOfAscedingNode = (Deg 48.307, Deg (-0.12534081)),
    -- W=29.18°, so longitude of perihelion = 48.307 + 29.18 = 77.49°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 77.49, Deg 0.16047689),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 0.3871, AU 0.00000037)
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = 77.49 + 187.25 = 264.74°
    epochData = MeanLongitude (Deg 264.74, Deg 149472.67411175),
    angularDiameter = Sec 6.74,
    albedo = 1.918e-6
}

-- | Venus osculating orbital elements for J2020.0 epoch
venus2020 :: OrbitalElements
venus2020 = OrbitalElements {
    eccentricity = (0.006745, (-0.00004107)),
    inclination = (Deg 3.3946, Deg (-0.00078890)),
    longitudeOfAscedingNode = (Deg 76.625, Deg (-0.27769418)),
    -- W=54.91°, so longitude of perihelion = 76.625 + 54.91 = 131.53°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 131.53, Deg 0.00268329),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 0.7233, AU 0.00000390)
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = 131.53 + 232.32 = 363.85° → 3.85°
    epochData = MeanLongitude (Deg 3.85, Deg 58517.81538729),
    angularDiameter = Sec 16.92,
    albedo = 1.721e-5
}

-- | Earth osculating orbital elements for J2020.0 epoch
earth2020 :: OrbitalElements
earth2020 = OrbitalElements {
    eccentricity = (0.01712, (-0.00003804)),
    inclination = (Deg 0.00278, Deg (-0.0130388888889)),
    longitudeOfAscedingNode = (Deg 159.70, Deg (-5.06340277778)),
    -- W=304.36°, so longitude of perihelion = 159.70 + 304.36 = 464.06° → 104.06°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 104.06, Deg 0.388331481481),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 1.0001, AU (-0.00000005))
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = 104.06 + 355.83 = 459.89° → 99.89°
    epochData = MeanLongitude (Deg 99.89, Deg 35999.3723972)
}

-- | Mars osculating orbital elements for J2020.0 epoch
mars2020 :: OrbitalElements
mars2020 = OrbitalElements {
    eccentricity = (0.0935, 0.00007882),
    inclination = (Deg 1.848, Deg (-0.00813131)),
    longitudeOfAscedingNode = (Deg 49.50, Deg (-0.29257343)),
    -- W=286.68°, so longitude of perihelion = 49.50 + 286.68 = 336.18°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 336.18, Deg 0.44441088),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 1.5237, AU 0.00001847)
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = 336.18 + 247.07 = 583.25° → 223.25°
    epochData = MeanLongitude (Deg 223.25, Deg 19140.30268499),
    angularDiameter = Sec 9.36,
    albedo = 4.539e-6
}

-- | Jupiter osculating orbital elements for J2020.0 epoch
jupiter2020 :: OrbitalElements
jupiter2020 = OrbitalElements {
    -- JPL Horizons: EC=0.04876
    eccentricity = (0.04876, -0.00013253),  -- rate from J2000
    -- JPL Horizons: IN=1.3038°
    inclination = (Deg 1.3038, Deg (-0.00183714)),  -- rate from J2000
    -- JPL Horizons: OM=100.52°
    longitudeOfAscedingNode = (Deg 100.52, Deg 0.20469106),  -- rate from J2000
    -- JPL Horizons: W=273.70°, so longitude of perihelion = 100.52 + 273.70 = 14.22° (mod 360)
    periapsisOrientation = LongitudeOfPeriapsis (Deg 14.22, Deg 0.21252668),  -- rate from J2000
    elementsByOrbitType = ClosedOrbitElements {
        -- JPL Horizons: A=7.7829e8 km = 5.2026 AU
        semiMajorAxis = (AU 5.2026, AU (-0.00011607))  -- rate from J2000
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = longitude of perihelion + mean anomaly = 14.22 + 267.12 = 281.34°
    epochData = MeanLongitude (Deg 281.34, Deg 3034.74612775),  -- rate from J2000
    angularDiameter = Sec 196.74,
    albedo = 1.994e-4
}

-- | Saturn osculating orbital elements for J2020.0 epoch
-- Source: JPL Horizons API (https://ssd.jpl.nasa.gov/horizons/)
-- Query: COMMAND='699', EPHEM_TYPE='ELEMENTS', CENTER='500@10', START_TIME='2020-01-01'
-- More accurate for dates near 2020 than J2000 elements
saturn2020 :: OrbitalElements
saturn2020 = OrbitalElements {
    -- JPL Horizons: EC=0.0509
    eccentricity = (0.0509, -0.00050991),  -- rate from J2000
    -- JPL Horizons: IN=2.490°
    inclination = (Deg 2.490, Deg 0.00193609),  -- rate from J2000
    -- JPL Horizons: OM=113.59°
    longitudeOfAscedingNode = (Deg 113.59, Deg (-0.28867794)),  -- rate from J2000
    -- JPL Horizons: W=337.76°, so longitude of perihelion = 113.59 + 337.76 = 91.35° (mod 360)
    periapsisOrientation = LongitudeOfPeriapsis (Deg 91.35, Deg (-0.41897216)),  -- rate from J2000
    elementsByOrbitType = ClosedOrbitElements {
        -- JPL Horizons: A=1.4334e9 km = 9.5819 AU
        semiMajorAxis = (AU 9.5819, AU (-0.00125060))  -- rate from J2000
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = longitude of perihelion + mean anomaly = 91.35 + 203.03 = 294.38°
    epochData = MeanLongitude (Deg 294.38, Deg 1222.49362201),  -- rate from J2000
    angularDiameter = Sec 165.60,
    albedo = 1.740e-4
}

-- | Uranus osculating orbital elements for J2020.0 epoch
uranus2020 :: OrbitalElements
uranus2020 = OrbitalElements {
    eccentricity = (0.04665, -0.00004397),
    inclination = (Deg 0.7721, Deg (-0.00242939)),
    longitudeOfAscedingNode = (Deg 74.044, Deg 0.04240589),
    -- W=98.97°, so longitude of perihelion = 74.044 + 98.97 = 173.01°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 173.01, Deg 0.40805281),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 19.17, AU (-0.00196176))
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = 173.01 + 225.70 = 398.71° → 38.71°
    epochData = MeanLongitude (Deg 38.71, Deg 428.48202785),
    angularDiameter = Sec 65.80,
    albedo = 7.768e-5
}

-- | Neptune osculating orbital elements for J2020.0 epoch
neptune2020 :: OrbitalElements
neptune2020 = OrbitalElements {
    eccentricity = (0.00968, 0.00005105),
    inclination = (Deg 1.774, Deg 0.00035372),
    longitudeOfAscedingNode = (Deg 131.87, Deg (-0.00508664)),
    -- W=247.54°, so longitude of perihelion = 131.87 + 247.54 = 379.41° → 19.41°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 19.41, Deg (-0.32241464)),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 30.18, AU 0.00026291)
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = 19.41 + 328.91 = 348.32°
    epochData = MeanLongitude (Deg 348.32, Deg 218.45945325),
    angularDiameter = Sec 62.20,
    albedo = 5.765e-5
}

-- | Pluto osculating orbital elements for J2020.0 epoch
-- Note: Pluto elements from PLU060/DE440 solution include post-New Horizons data
pluto2020 :: OrbitalElements
pluto2020 = OrbitalElements {
    eccentricity = (0.2496, 0.00449747),
    inclination = (Deg 16.88, Deg (-0.00349556)),
    longitudeOfAscedingNode = (Deg 110.27, Deg (-0.01262424)),
    -- W=114.62°, so longitude of perihelion = 110.27 + 114.62 = 224.89°
    periapsisOrientation = LongitudeOfPeriapsis (Deg 224.89, Deg (-0.04062942)),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 39.64, AU 0.00449024)
    },
    epoch = ymd 2020 1 1,
    -- Mean longitude = 224.89 + 43.21 = 268.10°
    epochData = MeanLongitude (Deg 268.10, Deg 145.18042903),
    angularDiameter = Sec 1.00,
    albedo = 1.000e-6
}

uranus2000 = OrbitalElements {
    eccentricity = (0.04725744, -0.00004397),
    inclination = (Deg 0.77263783, Deg (-0.00242939)),
    longitudeOfAscedingNode = (Deg 74.01692503, Deg 0.04240589),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 170.95427630, Deg 0.40805281),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 19.18916464, AU (-0.00196176))
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 313.23810451, Deg 428.48202785),
    angularDiameter = Sec 65.80,
    albedo = 7.768e-5
}

neptune2000 = OrbitalElements {
    eccentricity = (0.00859048, 0.00005105),
    inclination = (Deg 1.77004347, Deg 0.00035372),
    longitudeOfAscedingNode = (Deg 131.78422574, Deg (-0.00508664)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 44.96476227, Deg (-0.32241464)),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 30.06992276, AU 0.00026291)
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg (-55.12002969), Deg 218.45945325),
    angularDiameter = Sec 62.20,
    albedo = 7.597e-5
}

pluto2000 = OrbitalElements {
    eccentricity = (0.24882730, 0.00005170),
    inclination = (Deg 17.14001206, Deg 0.00004818),
    longitudeOfAscedingNode = (Deg 110.30393684, Deg (-0.01183482)),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 224.06891629, Deg (-0.04062942)),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 39.48211675, AU (-0.00031596))
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 238.92903833, Deg 145.20780515),
    angularDiameter = Sec 8.20,
    albedo = 4.073e-6
}

planets2000 = [
        (Mercury, mercury2000),
        (Venus, venus2000),
        (Mars, mars2000),
        (Jupiter, jupiter2000),
        (Saturn, saturn2000),
        (Uranus, uranus2000),
        (Neptune, neptune2000),
        (Pluto, pluto2000)
    ]

-- | All planets with J2020 osculating orbital elements from JPL Horizons DE441
planets2020 :: [(Planet, OrbitalElements)]
planets2020 = [
        (Mercury, mercury2020),
        (Venus, venus2020),
        (Mars, mars2020),
        (Jupiter, jupiter2020),
        (Saturn, saturn2020),
        (Uranus, uranus2020),
        (Neptune, neptune2020),
        (Pluto, pluto2020)
    ]
    