module Astro.Ephem.OrbitalElements (
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
    mercury2000,
    venus2000,
    earth2000,
    earth2000a,
    mars2000,
    jupiter2000,
    saturn2000,
    uranus2000,
    neptune2000,
    pluto2000,
    planets2000
    ) where
    
import Astro.Ephem.Types
import Astro.Ephem.Coords

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
    eccentricity = (0.01671123, (-0.00004392)),
    inclination = (Deg (-0.00001531), Deg (-0.01294668)),
    longitudeOfAscedingNode = (Deg 0, Deg 0),
    periapsisOrientation = LongitudeOfPeriapsis (Deg 102.93768193, Deg 0.32327364),
    elementsByOrbitType = ClosedOrbitElements {
        semiMajorAxis = (AU 1.00000261, AU 0.00000562)
    }, 
    epoch = ymd 2000 1 0,
    epochData = MeanLongitude (Deg 100.46457166, Deg 35999.37244981)
}


earth2000a = OrbitalElements {
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
    