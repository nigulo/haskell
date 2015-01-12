module Astro.Ephem.Coords (
    RA,
    LHA,
    Dec,
    Alt,
    Azi,
    EcLong,
    EcLat,
    raToLHA,
    lhaToRA,
    equToHor,
    horToEqu,
    calcObliquityOfEcliptic,
    eclToEqu,
    equToEcl,
    calcRiseSet,
    calcRiseSetCorrection,
    calcGeoParallax
    ) where

import Astro.Ephem.Utils
import Astro.Ephem.Types
import Astro.Ephem.Time
import Debug.Trace

type RA = Hours -- ^ right ascension
type LHA = Hours -- ^ local hour angle
type Dec = Angle -- ^ declination
type Alt = Angle -- ^ altitude
type Azi = Angle -- ^ azimuth
type EcLong = Angle -- ^ ecliptic longitude
type EcLat = Angle -- ^ ecliptic latitude

raToLHA :: RA -> LST -> LHA
raToLHA ra lst = 
    let
        Hrs hrsLST = toHrs lst
        Hrs hrsRA = toHrs ra
        lha = hrsLST - hrsRA
    in
        Hrs (if lha >= 0 then lha else lha + 24)
    
lhaToRA :: LHA -> LST -> RA
lhaToRA lha lst =
    let
        Hrs hrsLHA = toHrs lha
        Hrs hrsLST = toHrs lst
        ra = hrsLST - hrsLHA
    in
        Hrs (if ra >= 0 then ra else ra + 24)

equToHor :: LHA -> Dec -> Lat -> (Alt, Azi)
equToHor lha dec (Lat lat latType)  =
    let
        h = hoursToAngle lha
        sinDec = sine dec
        sinLat = sine lat
        cosLat = cosine lat
        sinAlt = sinDec * sinLat + cosine dec * cosLat * cosine h
        alt = asin sinAlt
        cosAlt = cos alt
        cosAzi = (sinDec - sinLat * sinAlt) / (cosLat * cosAlt)
        azi' = acos cosAzi
        azi = if sine h <= 0 then azi' else 2 * pi - azi'
    in
        (Rad alt, Rad azi)

horToEqu :: Alt -> Azi -> Lat -> (LHA, Dec)
horToEqu alt azi (Lat lat latType) = 
    let
        sinAlt = sine alt
        sinLat = sine $ case latType of
            N -> lat
            S -> neg lat
        
        cosLat = cosine lat
        sinDec = sinAlt * sinLat + cosine alt * cosLat * cosine azi
        dec = asin sinDec
        cosH = (sinAlt - sinLat * sinDec) / (cosLat * cos dec)
        h' = acos cosH
        h = if sine azi <= 0 then h' else 2 * pi - h'
    in 
        (angleToHours (Rad h), Rad dec)

calcObliquityOfEcliptic :: 
    Date  
    -> Angle
calcObliquityOfEcliptic date =
    let
        JD jd = toJD date
        JD jd2000 = j2000  
        t = (jd - jd2000) / 36525 -- ^ Julian centuries from J2000.0
        --y = jd - 2415020
    in
        --(toSec (DMS 23 26 21.406)) `subtr` Sec ((46.836769 + (0.0001831 - (0.00200340 - (0.576e-6 + 4.34e-8 * t) * t) * t) * t) * t)
        (toSec (DMS 23 26 21.45)) `subtr` Sec ((46.815 + (0.0006 - 0.00181 * t) * t) * t)

eclToEqu ::
    EcLong
    -> EcLat
    -> Angle -- ^ Earth's axial tilt
    -> (RA, Dec)
eclToEqu long lat tilt = 
    let
        cosTilt = cosine tilt
        sinTilt = sine tilt
        sinLong = sine long
        sinDec = sine lat * cosTilt + cosine lat * sinTilt * sinLong
        dec = asin sinDec 
        y = sinLong * cosTilt - tangent lat * sinTilt
        x = cosine long
        --ra' = atan (y / x)
        --ra = if (y > 0 && x < 0) || (y < 0 && x < 0) then ra' + pi else ra'
        ra' = atan2 y x
        ra = if ra' < 0 then ra' + 2 * pi else ra'
    in
        (angleToHours (Rad ra), Rad dec)
        
equToEcl ::
    RA
    -> Dec
    -> Angle -- ^ Earth's axial tilt
    -> (EcLong, EcLat)
equToEcl ra dec tilt = 
    let
        cosTilt = cosine tilt
        sinTilt = sine tilt
        raAngle = hoursToAngle ra
        sinRA = sine raAngle
        sinLat = sine dec * cosTilt - cosine dec * sinTilt * sinRA
        lat = asin sinLat
        y = sinRA * cosTilt + tangent dec * sinTilt
        x = cosine raAngle
        --long' = atan (y / x)
        --long = if (y > 0 && x < 0) || (y < 0 && x < 0) then long' + pi else long'
        long' = atan2 y x
        long = if long' < 0 then long' + 2 * pi else long'
    in
        (Rad long, Rad lat)
           
    
calcRiseSet ::
    RA
    -> Dec
    -> Lat
    -> Bool -- ^ whether to add correction due to atmospheric refraction    
    -> Maybe ((LST, Azi), (LST, Azi)) -- ^ (rise time, azimuth), (set time, azimuth)   
calcRiseSet ra dec lat@(Lat l latType) useRefraction =
    let
        latAngle = case latType of 
            N -> l 
            S -> neg l
        cosAziRise = sine dec / cosine latAngle
        Deg latDeg = toDeg latAngle
        Deg decDeg = toDeg dec
        cosH = -(tangent latAngle * tangent dec)
    in
        if cosAziRise < -1 || cosAziRise > 1 || abs latDeg == 90 || abs decDeg == 90 || cosH < -1 || cosH > 1 
            then Nothing
                else
                  let
                        aziRise = clipAngleRad (acos cosAziRise)
                        aziSet = clipAngleRad (pi2 - aziRise)
                        Hrs h = toHrs $ angleToHours $ Rad $ acos cosH
                        Hrs raHrs = toHrs ra 
                        lstRise = 24 + raHrs - h
                        lstSet = raHrs + h
                    in
                        case useRefraction of
                            False ->
                                Just ((Hrs (clipHour lstRise), Rad aziRise), (Hrs (clipHour lstSet), Rad aziSet))
                            True ->
                                let
                                    x = Deg 0.56666666667 -- ~34'
                                    (Hrs deltaT, Rad deltaA) = calcRiseSetCorrection dec lat x
                                in        
                                    Just ((Hrs (clipHour (lstRise - deltaT)), Rad (aziRise - deltaA)), (Hrs (clipHour (lstSet + deltaT)), Rad (aziSet + deltaA)))
                     
-- | Calculates corrections for rise and set time and azimuth
calcRiseSetCorrection ::
    Dec -- ^ declination of the object
    -> Lat -- ^ latitude of the observer
    -> Angle -- ^ vertical correction
    -> (Hours, Angle) -- ^ correction for time (in hours) and azimuth (in radians)
calcRiseSetCorrection dec (Lat l latType) x = 
    let
        lat = case latType of 
            N -> l 
            S -> neg l
        cosDec = cosine dec
        phi = acos (sine lat / cosDec)
        Deg y = toDeg $ Rad $ asin (sine x / sin phi)
        deltaA = asin (tangent x / tan phi)
        deltaT = 240 * y / cosDec / 3600
    in        
        (Hrs deltaT, Rad deltaA)
                     
calcGeoParallax ::
    GMT
    -> Date
    -> RA
    -> Dec
    -> Either Distance Angle -- ^ distance of celestial body from the center of the earth in km's or equatorial horizontal parallax
    -> Lat
    -> Long
    -> Double -- ^ altitude in meters above sea level
    -> (RA, Dec)
calcGeoParallax gmt date ra dec r' (Lat la latType) long height =
    let
        lat = case latType of 
            N -> la 
            S -> neg la
    
        u = atan (0.996647 * tangent lat)
        height' = height / 6378140
        rhoSinPhi' = 0.996647 * sin u + height' * sine lat
        rhoCosPhi' = cos u + height' * cosine lat
        lst = gstToLST (gmtToGST gmt date) long
        h = hoursToAngle $ raToLHA ra lst
        cosH = cosine h
        r = case r' of 
            Left (Km dist) -> dist / 6378.14
            Left dist@(AU _) -> d / 6378.14 where Km d = toKm dist  
            Right p -> 1 / sine p
        delta = atan ((rhoCosPhi' * sine h) / (r * cosine dec - rhoCosPhi' * cosH))
        h' = h `add` Rad delta
        Hrs deltaHours = toHrs $ angleToHours (Rad delta)
        Hrs raHrs = toHrs ra 
        ra' = raHrs - deltaHours
        dec' = atan (cosine h' * (r * sine dec - rhoSinPhi') / (r * cosine dec * cosine h - rhoCosPhi'))
    in
        (Hrs ra', Rad dec')
