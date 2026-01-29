module Ephem.Types (
    SphericalCoord (..),
    Hours (..),
    Angle (..),
    LongType (..),
    LatType (..),
    Long (..),
    Lat (..),
    Date (..),
    Distance (..),
    toHMS,
    toHrs,
    toDMS,
    degToRad,
    radToDeg,
    toDeg,
    toRad,
    toSec,
    angleToHours,
    hoursToAngle,
    toValidYMD,
    ymd,
    toYMD,
    toTropicalYears,
    toJD,
    toRJD,
    toMJD,
    toTJD,
    getDayOfMonth,
    diffDays,
    j2000,
    toAU,
    toKm,
    toEarthRadii,
    absAngle,
    sgnAngle,
    toLatitude,
    toLongitude,
    addDays,
    splitDayAndTime,
    getDay,
    getHours,
    toDays,
    numCenturies,
    fromDateAndHours
) where

import Data.Time.Calendar hiding (diffDays, addDays)
import Data.Time.Calendar.OrdinalDate

class SphericalCoord a where
    add :: a -> a -> a
    subtr :: a -> a -> a
    neg :: a -> a
    mul :: a -> Double -> a
    sine :: a -> Double
    cosine :: a -> Double
    tangent :: a -> Double
    
data Angle = 
    DMS Int Int Double
    | Deg Double
    | Rad Double
    | Sec Double
    deriving (Eq, Show)

data LongType = E | W
    deriving (Eq, Show)

data LatType = N | S
    deriving (Eq, Show)

data Lat = Lat Angle LatType 
    deriving (Eq, Show)
data Long = Long Angle LongType
    deriving (Eq, Show)

data Hours =
    HMS Int Int Double
    | Hrs Double
    deriving (Eq, Show)

data Date =
    YMD Integer Int Double
    | TropicalYears Double
    | JD Double
    | RJD Double -- ^ reduced JD
    | MJD Double -- ^ modified JD
    | TJD Double -- ^ truncated JD
    deriving (Eq, Show, Ord)

data Distance = AU Double | Km Double | EarthRadii Double

instance SphericalCoord Angle where
    add angle1@(DMS _ _ _) angle2 = toDMS $ add (toDeg angle1) (toDeg angle2)
    add (Deg x1) (Deg x2) = Deg (x1 + x2)
    add angle1@(Deg _) angle2 = add angle1 (toDeg angle2)
    add (Rad x1) (Rad x2) = Rad (x1 + x2)
    add angle1@(Rad _) angle2 = add angle1 (toRad angle2)
    add (Sec x1) (Sec x2) = Sec (x1 + x2)
    add angle1@(Sec _) angle2 = add angle1 (toSec angle2)

    subtr angle1@(DMS _ _ _) angle2 = toDMS $ subtr (toDeg angle1) (toDeg angle2)
    subtr (Deg x1) (Deg x2) = Deg (x1 - x2)
    subtr angle1@(Deg _) angle2 = subtr angle1 (toDeg angle2)
    subtr (Rad x1) (Rad x2) = Rad (x1 - x2)
    subtr angle1@(Rad _) angle2 = subtr angle1 (toRad angle2)
    subtr (Sec x1) (Sec x2) = Sec (x1 - x2)
    subtr angle1@(Sec _) angle2 = subtr angle1 (toSec angle2)
    
    neg (DMS d m s) = DMS (-d) m s
    neg (Deg x) = Deg (-x)
    neg (Rad x) = Rad (-x)
    neg (Sec x) = Sec (-x)

    mul angle@(DMS _ _ _) scalar = toDMS $ toDeg angle `mul` scalar 
    mul (Deg x) scalar = Deg (x * scalar)
    mul (Rad x) scalar = Rad (x * scalar)
    mul (Sec x) scalar = Sec (x * scalar)

    sine angle =
        let
            Rad x = toRad angle
        in
            sin x
    cosine angle =
        let
            Rad x = toRad angle
        in
            cos x
    tangent angle =
        let
            Rad x = toRad angle
        in
            tan x

instance Num Date where
    (+) (JD d1) (JD d2) = JD (d1+d2)
    (+) d1 d2 = toJD d1 + toJD d2
    (-) (JD d1) (JD d2) = JD (d1-d2)
    (-) d1 d2 = toJD d1 - toJD d2
    (*) (JD d1) (JD d2) = JD (d1*d2)
    (*) d1 d2 = toJD d1 * toJD d2
    negate (JD d) = JD (negate d)
    negate d = negate $ toJD d
    abs (JD d) = JD (abs d)
    abs d = abs $ toJD d
    signum (JD d) = JD (signum d)
    signum d = signum $ toJD d
    fromInteger int = JD (fromInteger int)

toHMS :: Hours -> Hours
toHMS x@(HMS _ _ _) = x
toHMS (Hrs x) = HMS h m s where
    h = truncate x
    minAndSec = (x - fromIntegral h) * 60
    m = truncate minAndSec
    s = (minAndSec - fromIntegral m) * 60

toHrs :: Hours -> Hours
toHrs (HMS h m s) = Hrs $ fromIntegral h + (fromIntegral m + s / 60) / 60
toHrs x@(Hrs _) = x

--------------------------------------------------------------------------------

toDMS :: Angle -> Angle
toDMS angle@(DMS _ _ _) = angle
toDMS (Deg x) = DMS d m s where
    d = truncate x
    minAndSec = (x - fromIntegral d) * 60
    m' = truncate minAndSec
    s' = (minAndSec - fromIntegral m') * 60
    (m, s) = if d /= 0 
        then 
            (abs m', abs s') 
        else if m' /= 0 then
            (m', abs s')
        else
            (m', s')
toDMS angle@(Rad _) = toDMS (toDeg angle)
toDMS angle@(Sec _) = toDMS $ toDeg angle

radToDeg :: Double -> Double
radToDeg rad = rad * 180 / pi

-- | Convert degrees to radians
degToRad :: Double -> Double
degToRad deg = deg * pi / 180

toDeg :: Angle -> Angle
toDeg (DMS d m s) =
    if d < 0 || m < 0 || s < 0 then Deg (-deg) else Deg deg where
        deg = fromIntegral (abs d) + (fromIntegral (abs m) + (abs s) / 60) / 60
toDeg angle@(Deg _) = angle
toDeg (Rad x) = Deg $ radToDeg x
toDeg (Sec x) = Deg (x / 3600)

toRad :: Angle -> Angle
toRad angle@(DMS _ _ _) = toRad $ toDeg angle
toRad (Deg x) = Rad $ degToRad x
toRad angle@(Rad _) = angle
toRad angle@(Sec _) = toRad $ toDeg angle

toSec :: Angle -> Angle
toSec (DMS d m s) = Sec (s + (fromIntegral m + (fromIntegral d) * 60) * 60)
toSec (Deg x) = Sec (x * 3600)
toSec angle@(Rad _) = toSec $ toDeg angle
toSec angle@(Sec _) = angle

angleToHours :: Angle -> Hours
angleToHours (Deg x) = 
    Hrs (x / 15)
angleToHours angle@(DMS _ _ _) = 
    toHMS $ angleToHours $ toDeg angle
angleToHours angle@(Rad _) =
    angleToHours $ toDeg angle 
angleToHours angle@(Sec _) =
    angleToHours $ toDeg angle 

hoursToAngle :: Hours -> Angle
hoursToAngle (Hrs x) =
    Deg (x * 15)
hoursToAngle hours@(HMS _ _ _) =
    toDMS $ hoursToAngle $ toHrs hours

absAngle :: Angle -> Angle
absAngle (Deg x) = Deg (abs x) 
absAngle (DMS d m s) = DMS (abs d) (abs m) (abs s) 
absAngle (Rad x) = Rad (abs x)
absAngle (Sec x) = Sec (abs x)

sgnAngle :: Angle -> Double
sgnAngle (Deg x) = signum x 
sgnAngle dms@(DMS d m s) = 
    let
        Deg deg = toDeg dms
    in
        signum deg 
sgnAngle (Rad x) = signum x
sgnAngle (Sec x) = signum x

--------------------------------------------------------------------------------
-- Use this function to construct dates that are not in standard
toValidYMD :: Date -> Date
toValidYMD (YMD y m d)  =
    let
        toValidMonth y m =
            if m < 1 
                then 
                    toValidMonth (y - 1) (m + 12)
                else if m > 12 then
                    toValidMonth (y + 1) (m - 12)
                else
                    (y, m)
                
        toValidMonthAndDay y m d =
            if d < 1 
                then
                  let
                        (y1, m1) = toValidMonth y (m - 1)
                    in
                        toValidMonthAndDay y1 m1 (d + fromIntegral (gregorianMonthLength y1 m1))
                else
                  let
                        monthLength = gregorianMonthLength y m
                    in
                     if d > fromIntegral monthLength 
                            then
                            let
                                    (y1, m1) = toValidMonth y (m + 1)
                                in
                                    toValidMonthAndDay y1 m1 (d - fromIntegral monthLength + 1)
            else
                (y, m, d)
        (y1, m1, d1) = toValidMonthAndDay y m d
    in
        YMD y1 m1 d1    

ymd :: Integer -> Int -> Double -> Date
ymd y m d = toValidYMD (YMD y m d)
  
toYMD :: Date -> Date
toYMD ymd@(YMD _ _ _) = ymd
toYMD years@(TropicalYears _) = toYMD $ toMJD years
toYMD jd@(JD _) = toYMD $ toMJD jd
toYMD rjd@(RJD _) = toYMD $ toMJD rjd
toYMD (MJD x) = 
    let
        (days, hours) = properFraction x 
        (y, m, d) = toGregorian $ ModifiedJulianDay days
    in
        YMD y m (hours + fromIntegral d)
toYMD tjd@(TJD _) = toYMD $ toMJD tjd

toTropicalYears :: Date -> Date
toTropicalYears ymd@(YMD _ _ _ ) = toTropicalYears $ toMJD ymd 
toTropicalYears jd@(JD _) = toTropicalYears $ toMJD jd
toTropicalYears rjd@(RJD _) = toTropicalYears $ toMJD rjd
toTropicalYears (MJD x) = 
    let
        (days, hours) = properFraction x 
        (y, d) = toOrdinalDate $ ModifiedJulianDay days
    in
        TropicalYears (fromIntegral y + (hours + fromIntegral d) / 365.2422)
toTropicalYears tjd@(TJD _) = toTropicalYears $ toMJD tjd

toJD :: Date -> Date
toJD ymd@(YMD _ _ _ ) = 
    toJD $ toMJD ymd 
toJD years@(TropicalYears _) = toJD $ toMJD years
toJD jd@(JD _) = jd 
toJD (RJD x) = JD (x + 2400000)
toJD (MJD x) = JD (x + 2400000.5) 
toJD (TJD x) = JD (x + 2440000.5) 

toRJD :: Date -> Date
toRJD ymd@(YMD _ _ _ ) = 
    toRJD $ toMJD ymd 
toRJD years@(TropicalYears _) = toRJD $ toMJD years
toRJD (JD x) = RJD (x - 2400000)
toRJD rjd@(RJD _) = rjd 
toRJD (MJD x) = RJD (x + 0.5) 
toRJD (TJD x) = RJD (x + 40000.5) 

toMJD :: Date -> Date
toMJD (YMD y m d) =
    MJD $ hours + fromIntegral (toModifiedJulianDay (fromGregorian y m day)) where 
        (day, hours) = properFraction d 
toMJD (TropicalYears x) = 
    let
        (year, days) = properFraction x
        (day, hours) = properFraction (days * 365.2422) 
    in    
        MJD (hours + fromIntegral (toModifiedJulianDay $ fromOrdinalDate year day))
toMJD (JD x) = MJD (x - 2400000.5) 
toMJD (RJD x) = MJD (x - 0.5) 
toMJD mjd@(MJD _) = mjd 
toMJD (TJD x) = MJD (x + 40000) 

toTJD :: Date -> Date
toTJD ymd@(YMD _ _ _ ) = 
    toTJD $ toMJD ymd
toTJD years@(TropicalYears _) = toTJD $ toMJD years
toTJD (JD x) = TJD (x - 2440000.5) 
toTJD (RJD x) = TJD (x - 40000.5) 
toTJD (MJD x) = TJD (x - 40000) 
toTJD tjd@(TJD _) = tjd 

getDayOfMonth :: Date -> Int
getDayOfMonth (YMD y m d) = fst $ properFraction d

diffDays :: 
    Date
    -> Date -- ^ reference date (days to calculate from)
    -> Double
diffDays date1 date2 = 
    let
        MJD mjd1 = toMJD date1
        MJD mjd2 = toMJD date2
    in
        mjd1 - mjd2

-- | julian date of 2000 Jan 1 at 12h
j2000 = JD 2451545.0

toAU :: Distance -> Distance
toAU (AU dist) = AU dist
toAU (Km dist) = AU (dist / 146.6e6)
toAU dist@(EarthRadii _) = toAU (toKm dist)

toKm :: Distance -> Distance
toKm (AU dist) = Km (dist * 146.6e6)
toKm (Km dist) = Km dist
toKm (EarthRadii dist) = Km (dist * 6378.1)

toEarthRadii :: Distance -> Distance
toEarthRadii dist@(AU _) = toEarthRadii (toKm dist)
toEarthRadii (Km dist) = EarthRadii (dist / 6378.1)
toEarthRadii (EarthRadii dist) = EarthRadii dist

toLatitude :: Angle -> Lat
toLatitude angle = 
    if sgnAngle angle < 0 
        then Lat (absAngle angle) S
        else Lat angle N

toLongitude :: Angle -> Long
toLongitude angle =
    if sgnAngle angle < 0
        then Long (absAngle angle) W
        else Long angle E

-- | Add days to a Date (sign-sensitive: negative values subtract days)
addDays :: Double -> Date -> Date
addDays days (YMD y m d) = toYMD $ MJD (mjd + days)
    where MJD mjd = toMJD (YMD y m d)
addDays days (TropicalYears x) = TropicalYears (x + days / 365.2422)
addDays days (JD x) = JD (x + days)
addDays days (RJD x) = RJD (x + days)
addDays days (MJD x) = MJD (x + days)
addDays days (TJD x) = TJD (x + days)

splitDayAndTime :: Date -> (Date, Hours)
splitDayAndTime date =
    let
        JD jd = toJD date
        day = 0.5 + (fromIntegral . truncate) (jd - 0.5)
        time = jd - day
    in
       (JD day, Hrs (time * 24))

getDay :: Date -> Date
getDay date = day where
    (day, _) = splitDayAndTime date

getHours :: Date -> Hours
getHours date = hrs where
    (_, hrs) = splitDayAndTime date

toDays :: Hours -> Double
toDays hours = hrs/24
    where Hrs hrs = toHrs hours

numCenturies :: Date -> Date -> Double
numCenturies (JD jd1) (JD jd2) = (jd2-jd1) / 36525
numCenturies d1 d2 = numCenturies d1 d2

fromDateAndHours :: Date -> Hours -> Date
fromDateAndHours date hours = addDays (toDays hours) date