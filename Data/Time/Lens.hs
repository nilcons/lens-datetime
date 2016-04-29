{- |
Module      : Data.Time.Lens
Copyright   : (C) 2013 Mihaly Barasz
License     : BSD3, see LICENSE
Maintainer  : Mihaly Barasz <mihaly@barasz.com>
Stability   : experimental
Portability : non-portable

/Usage:/

Basic interface consists of the following six lenses: 'years',
'months', 'days', 'hours', 'minutes' and 'seconds' with which you can
access the corresponding \"fields\" of 'LocalTime' and 'UTCTime' in
a unified way. Also, use 'date' and 'time' if you want to access the
'Day' and 'TimeOfDay' parts as a whole.


Let's assume the following definitions:

>import Control.Lens
>import Data.Time
>import Data.Time.Lens
>
>aDay :: Day
>aDay = fromGregorian 2013 08 22
>
>aLocal :: LocalTime
>aLocal = LocalTime aDay (TimeOfDay 13 45 28)
>
>aUTC :: UTCTime
>aUTC = UTCTime aDay 7458.9

Then you can use the above lenses as follows:

>>> aLocal ^. years
2013
>>> aUTC ^. months
8
>>> aDay ^. days
22
>>> aLocal & time .~ midnight
2013-08-22 00:00:00
>>> aUTC & days .~ 1 & months .~ 1
2013-01-01 02:04:18.9 UTC
>>> aLocal & hours +~ 1            -- But see the note below!
2013-08-22 14:45:28


/A note about invalid values and lens laws./

For 'LocalTime' and 'UTCTime' these lenses provide the most
straightforward implementation: via 'toGregorian'/'fromGregorian'
in the case of 'years', 'months' and 'days'; and directly to the
fields of 'TimeOfDay' in the case of 'hours', 'minutes' and 'seconds'.

Which means, on one hand, that the date \"parts\" will be clipped
to valid values:

>>> aLocal & months +~ 12
2013-12-22 13:45:28        -- instead of: 2014-08-22 13:45:28
>>> aUTC & days +~ 100
2013-08-31 02:04:18.9 UTC  -- instead of: 2013-11-30 02:04:18.9 UTC

And on the other hand, that the time \"parts\" will not roll over
and produce invalid values:

>>> aLocal & minutes +~ 120
2013-08-22 13:165:28       -- instead of: 2013-08-22 15:45:28

Also, this means that the date lenses are not proper lenses: they
only satisfy the lens laws when used with valid values for the
given fields.

Basically, avoid setting/modifying the date-time values directly
via these lenses if you cannot be sure that the result is a valid
value. Instead use the 'FlexibleDateTime' mechanism and the
'flexDT' isomorphism, which correctly rolls over:

>>> aLocal & flexDT.months +~ 12
2014-08-22 13:45:28
>>> aUTC & flexDT.days +~ 100
2013-11-30 02:04:18.9 UTC
>>> aLocal & flexDT.minutes +~ 120
2013-08-22 15:45:28

If you need to set multiple fields try to make only one round-trip
via flexDT:

>>> aLocal & flexDT %~ ((days +~ 7) . (hours +~ 2))
2013-08-22 13:45:28

Note that even with 'flexDT' we completely ignore all the issues
around daylight saving time and leap seconds. If your code has to
be correct wrt. DST, do all the computations in 'UTCTime' and convert
to local time only for output. If you need to be correct wrt. leap
seconds, then... Well, then I don't know. :)

And while this doesn't strictly belong to this package, here's a
complete example of working with daylight saving time:

>dstExample :: IO ()
>dstExample = do
>  let baseT = UTCTime (fromGregorian 2013 10 26) 0
>
>      printInLocal :: UTCTime -> IO ()
>      printInLocal t = do
>        tz <- getTimeZone t
>        print (tz, t ^. utcInTZ tz)
>
>  printInLocal baseT
>  printInLocal $ baseT & flexDT %~ ((days +~ 1) . (hours +~ 0) . (minutes +~ 5))
>  printInLocal $ baseT & flexDT %~ ((days +~ 1) . (hours +~ 1) . (minutes +~ 5))
>  printInLocal $ baseT & flexDT %~ ((days +~ 1) . (hours +~ 2) . (minutes +~ 5))

>>> dstExample
(CEST,2013-10-26 02:00:00)
(CEST,2013-10-27 02:05:00)
(CET,2013-10-27 02:05:00)
(CET,2013-10-27 03:05:00)

-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Time.Lens (
  -- * Lenses for the date parts
    Dateable(..)
  , years, months, days
  -- * Lenses for the time parts
  , Timeable(..)
  , hours, minutes, seconds
  -- * Support for the correct roll-over of fields
  , FlexDateTime(..)
  , FlexDate(..)
  , FlexTime(..)
  , FlexibleDateTime(..)
  , FlexibleDate(..)
  , FlexibleTime(..)
  -- * Miscellaneous
  , utcInTZ
  , utcAsLocal
  , zonedAsLocal
  , julianDay
  , julianDT
  , gregorianDate
  ) where

import Control.Applicative
import Control.Lens
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

-- To avoid the 'redundant import' warning for Applicative:
import Prelude

--------------------------------------------------------------------------------
-- 'Flexible' data types to properly implement rolling-over behavior.

data FlexDate = FlexDate { flexYear :: Integer
                         , flexMonth :: Int
                         , flexDay :: Int
                         } deriving (Show)

data FlexDateTime = FlexDateTime { flexDate :: FlexDate
                                 , flexTOD :: TimeOfDay
                                 } deriving (Show)

newtype FlexTime = FlexTime TimeOfDay deriving (Show)

flexDateTriple :: Iso' FlexDate (Integer,Int,Int)
flexDateTriple = iso (\(FlexDate y m d) -> (y,m,d)) (\(y,m,d) -> FlexDate y m d)
{-# INLINE flexDateTriple #-}

gregorianUnflex :: Iso' Day FlexDate
gregorianUnflex = gregorianDate.from flexDateTriple
{-# INLINE gregorianUnflex #-}

instance Field1 FlexDate FlexDate Integer Integer where
  _1 f (FlexDate y m d) = indexed f (0 :: Int) y <&> \y' -> FlexDate y' m d
  {-# INLINE _1 #-}

instance Field2 FlexDate FlexDate Int Int where
  _2 f (FlexDate y m d) = indexed f (1 :: Int) m <&> \m' -> FlexDate y m' d
  {-# INLINE _2 #-}

instance Field3 FlexDate FlexDate Int Int where
  _3 f (FlexDate y m d) = indexed f (2 :: Int) d <&> \d' -> FlexDate y m d'
  {-# INLINE _3 #-}

-- | Type class to provide correct roll-over behavior for date-time lenses.
--
-- See examples in the general overview part.
class FlexibleDateTime a where
  flexDT :: Lens' a FlexDateTime

instance FlexibleDateTime LocalTime where
  {-# INLINE flexDT #-}
  flexDT = lens convert rollOver
    where
      convert (LocalTime d t) = FlexDateTime (d ^. gregorianUnflex) t
      {-# INLINE convert #-}

      {-# INLINABLE rollOver #-}
      rollOver _ (FlexDateTime (FlexDate y m d) tod) = result
        where
          (secs0, p0) = properFraction $ tod ^. from diffTOD
          (secs, p) = if p0 < 0 then (secs0 - 1, p0 + 1) else (secs0, p0)
          (dt, secs1) = secs `divMod` (24*60*60)
          tod' = (fromIntegral secs1 + p) ^. diffTOD

          date0 = if m >= 1 && m <= 12
                  then fromGregorian y m 1
                  else fromGregorian y 1 1 & addGregorianMonthsRollOver (fromIntegral $ m-1)
          date' = addDays (fromIntegral $ d + dt - 1) date0
          result = LocalTime date' tod'

instance FlexibleDateTime UTCTime where
  flexDT = utcAsLocal.flexDT
  {-# INLINE flexDT #-}

instance FlexibleDateTime ZonedTime where
  flexDT = zonedAsLocal . flexDT
  {-# INLINE flexDT #-}

-- | Type class to provide correct roll-over behavior for date lenses.
--
-- Used exactly as 'flexDT', but for values that have only \"date\"
-- and no \"time\" part.
class FlexibleDate a where
  flexD :: Lens' a FlexDate

instance FlexibleDate Day where
  {-# INLINE flexD #-}
  flexD = lens (view gregorianUnflex) rollOver
    where
      {-# INLINABLE rollOver #-}
      rollOver _ (FlexDate y m d) = result
        where
          date0 = if m >= 1 && m <= 12
                  then fromGregorian y m 1
                  else fromGregorian y 1 1 & addGregorianMonthsRollOver (fromIntegral $ m-1)
          result = addDays (fromIntegral $ d  - 1) date0

-- | Type class to provide correct roll-over behavior for time lenses.
--
-- Used exactly as 'flexDT', but for values that have only \"time\"
-- and no \"date\" part.
--
-- If the time rolls-over more than 24 hours the day carry is
-- discarded. Ex.:
--
-- >>> let t = TimeOfDay 1 12 3
-- >>> t
-- 01:12:03
-- >>> t & flexT.seconds +~ (-7200)
-- 23:12:03
--
class FlexibleTime a where
  flexT :: Lens' a FlexTime

instance FlexibleTime TimeOfDay where
  {-# INLINABLE flexT #-}
  flexT = lens FlexTime rollOver
    where
      {-# INLINABLE rollOver #-}
      rollOver _ (FlexTime tod) = tod'
        where
          secs0 :: Int
          (secs0, p0) = properFraction $ tod ^. from diffTOD
          (secs, p) = if p0 < 0 then (secs0 - 1, p0 + 1) else (secs0, p0)
          secs1 = secs `mod` (24*60*60)
          tod' = (fromIntegral secs1 + p) ^. diffTOD

--------------------------------------------------------------------------------
-- Date parts

-- | Type class that defines access to the \"date\" part of a type.
--
-- You can implement either of the two methods.
class Dateable a where
  date :: Lens' a Day
  date = _dateFlex.from gregorianUnflex
  {-# INLINE date #-}

  _dateFlex :: Lens' a FlexDate
  _dateFlex = date.gregorianUnflex
  {-# INLINE _dateFlex #-}

instance Dateable UTCTime where
  date f (UTCTime d t) = flip UTCTime t <$> f d
  {-# INLINE date #-}

instance Dateable LocalTime where
  date f (LocalTime d t) = flip LocalTime t <$> f d
  {-# INLINE date #-}

instance Dateable ZonedTime where
  date = zonedAsLocal . date
  {-# INLINE date #-}

instance Dateable Day where
  date = id
  {-# INLINE date #-}

instance Dateable FlexDate where
  _dateFlex = id
  {-# INLINE _dateFlex #-}

instance Dateable FlexDateTime where
  _dateFlex f (FlexDateTime d t) = flip FlexDateTime t <$> f d
  {-# INLINE _dateFlex #-}

-- | View 'Day' as an 'Integer' day number in the Julian calendar.
--
-- See the description at the definition of 'Day'.
julianDay :: Iso' Day Integer
julianDay = iso toModifiedJulianDay ModifiedJulianDay
{-# INLINE julianDay #-}

-- | View 'Day' as a triple of (year,month,day) in Gregorian calendar.
--
-- See the description at the definition of 'fromGregorian' / 'toGregorian'.
gregorianDate :: Iso' Day (Integer,Int,Int)
gregorianDate = iso toGregorian (\(y,m,d) -> fromGregorian y m d)
{-# INLINE gregorianDate #-}

-- | View 'LocalTime' as a fractional day in the modified Julian calendar.
--
-- See the description of 'ModifiedJulianDay' and 'timeOfDayToDayFraction'.
julianDT :: Iso' LocalTime Rational
julianDT = iso there back
  where
    {-# INLINE there #-}
    there (LocalTime d tod) =
      fromIntegral (toModifiedJulianDay d) + timeOfDayToDayFraction tod
    {-# INLINE back #-}
    back r = let (d,t) = properFraction r in
      LocalTime (ModifiedJulianDay d) (dayFractionToTimeOfDay t)
{-# INLINE julianDT #-}

-- | Lens into the year value of a 'Dateable'.
--
-- Warning: this is not a proper lens for 'LocalTime' and 'UTCTime':
-- it only obeys the lens laws if used with valid values. When the
-- year value in a date is modified the month and day values might
-- also change. This happens when the original date was a February
-- 29th and we change to a non-leap year.
years :: Dateable d => Lens' d Integer
years = _dateFlex._1
{-# INLINE years #-}

-- | Lens into the month value of a 'Dateable'.
--
-- Warning: this is not a proper lens for 'LocalTime' and 'UTCTime':
-- it only obeys the lens laws if used with valid values. The updated
-- month value will be clipped to a valid month value. Also note that
-- the day value might also be modified (clipped to a valid day in
-- that month).
months :: Dateable d => Lens' d Int
months = _dateFlex._2
{-# INLINE months #-}

-- | Lens into the day value of a 'Dateable'.
--
-- Warning: this is not a proper lens for 'LocalTime' and 'UTCTime':
-- it only obeys the lens laws if used with valid values. The updated
-- day value will be clipped to a valid day value in the given
-- year-month.
days :: Dateable d => Lens' d Int
days = _dateFlex._3
{-# INLINE days #-}

--------------------------------------------------------------------------------
-- Time of day parts

diffTOD :: Iso' DiffTime TimeOfDay
diffTOD = iso timeToTimeOfDay timeOfDayToTime
{-# INLINE diffTOD #-}

-- | Type class that defines access to the \"time\" part of a type.
--
-- You only need to define one of the two methods, whichever is more
-- natural.
class Timeable a where
  time :: Lens' a TimeOfDay
  time = timeAsDiff . diffTOD
  {-# INLINE time #-}

  timeAsDiff :: Lens' a DiffTime
  timeAsDiff = time . from diffTOD
  {-# INLINE timeAsDiff #-}

instance Timeable UTCTime where
  timeAsDiff f (UTCTime d t) = UTCTime d <$> f t
  {-# INLINE timeAsDiff #-}

instance Timeable LocalTime where
  time f (LocalTime d t) = LocalTime d <$> f t
  {-# INLINE time #-}

instance Timeable ZonedTime where
  time = zonedAsLocal . time
  {-# INLINE time #-}

instance Timeable TimeOfDay where
  time = id
  {-# INLINE time #-}

instance Timeable FlexDateTime where
  time f (FlexDateTime d t) = FlexDateTime d <$> f t
  {-# INLINE time #-}

instance Timeable FlexTime where
  time f (FlexTime t) = FlexTime <$> f t

-- | Lens into the hour value of a 'Timeable'.
--
-- Warning: this is not a proper lens for 'UTCTime': it only obeys the
-- lens laws if used with valid values.
hours :: Timeable t => Lens' t Int
hours = time.hours'
  where
    hours' f (TimeOfDay h m s) = (\h' -> TimeOfDay h' m s) <$> f h
{-# INLINE hours #-}

-- | Lens into the minute value of a 'Timeable'.
--
-- Warning: this is not a proper lens for 'UTCTime': it only obeys the
-- lens laws if used with valid values.
minutes :: Timeable t => Lens' t Int
minutes = time.minutes'
  where
    minutes' f (TimeOfDay h m s) = (\m' -> TimeOfDay h m' s) <$> f m
{-# INLINE minutes #-}

-- | Lens into the second value of a 'Timeable'.
--
-- Warning: this is not a proper lens for 'UTCTime': it only obeys the
-- lens laws if used with valid values.
seconds :: Timeable t => Lens' t Pico
seconds = time.seconds'
  where
    seconds' f (TimeOfDay h m s) = TimeOfDay h m <$> f s
{-# INLINE seconds #-}

--------------------------------------------------------------------------------
-- Misc

-- | Trivial isomorphism between 'UTCTime' and 'LocalTime'.
--
-- We view 'LocalTime' values as being in the UTC time zone. This is
-- 'utcInTZ' applied to 'utc'.
utcAsLocal :: Iso' UTCTime LocalTime
utcAsLocal = utcInTZ utc
{-# INLINE utcAsLocal #-}

-- | Isomorphism between 'UTCTime' and 'LocalTime' for the given
-- 'TimeZone'.
utcInTZ :: TimeZone -> Iso' UTCTime LocalTime
utcInTZ tz = iso (utcToLocalTime tz) (localTimeToUTC tz)
{-# INLINE utcInTZ #-}

-- | Lens into the 'LocalTime' part of 'ZonedTime'.
zonedAsLocal :: Lens' ZonedTime LocalTime
zonedAsLocal f (ZonedTime lt tz) = flip ZonedTime tz <$> f lt
{-# INLINE zonedAsLocal #-}
