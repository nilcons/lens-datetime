--------------------------------------------------------------------------------
-- |
-- Module      : Data.Time.Lens
-- Copyright   : (C) 2013 Mihaly Barasz
-- License     : BSD3, see LICENSE
-- Maintainer  : Mihaly Barasz <mihaly@barasz.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Usage:
--
-- Basic interface consists of the following six lenses: 'year',
-- 'month', 'day', 'hour', 'minute' and 'second' with which you can
-- corresponding \"fields\" of 'LocalTime' and 'UTCTime' in a unified
-- way. Also 'date' and 'time' if you want to access the 'Day' and
-- 'TimeOfDay' parts as a whole.
--
--
-- Let's assume the following definitions:
--
-- >import Data.Time
-- >import Data.Time.Lens
-- >
-- >aDay :: Day
-- >aDay = fromGregorian 2013 08 22
-- >
-- >aLocal :: LocalTime
-- >aLocal = LocalTime aDay (TimeOfDay 13 45 28)
-- >
-- >aUTC :: UTCTime
-- >aUTC = UTCTime aDay 7458.9
--
-- Then you can use the above lenses as follows:
--
-- >>> aLocal ^. year
-- 2013
-- >>> aUTC ^. month
-- 8
-- >>> aLocal & time .~ midnight
-- 2013-08-22 00:00:00
-- >>> aUTC & day .~ 1 & month .~ 1
-- 2013-01-01 02:04:18.9 UTC
-- >>> aLocal & hour +~ 1            -- But see the note below!
-- 2013-08-22 14:45:28
--
--
-- Note about invalid values and lens laws.
--
-- For 'LocalTime' and 'UTCTime' these lenses provide the most
-- straightforward implementation: via 'toGregorian'/'fromGregorian'
-- in the case of 'year', 'month' and 'day'; and directly to the
-- fields of 'TimeOfDay' in the case of 'hour', 'minute' and 'second'.
--
-- Which means, on one hand, that the date \"parts\" will be clipped
-- to valid values:
--
-- >>> aLocal & month +~ 12
-- 2013-12-22 13:45:28        -- instead of: 2014-08-22 13:45:28
-- >>> aUTC & day +~ 100
-- 2013-08-31 02:04:18.9 UTC  -- instead of: 2013-11-30 02:04:18.9 UTC
--
-- And on the other hand, that the time \"parts\" will not roll over
-- and produce invalid values:
--
-- >>> aLocal & minute +~ 120
-- 2013-08-22 13:165:28       -- instead of: 2013-08-22 15:45:28
--
-- Also, this means that the date lenses are not proper lenses: they
-- only satisfy the lens laws when used with valid values for the
-- given fields.
--
-- Basically, avoid setting/modifying the date-time values directly
-- via these lenses if you cannot be sure that the result is a valid
-- value. Instead use the 'FlexibleDateTime' mechanism and the
-- 'flexDT' isomorphism, which correctly rolls over:
--
-- >>> aLocal & flexDT.month +~ 12
-- 2014-08-22 13:45:28
-- >>> aUTC & flexDT.day +~ 100
-- 2013-11-30 02:04:18.9 UTC
-- >>> aLocal & flexDT.minute +~ 120
-- 2013-08-22 15:45:28
--
-- If you need to set multiple fields try to make only one round-trip
-- via flexDT:
--
-- >>> aLocal & flexDT %~ ((day +~ 7) . (hour +~ 2))
-- 2013-08-22 13:45:28
--
-- Note that even with 'flexDT' we completely ignore all the issues
-- around daylight saving time and leap seconds. If your code has to
-- be correct wrt. DST, do all the computations in 'UTCTime' and convert
-- to local time only for output. If you need to be correct wrt. leap
-- seconds, then... Well, then I don't know. :)


{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Time.Lens (
  -- * Lenses for the date parts
    Dateable(..)
  , year, month, day
  -- * Lenses for the time parts
  , Timeable(..)
  , hour, minute, second
  -- * Support for the correct roll-over of fields
  , FlexDate(..)
  , FlexTime(..)
  , FlexibleDateTime(..)
  -- * Miscellaneous
  , utcInTZ
  , utcAsLocal
  , julianDay
  , gregorianDate
  ) where

import Control.Applicative
import Control.Lens
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

--------------------------------------------------------------------------------
-- 'Flexible' data types to properly implement rolling-over behavior.

data FlexDate = FlexDate { flexYear :: Integer
                         , flexMonth :: Int
                         , flexDay :: Int
                         } deriving (Show)

data FlexTime = FlexTime { flexDate :: FlexDate
                         , flexTOD :: TimeOfDay
                         } deriving (Show)

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
  flexDT :: Lens' a FlexTime

instance FlexibleDateTime LocalTime where
  {-# INLINE flexDT #-}
  flexDT = lens convert rollOver
    where
      convert (LocalTime d t) = FlexTime (d ^. gregorianUnflex) t
      {-# INLINE convert #-}

      {-# INLINABLE rollOver #-}
      rollOver _ (FlexTime (FlexDate y m d) tod) = result
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

--------------------------------------------------------------------------------
-- Date parts

-- | Type class that defines access to the \"date\" part of a type.
--
-- You can implement either of the two methods.
class Dateable a where
  date :: Lens' a Day
  date = dateFlex.from gregorianUnflex
  {-# INLINE date #-}

  dateFlex :: Lens' a FlexDate
  dateFlex = date.gregorianUnflex
  {-# INLINE dateFlex #-}

instance Dateable UTCTime where
  date f (UTCTime d t) = flip UTCTime t <$> f d
  {-# INLINE date #-}

instance Dateable LocalTime where
  date f (LocalTime d t) = flip LocalTime t <$> f d
  {-# INLINE date #-}

instance Dateable Day where
  date = id
  {-# INLINE date #-}

instance Dateable FlexDate where
  dateFlex = id
  {-# INLINE dateFlex #-}

instance Dateable FlexTime where
  dateFlex f (FlexTime d t) = flip FlexTime t <$> f d
  {-# INLINE dateFlex #-}

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

-- | Lens into the year value of a 'Dateable'.
--
-- Warning: this is not a proper lens for 'LocalTime' and 'UTCTime':
-- it only obeys the lens laws if used with valid values. When the
-- year value in a date is modified the month and day values might
-- also change. This happens when the original date was a February
-- 29th and we change to a non-leap year.
year :: Dateable d => Lens' d Integer
year = dateFlex._1
{-# INLINE year #-}

-- | Lens into the month value of a 'Dateable'.
--
-- Warning: this is not a proper lens for 'LocalTime' and 'UTCTime':
-- it only obeys the lens laws if used with valid values. The updated
-- month value will be clipped to a valid month value. Also note that
-- the day value might also be modified (clipped to a valid day in
-- that month).
month :: Dateable d => Lens' d Int
month = dateFlex._2
{-# INLINE month #-}

-- | Lens into the day value of a 'Dateable'.
--
-- Warning: this is not a proper lens for 'LocalTime' and 'UTCTime':
-- it only obeys the lens laws if used with valid values. The updated
-- day value will be clipped to a valid day value in the given
-- year-month.
day :: Dateable d => Lens' d Int
day = dateFlex._3
{-# INLINE day #-}

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

instance Timeable TimeOfDay where
  time = id
  {-# INLINE time #-}

instance Timeable FlexTime where
  time f (FlexTime d t) = FlexTime d <$> f t
  {-# INLINE time #-}

-- | Lens into the hour value of a 'Timeable'.
--
-- Warning: this is not a proper lens for 'UTCTime': it only obeys the
-- lens laws if used with valid values.
hour :: Timeable t => Lens' t Int
hour = time.hour'
  where
    hour' f (TimeOfDay h m s) = (\h' -> TimeOfDay h' m s) <$> f h
{-# INLINE hour #-}

-- | Lens into the minute value of a 'Timeable'.
--
-- Warning: this is not a proper lens for 'UTCTime': it only obeys the
-- lens laws if used with valid values.
minute :: Timeable t => Lens' t Int
minute = time.minute'
  where
    minute' f (TimeOfDay h m s) = (\m' -> TimeOfDay h m' s) <$> f m
{-# INLINE minute #-}

-- | Lens into the second value of a 'Timeable'.
--
-- Warning: this is not a proper lens for 'UTCTime': it only obeys the
-- lens laws if used with valid values.
second :: Timeable t => Lens' t Pico
second = time.second'
  where
    second' f (TimeOfDay h m s) = TimeOfDay h m <$> f s
{-# INLINE second #-}

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
