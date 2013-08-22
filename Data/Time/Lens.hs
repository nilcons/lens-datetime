{-# LANGUAGE Rank2Types #-}

module Data.Time.Lens
       ( Dateable(..)
       , julianDay
       , gregorianDate
       , year, month, day
       , Timeable(..)
       , hour, minute, second
       , utcInTZ
       , utcAsLocal
       ) where

import Control.Applicative
import Control.Lens
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

--------------------------------------------------------------------------------
-- Date parts

class Dateable a where
  date :: Lens' a Day

instance Dateable UTCTime where
  date f (UTCTime d t) = flip UTCTime t <$> f d
  {-# INLINE date #-}

instance Dateable LocalTime where
  date f (LocalTime d t) = flip LocalTime t <$> f d
  {-# INLINE date #-}

instance Dateable Day where
  date = id
  {-# INLINE date #-}

julianDay :: Iso' Day Integer
julianDay = iso toModifiedJulianDay ModifiedJulianDay
{-# INLINE julianDay #-}

gregorianDate :: Iso' Day (Integer,Int,Int)
gregorianDate = iso toGregorian (\(y,m,d) -> fromGregorian y m d)
{-# INLINE gregorianDate #-}

-- |
-- Note: when the year value in a date is modified the month and day
-- values might also change. This happens when the original date was a
-- February 29th and we change to a non-leap year.
year :: Dateable d => Lens' d Integer
year = date.gregorianDate._1
{-# INLINE year #-}

-- |
-- Warning: this is not a proper lens: the updated month value will be
-- clipped to a valid month value. Also note that the day value might
-- also be modified (clipped to a valid day in that month).
month :: Dateable d => Lens' d Int
month = date.gregorianDate._2
{-# INLINE month #-}

-- |
-- Warning: this is not a proper lens: the updated day value will be
-- clipped to a valid day value in the given year-month.
day :: Dateable d => Lens' d Int
day = date.gregorianDate._3
{-# INLINE day #-}

--------------------------------------------------------------------------------
-- Time of day parts

diffTOD :: Iso' DiffTime TimeOfDay
diffTOD = iso timeToTimeOfDay timeOfDayToTime
{-# INLINE diffTOD #-}

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


hour :: Timeable t => Lens' t Int
hour = time.hour'
  where
    hour' f (TimeOfDay h m s) = (\h' -> TimeOfDay h' m s) <$> f h
{-# INLINE hour #-}

minute :: Timeable t => Lens' t Int
minute = time.minute'
  where
    minute' f (TimeOfDay h m s) = (\m' -> TimeOfDay h m' s) <$> f m
{-# INLINE minute #-}

second :: Timeable t => Lens' t Pico
second = time.second'
  where
    second' f (TimeOfDay h m s) = TimeOfDay h m <$> f s
{-# INLINE second #-}

--------------------------------------------------------------------------------
-- Misc

utcAsLocal :: Iso' UTCTime LocalTime
utcAsLocal = utcInTZ utc
{-# INLINE utcAsLocal #-}

utcInTZ :: TimeZone -> Iso' UTCTime LocalTime
utcInTZ tz = iso (utcToLocalTime tz) (localTimeToUTC tz)
{-# INLINE utcInTZ #-}
