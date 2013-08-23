{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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


class FlexibleTime a where
  flexible :: Iso' a FlexTime

instance FlexibleTime LocalTime where
  {-# INLINE flexible #-}
  flexible = iso convert rollOver
    where
      convert (LocalTime d t) = FlexTime (d ^. gregorianUnflex) t
      {-# INLINE convert #-}

      {-# INLINABLE rollOver #-}
      rollOver (FlexTime (FlexDate y m d) tod) = result
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



instance FlexibleTime UTCTime where
  flexible = utcAsLocal.flexible
  {-# INLINE flexible #-}

--------------------------------------------------------------------------------
-- Date parts

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
year = dateFlex._1
{-# INLINE year #-}

-- |
-- Warning: this is not a proper lens: the updated month value will be
-- clipped to a valid month value. Also note that the day value might
-- also be modified (clipped to a valid day in that month).
month :: Dateable d => Lens' d Int
month = dateFlex._2
{-# INLINE month #-}

-- |
-- Warning: this is not a proper lens: the updated day value will be
-- clipped to a valid day value in the given year-month.
day :: Dateable d => Lens' d Int
day = dateFlex._3
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

instance Timeable FlexTime where
  time f (FlexTime d t) = FlexTime d <$> f t
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
