{-
---
fulltitle: "In class exercise: Practice with Maybe Monad"
date:
---

The goal of this short in-class exercise is to get a bit more practice with using the `Maybe` Monad.
-}

module MaybePractice where

import qualified Control.Monad as Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Read as Text

{-
Part 1
------

Consider the `Weather` datatype from the Kata problem from HW #3.
-}

data Weather = Weather
  { dayNumber :: Int,
    maxTemp :: Int,
    minTemp :: Int
  }
  deriving (Eq, Show)

{-
Use `Map.lookup` and `Text.readMaybe` to write a function that takes a
dictionary (i.e. a finite map from String keys to String values) and converts
it into weather data. Your implementation should not use pattern matching;
use `do` notation instead.

If the provided dictionary does not have the appropriate data, then the
function should return `Nothing`. In other words, this function should behave
according to the examples below.
-}

-- >>> parseWeather (Map.fromList [("day", "2"), ("maxTemp", "78"), ("minTemp", "62")])
-- Just (Weather {dayNumber = 2, maxTemp = 78, minTemp = 62})
-- >>> parseWeather (Map.fromList [("day", "2")])
-- Nothing
-- >>> parseWeather (Map.fromList [("day", "two"), ("maxTemp", "78"), ("minTemp", "62")])
-- Nothing

parseWeather :: Map String String -> Maybe Weather
parseWeather m = do
  dayStr <- Map.lookup "day" m
  maxTempStr <- Map.lookup "maxTemp" m
  minTempStr <- Map.lookup "minTemp" m
  day <- Text.readMaybe dayStr
  maxTemp <- Text.readMaybe maxTempStr
  minTemp <- Text.readMaybe minTempStr
  return Weather {dayNumber= day, maxTemp=maxTemp, minTemp= minTemp}

{-
Part 2
------

Consider the following operations that combine two `Maybe`s. Implement them both.
One can be defined using `(>>=)` from the `Maybe` monad, and one cannot. Which is which?

-}

-- | Left-biased choice on maybes
--
-- >>> firstJust (Just 1) (Just 2)
-- Just 1
-- >>> firstJust Nothing (Just 2)
-- Just 2
-- >>> firstJust (Just 1) Nothing
-- Just 1
-- >>> firstJust Nothing Nothing
-- Nothing
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust x y = case x of
  Just a -> Just a
  Nothing -> y

-- | Ensure that both Maybes are 'Just' and retain the first one
--
-- >>> sequenceFirst (Just 1) (Just 'a')
-- Just 1
-- >>> sequenceFirst Nothing (Just 'a')
-- Nothing
-- >>> sequenceFirst (Just 1) Nothing
-- Nothing 
-- >>> sequenceFirst Nothing Nothing
-- Nothing
{-
sequenceFirst :: Maybe a -> Maybe b -> Maybe a
sequenceFirst x y = 
  case x of 
    Nothing -> Nothing
    Just a -> case y of
      Just b -> Just a
      Nothing -> Nothing
-}
-- 因为我们想要两个 argument 都不是 nothing
-- 所以 do syntax 或者说 binding 就比较合适
sequenceFirst :: Maybe a -> Maybe b -> Maybe a
sequenceFirst x y = x >>= (\a -> y >>= (\b -> Just a))
