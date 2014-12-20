module Text.XML.Light.Extractors.Extra where

import Safe (readMay)
import Text.XML.Light.Extractors


float :: (Floating a, Read a) => String -> Either Err a
float s = 
  maybe (Left $ ErrMsg ("Expected float, found: " ++ show s)) return (readMay s)


integer :: (Integral a, Read a) => String -> Either Err a
integer s = 
  maybe (Left $ ErrMsg ("Expected integer, found: " ++ show s)) return (readMay s)
