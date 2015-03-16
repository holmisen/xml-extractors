module Text.XML.Light.Extractors.Extra where

import Safe (readMay)
import Text.XML.Light.Extractors


-- | Reads a floating point value or return @'Left' "float"@ if
-- the read fails.
float :: (Floating a, Read a) => String -> Either String a
float = maybe (Left "float") return . readMay


-- | Reads an integer value or return @'Left' "integer"@ if the read
-- fails.
integer :: (Integral a, Read a) => String -> Either String a
integer = maybe (Left "integer") return . readMay
