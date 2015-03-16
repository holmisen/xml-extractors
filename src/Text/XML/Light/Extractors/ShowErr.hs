-- | This module provides functions to translate errors into strings.
module Text.XML.Light.Extractors.ShowErr
  ( showExtractionErr
  , showErr
  , showPath
  )
where

import Data.List

import           Text.XML.Light.Types
import qualified Text.XML.Light.Output as XML
import           Text.XML.Light.Extractors.Internal (ExtractionErr(..), Err(..), Path)


-- | Converts an extraction error to a multi line string message.
--
-- Paths will convert like this:
--
-- @1\/foo\/2\/bar\/\@x@
--
-- which represents the \"x\" attribute of the \"bar\" element, which
-- is the second content of the \"foo\" element which is the first
-- content from the root.
showExtractionErr :: ExtractionErr -> String
showExtractionErr (ExtractionErr e path) =
  unlines [showErr e ++ "in path: " ++ showPath path]

showPath :: Path -> String
showPath = intercalate "/" . reverse


showErr :: Err -> String
showErr (ErrExpectContent expect found) =
  unlines
  [ unwords ["Expected:", expect]
  , unwords ["Found:", take 60 $ XML.showContent found]
  , unwords ["at line:", showLine found]
  ]
showErr (ErrExpectAttrib expect parent) =
  unlines
  [ unwords ["Missing attribute", show expect, "of", show $ qName $ elName parent]
  , unwords ["at line:", showLine $ Elem parent]
  ]
showErr (ErrAttribValue expect found parent) =
  unlines
  [ unwords ["Expected:", expect]
  , unwords ["Found:", show found]
  , unwords ["line:", showLine $ Elem parent]
  ]
showErr (ErrMsg msg) =
  unlines [msg]

showErr (ErrNull expected) = 
  unlines [unwords ["Expected:", expected]]

showErr (ErrEnd found) =
  unlines [unwords ["Unexpected:", take 60 $ XML.showContent found]]


contentLine :: Content -> Maybe Integer
contentLine (Elem e) = elLine e
contentLine (Text t) = cdLine t
contentLine _        = Nothing

showLine :: Content -> String
showLine = maybe "??" show . contentLine
