module Text.XML.Light.Extractors.ShowErr where

import Data.List

import           Text.XML.Light.Types
import qualified Text.XML.Light.Output as XML
import           Text.XML.Light.Extractors.Internal (ExtractionErr(..), Err(..), Path)


showExtractionErr :: ExtractionErr -> String
showExtractionErr (ExtractionErr e path) = showErr e ++ "\nin " ++ showPath path

showPath :: Path -> String
showPath = intercalate "/" . reverse


showErr (ErrExpect expect found) =
  unwords ["Expected", expect, "found", take 60 $ XML.showContent found]

showErr (ErrAttr expect parent) =
  unwords ["Missing attribute", show expect, "of", qName $ elName parent]

showErr (ErrMsg msg) = msg

showErr (ErrNull expected) = 
  unwords ["Expected", expected]

showErr (ErrEnd found) =
  unwords ["Unexpected:", take 60 $ XML.showContent found]
