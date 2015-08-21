{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Functions to extract data from parsed XML.
--
-- = Example
--
-- Suppose you have an xml file of books like this:
--
-- > <?xml version="1.0"?>
-- > <library>
-- >   <book id="1" isbn="23234-1">
-- >     <author>John Doe</author>
-- >     <title>Some book</title>
-- >   </book>
-- >   <book id="2">
-- >     <author>You</author>
-- >     <title>The Great Event</title>
-- >   </book>
-- >   ...
-- > </library>
--
-- And a data type for a book:
--
-- > data Book = Book { bookId        :: Int
-- >                  , isbn          :: Maybe String
-- >                  , author, title :: String
-- >                  }
--
-- You can parse the xml file into a generic tree structure using
-- 'Text.XML.Light.Input.parseXMLDoc' from the `xml` package.
--
-- Using this library one can define extractors to extract Books from
-- the generic tree.
-- 
-- @
--    book = 'element' "book" $ do
--             i <- 'attribAs' "id" 'Text.XML.Light.Extractors.Extra.integer'
--             s <- 'optional' ('attrib' "isbn")
--             'children' $ do
--               a <- 'element' "author" $ 'contents' $ 'text'
--               t <- 'element' "title" $ 'contents' $ 'text'
--               return Book { bookId = i, author = a, title = t, isbn = s }
--
--    library = 'element' "library" $ 'children' $ 'only' $ 'many' book
--
--    extractLibrary :: 'XML.Element' -> 'Either' 'ExtractionErr' [Book]
--    extractLibrary = 'extractDocContents' library
-- @
--
-- = Notes
--
--  * The 'only' combinator can be used to exhaustively extract contents.
--
--  * The "Control.Applicative" module contains some useful
--    combinators like 'optional', 'many' and '<|>'.
--
--  * The "Text.XML.Light.Extractors.ShowErr" contains some
--    predefined functions to convert error values to strings.
--
module Text.XML.Light.Extractors
  ( 
  -- * Errors
    Path
  , Err(..)
  , ExtractionErr(..)
    
  -- * Element extraction
  , ElementExtractor
  , extractElement
  , attrib
  , attribAs
  , children
  , contents
  
  -- * Contents extraction
  , ContentsExtractor
  , extractContents
  , extractDocContents
  , element
  , text
  , textAs
  , choice
  , anyContent
  , eoc
  , only

  -- * Utils
  , showExtractionErr
  , eitherMessageOrValue
  , integer
  , float
  ) 
where

import Control.Applicative

import           Text.XML.Light.Types as XML

import           Text.XML.Light.Extractors.Extra
import           Text.XML.Light.Extractors.ShowErr  (showExtractionErr)
import           Text.XML.Light.Extractors.Internal (ExtractionErr, Err, Path)
import qualified Text.XML.Light.Extractors.Internal as Internal
import           Text.XML.Light.Extractors.Internal.Result hiding (throwError, throwFatal)

--------------------------------------------------------------------------------

newtype ElementExtractor a = ElementExtractor (Internal.ElementExtractor a)
 deriving (Applicative, Alternative, Functor, Monad)


newtype ContentsExtractor a = ContentsExtractor (Internal.ContentsExtractor a)
 deriving (Applicative, Alternative, Functor, Monad)

--------------------------------------------------------------------------------
 
-- | @extractElement p element@ extracts @element@ with @p@.
extractElement :: ElementExtractor a -> XML.Element -> Either ExtractionErr a
extractElement (ElementExtractor p) elem = toEither $ Internal.runElementExtractor p elem []


-- | @attrib name@ extracts the value of attribute @name@.
attrib :: String -> ElementExtractor String
attrib = ElementExtractor . Internal.attrib


-- | @attribAs name f@ extracts the value of attribute @name@ and runs
-- it through a conversion/validation function.
--
-- The conversion function takes a string with the value and returns
-- either a description of the expected format of the value or the
-- converted value.
attribAs :: String -> (String -> Either String a) -> ElementExtractor a
attribAs name = ElementExtractor . (Internal.attribAs name)


-- | @children p@ extract only child elements with @p@.
children :: ContentsExtractor a -> ElementExtractor a
children (ContentsExtractor p) = ElementExtractor (Internal.children p)


-- | @contents p@ extract contents with @p@.
contents :: ContentsExtractor a -> ElementExtractor a
contents (ContentsExtractor p) = ElementExtractor (Internal.contents p)

--------------------------------------------------------------------------------

-- | @extractContents p contents@ extracts the contents with @p@.
extractContents :: ContentsExtractor a -> [XML.Content] -> Either ExtractionErr a
extractContents (ContentsExtractor p) cs =
  toEither (fst <$> Internal.runContentsExtractor p cs 1 [])


-- | Using 'Text.XML.Light.Input.parseXMLDoc' produces a single
-- 'Element'. Such an element can be extracted using this function.
extractDocContents :: ContentsExtractor a -> XML.Element -> Either ExtractionErr a
extractDocContents p = extractContents p . return . Elem


-- | @only p@ fails if there is more contents than extracted by @p@.
--
-- > only p = p <* eoc
only :: ContentsExtractor a -> ContentsExtractor a
only p = p <* eoc


-- | Succeeds only when there is no more content.
eoc :: ContentsExtractor ()
eoc = ContentsExtractor Internal.eoc


-- | @element name p@ extracts a @name@ element with @p@.
element :: String -> ElementExtractor a -> ContentsExtractor a
element name (ElementExtractor a) = ContentsExtractor $ Internal.element name a


-- | Extracts text.
text :: ContentsExtractor String
text = ContentsExtractor Internal.text


-- | Extracts text applied to a conversion function.
textAs :: (String -> Either Err a) -> ContentsExtractor a
textAs = ContentsExtractor . Internal.textAs


-- | Extracts first matching.
choice :: [ContentsExtractor a] -> ContentsExtractor a
choice = foldr (<|>) empty


-- | Extracts one 'Content' item.
anyContent :: ContentsExtractor Content
anyContent = ContentsExtractor Internal.anyContent


-- | Convenience function to convert extraction errors to string
-- messages using 'showExtractionErr'.
--
-- > eitherMessageOrValue = either (Left . showExtractionErr) Right
eitherMessageOrValue :: Either ExtractionErr a -> Either String a
eitherMessageOrValue = either (Left . showExtractionErr) Right
