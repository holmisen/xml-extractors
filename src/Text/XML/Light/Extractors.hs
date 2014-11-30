{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

-- | A library for making extraction of information from parsed XML easier.
--
-- The 'Control.Applicative' module contains some useful combinators
-- like 'optional', 'many' and '<|>'.
--
-- Example:
--
-- @
--    data Book = Book { bookdId, author, title :: String, isbn :: Maybe String }
-- @
--
-- @
--    library = many book
--
--    book = 'element' "book" $ do
--             i <- 'attrib' "id"
--             s <- 'optional' ('attrib' "isbn")
--             'children' $ do
--               t <- 'element' "title" $ 'contents' $ 'text'
--               a <- 'element' "author" $ 'contents' $ 'text'
--               return $ Book { bookId = i, author = a, title = t, isbn = s }
-- @
--
-- @
--    extractLibrary :: ['XML.Content'] -> Either 'ExtractionErr' [Book]
--    extractLibrary = 'extractContents' library
-- @
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
  , eoc
  , only
  ) 
where

import Control.Applicative


import           Text.XML.Light.Types as XML
import qualified Text.XML.Light.Proc  as XML

import           Text.XML.Light.Extractors.Internal (ExtractionErr, Err, Path)
import qualified Text.XML.Light.Extractors.Internal as Internal
import           Text.XML.Light.Extractors.Internal.Result hiding (throwError, throwFatal)
import qualified Text.XML.Light.Extractors.Internal.Result as R

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
attribAs :: String -> (String -> Either Err a) -> ElementExtractor a
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
