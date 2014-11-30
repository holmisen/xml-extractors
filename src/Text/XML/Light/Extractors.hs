{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

-- | PRELIMINARY DRAFT
--
-- A library to make extracting information from parsed XML easier.
--
-- The 'Control.Applicative' module contains some useful combinators
-- like 'optional', 'many' and '<|>'.
--
-- Example:
--
-- @
--    data Book = Book { bookdId, author, title, isbn :: String }
-- @
--
-- @
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
--    bookParse = 'parseElement' book
-- @
--
-- __TODO__: Newtype wrappers
--

module Text.XML.Light.Extractors
  ( Path
  , Err(..)
  , ParseErr(..)
    
  -- * Element extraction
  , ElementParser
  , parseElement
  , attrib
  , attribAs
  , children
  , contents
  
  -- * Contents extraction
  , ContentsParser
  , parseContents
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

import           Text.XML.Light.Extractors.Internal (ParseErr, Err, Path)
import qualified Text.XML.Light.Extractors.Internal as Internal
import           Text.XML.Light.Extractors.Result hiding (throwError, throwFatal)
import qualified Text.XML.Light.Extractors.Result as R

--------------------------------------------------------------------------------

newtype ElementParser a = ElementParser (Internal.ElementParser a)
 deriving (Applicative, Alternative, Functor, Monad)


newtype ContentsParser a = ContentsParser (Internal.ContentsParser a)
 deriving (Applicative, Alternative, Functor, Monad)

--------------------------------------------------------------------------------
 
-- | @parseElement p element@ parses @element@ with @p@.
parseElement :: ElementParser a -> XML.Element -> Either ParseErr a
parseElement (ElementParser p) elem = toEither $ Internal.runElementParser p elem []


attrib :: String -> ElementParser String
attrib = ElementParser . Internal.attrib

attribAs :: String -> (String -> Either Err a) -> ElementParser a
attribAs name = ElementParser . (Internal.attribAs name)


children :: ContentsParser a -> ElementParser a
children (ContentsParser p) = ElementParser (Internal.children p)


contents :: ContentsParser a -> ElementParser a
contents (ContentsParser p) = ElementParser (Internal.contents p)

--------------------------------------------------------------------------------

-- | @parseContents p contents@ parses the contents with @p@.
parseContents :: ContentsParser a -> [XML.Content] -> Either ParseErr a
parseContents (ContentsParser p) cs =
  toEither (fst <$> Internal.runContentsParser p cs 1 [])


only :: ContentsParser a -> ContentsParser a
only p = p <* eoc


eoc :: ContentsParser ()
eoc = ContentsParser Internal.eoc


element :: String -> ElementParser a -> ContentsParser a
element name (ElementParser a) = ContentsParser $ Internal.element name a


text :: ContentsParser String
text = ContentsParser Internal.text

textAs :: (String -> Either Err a) -> ContentsParser a
textAs = ContentsParser . Internal.textAs
