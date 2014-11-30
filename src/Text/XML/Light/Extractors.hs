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
--    libraryParse :: ['XML.Content'] -> Either 'ParseErr' [Book]
--    libraryParse = 'parseContents' library
-- @
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
import           Text.XML.Light.Extractors.Internal.Result hiding (throwError, throwFatal)
import qualified Text.XML.Light.Extractors.Internal.Result as R

--------------------------------------------------------------------------------

newtype ElementParser a = ElementParser (Internal.ElementParser a)
 deriving (Applicative, Alternative, Functor, Monad)


newtype ContentsParser a = ContentsParser (Internal.ContentsParser a)
 deriving (Applicative, Alternative, Functor, Monad)

--------------------------------------------------------------------------------
 
-- | @parseElement p element@ parses @element@ with @p@.
parseElement :: ElementParser a -> XML.Element -> Either ParseErr a
parseElement (ElementParser p) elem = toEither $ Internal.runElementParser p elem []


-- | @attrib name@ extracts the value of attribute @name@.
attrib :: String -> ElementParser String
attrib = ElementParser . Internal.attrib


-- | @attribAs name f@ extracts the value of attribute @name@ and runs
-- it through a conversion/validation function.
attribAs :: String -> (String -> Either Err a) -> ElementParser a
attribAs name = ElementParser . (Internal.attribAs name)


-- | @children p@ extract only child elements with @p@.
children :: ContentsParser a -> ElementParser a
children (ContentsParser p) = ElementParser (Internal.children p)


-- | @contents p@ extract contents with @p@.
contents :: ContentsParser a -> ElementParser a
contents (ContentsParser p) = ElementParser (Internal.contents p)

--------------------------------------------------------------------------------

-- | @parseContents p contents@ parses the contents with @p@.
parseContents :: ContentsParser a -> [XML.Content] -> Either ParseErr a
parseContents (ContentsParser p) cs =
  toEither (fst <$> Internal.runContentsParser p cs 1 [])


-- | @only p@ fails if there is more contents than parsed by @p@.
only :: ContentsParser a -> ContentsParser a
only p = p <* eoc


-- | Succeeds only when there is no more content.
eoc :: ContentsParser ()
eoc = ContentsParser Internal.eoc


-- | @element name p@ extracts a @name@ element with @p@.
element :: String -> ElementParser a -> ContentsParser a
element name (ElementParser a) = ContentsParser $ Internal.element name a


-- | Extracts text.
text :: ContentsParser String
text = ContentsParser Internal.text


-- | Extracts text applied to a conversion function.
textAs :: (String -> Either Err a) -> ContentsParser a
textAs = ContentsParser . Internal.textAs
