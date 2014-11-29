{-# LANGUAGE NoMonomorphismRestriction #-}

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
--    bookParse = 'parseElem' book
-- @
--
-- __TODO__: Newtype wrappers
--

module Text.XML.Light.Extractors
  ( Path
  , Err(..)
  , ParseErr(..)
    
  -- * Element extraction
  , ElemParser
  , runElemParser
  , parseElem
  , attrib
  , attribAs
  , children
  , contents
  
  -- * Contents extraction
  , ContentsParser
  , runContentsParser
  , parseContents
  , element
  , text
  , textAs
  , only
  , eoc
  ) 
where

import Control.Applicative

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Monoid

import qualified Safe

import           Text.XML.Light.Types as XML
import qualified Text.XML.Light.Proc  as XML

import Text.XML.Light.Extractors.Result hiding (throwError, throwFatal)
import qualified Text.XML.Light.Extractors.Result as R

--------------------------------------------------------------------------------

elemName = qName . elName

qname name = QName name Nothing Nothing

--------------------------------------------------------------------------------

-- | Location for some content.
type Path = [String]


addIdx :: Int -> Path -> Path
addIdx i p = show i : p

addElem :: XML.Element -> Path -> Path


addElem e p = elemName e : p

addAttrib :: String -> Path -> Path
addAttrib a p = ('@':a) : p

--------------------------------------------------------------------------------

data ParseErr = ParseErr { err :: Err, context :: Path }
  deriving Show

-- | Parsing errors.
data Err = ErrExpect
           { expected :: String      -- ^ expected content
           , found    :: XML.Content -- ^ found content
           } -- ^ Some expected content is missing
         | ErrAttr 
           { expected  :: String       -- ^ expected attribute
           , atElement :: XML.Element  -- ^ element with missing attribute
           } -- ^ An expected attribute is missing
         | ErrEnd 
           { found    :: XML.Content
           } -- ^ Expected end of contents
         | ErrNull
           { expected :: String  -- ^ expected content
           } -- ^ Unexpected end of contents
         | ErrMsg String
  deriving Show


instance Error ParseErr where
  strMsg msg = ParseErr (ErrMsg msg) []


throwError = lift . R.throwError

throwFatal = lift . R.throwFatal

--------------------------------------------------------------------------------

type ElemParser a = ReaderT (Path, XML.Element) (ResultT ParseErr Identity) a

runElemParser :: ElemParser a -> XML.Element -> Path -> Result ParseErr a
runElemParser p elem path = runIdentity $ runResultT $ runReaderT p (path,elem)

-- | @parseElem p element@ parses @element@ with @p@.
parseElem :: ElemParser a -> XML.Element -> Either ParseErr a
parseElem p elem = toEither $ runElemParser p elem []


makeElemParser :: Result ParseErr a -> ElemParser a
makeElemParser (Fatal e) = throwFatal e
makeElemParser (Fail e)  = throwError e
makeElemParser (Ok a)    = return a


-- | Extract the value of the attribute with given name.
attrib :: String -> ElemParser String
attrib name = attribAs name return


attribAs :: String -- ^ name of attribute to extract
         -> (String -> Either Err a)
         -> ElemParser a
attribAs name f = do
  (path,x) <- ask
  let path' = addAttrib name path
  case XML.lookupAttr (qname name) (elAttribs x) of
    Nothing -> throwError $ ParseErr (ErrAttr name x) path
    Just s  ->
      case f s of
        Left e  -> throwFatal $ ParseErr e path'
        Right a -> return a


-- | @contents p@ parse contents with @p@.
contents :: ContentsParser a -> ElemParser a
contents p = do
  (path,x) <- ask
  let r = runContentsParser p (elContent x) 1 path
  makeElemParser $ fmap fst r


-- | @children p@ parse children elements with @p@. Other contents will be ignored.
children :: ContentsParser a -> ElemParser a
children p = do
  (path,x) <- ask
  let r = runContentsParser p (map XML.Elem $ XML.elChildren x) 1 path
  makeElemParser $ fmap fst r


-- | Lift a string function to an element extractor.
liftToElem :: (String -> Either Err a) -> String -> ElemParser a
liftToElem f s = do
  (path,x) <- ask
  case f s of
    Left e   -> throwError (ParseErr e path)
    Right a  -> return a
  

--------------------------------------------------------------------------------

type Ctx = (Path, Int, [XML.Content])

type ContentsParser a = StateT Ctx (ResultT ParseErr Identity) a

runContentsParser :: ContentsParser a -> [Content] -> Int -> Path -> Result ParseErr (a, Ctx)
runContentsParser p contents i path = 
  runIdentity $ runResultT $ runStateT p (path, i, contents)


-- | @parseContents p contents@ parses the contents with @p@.
parseContents :: ContentsParser a -> [XML.Content] -> Either ParseErr a
parseContents p cs = toEither (fst <$> runContentsParser p cs 1 [])


first :: String -> (Content -> Path -> Result ParseErr a) -> ContentsParser a
first expect f = do
  (path,i,xs) <- get
  case xs of
    []     -> throwError $ ParseErr (ErrNull expect) path
    (x:xs) -> do
      case f x (addIdx i path) of
        Fatal e -> throwFatal e
        Fail  e -> throwError e
        Ok    a -> do
          put (path,i+1,xs)
          return a


only :: ContentsParser a -> ContentsParser a
only p = p <* eoc


eoc :: ContentsParser ()
eoc = do
  (path,_,xs) <- get
  case xs of
    []    -> return ()
    (x:_) -> throwError (ParseErr (ErrEnd x) path)


-- | @element name p@ parses an element with name @name@ using @p@.
element :: String -> ElemParser a -> ContentsParser a
element name p = first expect go
  where
    go c@(Elem x) path
      | elemName x == name = escalate $ runElemParser p x (addElem x path)
    go c          path     = Fail (ParseErr (ErrExpect expect c) path)

    expect = "element " ++ show name


-- | Extracts text applied to a conversion function.
textAs :: (String -> Either Err a) -> ContentsParser a
textAs f = first "text" go 
  where
    go (Text x) path =
      case f (cdData x) of
        Left e  -> Fatal $ ParseErr e path
        Right s -> return s
    go c path = Fail $ ParseErr (ErrExpect "text" c) path


-- | Extracts text.
text = textAs return
