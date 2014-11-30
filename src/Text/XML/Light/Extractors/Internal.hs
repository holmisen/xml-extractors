{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.XML.Light.Extractors.Internal
  ( Path
  , Err(..)
  , ParseErr(..)
    
  -- * Element extraction
  , ElementParser
  , runElementParser
  , attrib
  , attribAs
  , children
  , contents
  
  -- * Contents extraction
  , ContentsParser
  , runContentsParser
  , element
  , text
  , textAs
  , eoc
  ) 
where

import Control.Applicative

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Monoid

import           Text.XML.Light.Types as XML
import qualified Text.XML.Light.Proc  as XML

import           Text.XML.Light.Extractors.Internal.Result hiding (throwError, throwFatal)
import qualified Text.XML.Light.Extractors.Internal.Result as R

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

type ElementParser a = ReaderT (Path, XML.Element) (ResultT ParseErr Identity) a

runElementParser :: ElementParser a -> XML.Element -> Path -> Result ParseErr a
runElementParser p elem path = runIdentity $ runResultT $ runReaderT p (path,elem)

makeElementParser :: Result ParseErr a -> ElementParser a
makeElementParser (Fatal e) = throwFatal e
makeElementParser (Fail e)  = throwError e
makeElementParser (Ok a)    = return a


-- | Extract the value of the attribute with given name.
attrib :: String -> ElementParser String
attrib name = attribAs name return


attribAs :: String -- ^ name of attribute to extract
         -> (String -> Either Err a)
         -> ElementParser a
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
contents :: ContentsParser a -> ElementParser a
contents p = do
  (path,x) <- ask
  let r = runContentsParser p (elContent x) 1 path
  makeElementParser $ fmap fst r


-- | @children p@ parse children elements with @p@. Other contents will be ignored.
children :: ContentsParser a -> ElementParser a
children p = do
  (path,x) <- ask
  let r = runContentsParser p (map XML.Elem $ XML.elChildren x) 1 path
  makeElementParser $ fmap fst r


-- | Lift a string function to an element extractor.
liftToElement :: (String -> Either Err a) -> String -> ElementParser a
liftToElement f s = do
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


eoc :: ContentsParser ()
eoc = do
  (path,_,xs) <- get
  case xs of
    []    -> return ()
    (x:_) -> throwError (ParseErr (ErrEnd x) path)


-- | @element name p@ parses an element with name @name@ using @p@.
element :: String -> ElementParser a -> ContentsParser a
element name p = first expect go
  where
    go c@(Elem x) path
      | elemName x == name = escalate $ runElementParser p x (addElem x path)
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
