{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.XML.Light.Extractors.Internal
  ( Path
  , Err(..)
  , ExtractionErr(..)

  -- * Element extraction
  , ElementExtractor
  , runElementExtractor
  , attrib
  , attribAs
  , children
  , contents

  -- * Contents extraction
  , ContentsExtractor
  , runContentsExtractor
  , element
  , text
  , textAs
  , anyContent
  , eoc
  )
where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import           Text.XML.Light.Types as XML
import qualified Text.XML.Light       as XML

import           Text.XML.Light.Extractors.Internal.Result hiding (throwError, throwFatal)
import qualified Text.XML.Light.Extractors.Internal.Result as R

--------------------------------------------------------------------------------

elemName :: Element -> String
elemName = XML.qName . XML.elName

--------------------------------------------------------------------------------

-- | Location for some content.
--
-- For now it is a reversed list of content indices (starting at 1)
-- and element names. This may change to something less \"stringly
-- typed\".
type Path = [String]


pushIdx :: Int -> Path -> Path
pushIdx i p = show i : p

pushElem :: XML.Element -> Path -> Path
pushElem e p = elemName e : p

pushAttrib :: String -> Path -> Path
pushAttrib a p = ('@':a) : p

--------------------------------------------------------------------------------

-- | Error with a context.
data ExtractionErr = ExtractionErr { err :: Err, context :: Path }
  deriving Show


-- | Extraction errors.
data Err = ErrExpectContent
           { expectedContent :: String
           , foundContent    :: XML.Content
           } -- ^ Some expected content is missing
         | ErrExpectAttrib
           { expectedAttrib :: String       -- ^ name of expected attribute
           , atElement      :: XML.Element  -- ^ element with missing attribute
           } -- ^ An expected attribute is missing
         | ErrAttribValue
           { expectedValue  :: String       -- ^ description of expected value
           , foundValue     :: String       -- ^ the value found
           , atElement      :: XML.Element  -- ^ element with bad attribute
           } -- ^ An attribute value was bad
         | ErrEnd
           { foundContent   :: XML.Content
           } -- ^ Expected end of contents
         | ErrNull
           { expectedContent :: String
           } -- ^ Unexpected end of contents
         | ErrMsg String
  deriving Show


instance Error ExtractionErr where
  strMsg msg = ExtractionErr (ErrMsg msg) []


throwError = lift . R.throwError

throwFatal = lift . R.throwFatal

--------------------------------------------------------------------------------

type ElementExtractor a = ReaderT (Path, XML.Element) (ResultT ExtractionErr Identity) a

runElementExtractor :: ElementExtractor a -> XML.Element -> Path -> Result ExtractionErr a
runElementExtractor p elem path = runIdentity $ runResultT $ runReaderT p (path, elem)

makeElementExtractor :: Result ExtractionErr a -> ElementExtractor a
makeElementExtractor (Fatal e) = throwFatal e
makeElementExtractor (Fail e)  = throwError e
makeElementExtractor (Ok a)    = return a


attrib :: String -> ElementExtractor String
attrib name = attribAs name return


attribAs :: String -- ^ name of attribute to extract
         -> (String -> Either String a) -- ^ function returning given string to some value or an error message
         -> ElementExtractor a
attribAs name f = do
  (path,x) <- ask
  let path' = pushAttrib name path
  case XML.lookupAttr (XML.unqual name) (elAttribs x) of
    Nothing -> throwError $ ExtractionErr (ErrExpectAttrib name x) path
    Just s  ->
      case f s of
        Left e  -> throwFatal $ ExtractionErr (ErrAttribValue e s x) path'
        Right a -> return a


contents :: ContentsExtractor a -> ElementExtractor a
contents p = do
  (path,x) <- ask
  let r = runContentsExtractor p (elContent x) 1 path
  makeElementExtractor $ fmap fst r


children :: ContentsExtractor a -> ElementExtractor a
children p = do
  (path,x) <- ask
  let r = runContentsExtractor p (map XML.Elem $ XML.elChildren x) 1 path
  makeElementExtractor $ fmap fst r


-- -- | Lift a string function to an element extractor.
-- liftToElement :: (String -> Either Err a) -> String -> ElementExtractor a
-- liftToElement f s = do
--   (path,_) <- ask
--   case f s of
--     Left e   -> throwError (ExtractionErr e path)
--     Right a  -> return a

--------------------------------------------------------------------------------

type Ctx = (Path, Int, [XML.Content])

type ContentsExtractor a = StateT Ctx (ResultT ExtractionErr Identity) a

runContentsExtractor :: ContentsExtractor a -> [Content] -> Int -> Path -> Result ExtractionErr (a, Ctx)
runContentsExtractor p contents i path =
  runIdentity $ runResultT $ runStateT p (path, i, contents)


first :: String -> (Content -> Path -> Result ExtractionErr a) -> ContentsExtractor a
first expect f = do
  (path,i,xs) <- get
  case xs of
    []     -> throwError $ ExtractionErr (ErrNull expect) path
    (x:xs) -> do
      case f x (pushIdx i path) of
        Fatal e -> throwFatal e
        Fail  e -> throwError e
        Ok    a -> do
          put (path,i+1,xs)
          return a


element :: String -> ElementExtractor a -> ContentsExtractor a
element name p = first expect go
  where
    go (Elem x) path
      | elemName x == name = escalate $ runElementExtractor p x (pushElem x path)
    go c        path       = Fail (ExtractionErr (ErrExpectContent expect c) path)

    expect = "element " ++ show name


textAs :: (String -> Either Err a) -> ContentsExtractor a
textAs f = first "text" go
  where
    go (Text x) path =
      case f (cdData x) of
        Left e  -> Fatal $ ExtractionErr e path
        Right s -> return s
    go c path = Fail $ ExtractionErr (ErrExpectContent "text" c) path


text :: ContentsExtractor String
text = textAs return


anyContent :: ContentsExtractor Content
anyContent = first "something" (const . return)


eoc :: ContentsExtractor ()
eoc = do
  (path,_,xs) <- get
  case xs of
    []    -> return ()
    (x:_) -> throwError (ExtractionErr (ErrEnd x) path)
