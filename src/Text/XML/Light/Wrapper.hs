-- | Preliminary draft
--
-- A library to make extracting information from parsed XML easier.
--
-- The 'Control.Applicative' module contains some useful combinators.
--
-- Example:
--
-- @
--    foo = 'element' "book" $ do
--            i <- 'attrib' "id"
--            s <- 'optional' ('attrib' "isbn")
--            'children' $ do
--              t <- 'element' "title" $ 'contents' $ 'text'
--              a <- 'element' "author" $ 'contents' $ 'text'
--              return $ Book { bookId = i, author = a, title = t, isbn = s }
-- @
--

module Text.XML.Light.Wrapper
  ( Path
  , Err(..)
    
  -- * Element extraction
  , ElemParser
  , runElemParser
  , attrib
  , children
  , contents
  
  -- * Contents extraction
  , ContentsParser
  , runContentsParser
  , parseContents
  , element
  , text
  , eoc
  ) 
where

import Control.Applicative

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Monoid

import           Text.XML.Light.Types as XML
import qualified Text.XML.Light.Proc  as XML

--------------------------------------------------------------------------------

elemName = qName . elName

qname name = QName name Nothing Nothing

--------------------------------------------------------------------------------

-- | Location for some content.
type Path = [String]

-- | Parsing errors.
data Err = ErrExpect
             { expected :: String      -- ^ expected content
             , found    :: XML.Content -- ^ found content
             , location :: Path 
             } -- ^ Some expected content is missing
         | ErrAttr 
             { expected :: String       -- ^ expected attribute
             , atAttrib :: XML.Element  -- ^ element with missing attribute
             , location :: Path
             } -- ^ An expected attribute is missing
         | ErrEnd 
             { found    :: XML.Content
             , location :: Path
             } -- ^ Expected end of contents
         | ErrNull
             { expected :: String
             , location :: Path
             } -- ^ Unexpected end of contents
         | ErrMsg String
  deriving Show


instance Error Err where
  strMsg = ErrMsg

--------------------------------------------------------------------------------

type ElemParser a = ReaderT (Path, XML.Element) (ErrorT Err Identity) a

runElemParser :: Path -> XML.Element -> ElemParser a -> Either Err a
runElemParser path elem p = runIdentity $ runErrorT $ runReaderT p (path,elem)


-- | Extract the value of the attribute with given name.
attrib :: String -- ^ name of attribute to extract
       -> ElemParser String
attrib name = do
  (p,x) <- ask
  case XML.lookupAttr (qname name) (elAttribs x) of
    Nothing -> throwError (ErrAttr name x p)
    Just v  -> return v


-- | @contents p@ parse contents with @p@.
contents :: ContentsParser a -> ElemParser a
contents p = do
  (path,x) <- ask
  case runContentsParser (elemName x : path) 0 (elContent x) p of
    Left e -> throwError e
    Right (a,_) -> return a


-- | @children p@ parse children elements with @p@. Other contents will be ignored.
children :: ContentsParser a -> ElemParser a
children p = do
  (path,x) <- ask
  case runContentsParser (qName (elName x) : path) 0 (map XML.Elem $ XML.elChildren x) p of
    Left e -> throwError e
    Right (a,_) -> return a

--------------------------------------------------------------------------------

type Ctx = (Path, Int, [XML.Content])

type ContentsParser a = StateT Ctx (ErrorT Err Identity) a

runContentsParser :: Path -> Int -> [Content] -> ContentsParser a -> Either Err (a, Ctx)
runContentsParser path i contents p = runIdentity $ runErrorT $ runStateT p (path,i,contents)


-- | @parseContents p contents@ parses the contents with @p@.
parseContents :: ContentsParser a -> [XML.Content] -> Either Err a
parseContents p cs = fst <$> runContentsParser [] 0 cs p


first :: (Path -> Err) -> (Content -> Path -> Either Err a) -> ContentsParser a
first e f = do
  (path,i,xs) <- get
  let path' = ("[" ++ show i ++ "]") : path
  case xs of
    []     -> throwError $ e path'
    (x:xs) -> do
      case f x path' of
        Left e -> throwError e
        Right a -> do
          put (path, i+1, xs)
          return a


-- | @element name p@ parses an element with name @name@ using @p@.
element :: String -> ElemParser a -> ContentsParser a
element name p = first (ErrNull name) go
  where
    go c@(Elem x) path
      | elemName x /= name = err c path
      | otherwise          = runElemParser path x p
    go c path = err c path

    err c path = Left (ErrExpect name c path)


-- | Extracts text.
text :: ContentsParser String
text = first (ErrNull "text") go 
  where
    go (Text x) _    = return (cdData x)
    go c        path = Left (ErrExpect "text" c path)


eoc :: ContentsParser ()
eoc = do
  (path,i,xs) <- get
  case xs of
    [] -> return ()
    (x:_) -> throwError (ErrEnd x path)
