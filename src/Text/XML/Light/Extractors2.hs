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
    
  -- * Element extraction
  , ElemParser
  , runElemParser
  , parseElem
  , attrib
  , children
  , contents
  , liftToElem
  
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


addIdx :: Int -> Path -> Path
addIdx i p = ("[" ++ show i ++ "]") : p

addElem :: XML.Element -> Path -> Path
addElem e p = elemName e : p

addAttrib :: String -> Path -> Path
addAttrib a p = ('@':a) : p


data ParseErr = ParseErr { err :: Err, context :: Path }

-- | Parsing errors.
data Err = ErrExpect
           { expected :: String      -- ^ expected content
           , found    :: XML.Content -- ^ found content
           } -- ^ Some expected content is missing
         | ErrExpect2
           { expected :: String            -- ^ expected content
           , found2   :: Maybe XML.Content -- ^ found content
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

--------------------------------------------------------------------------------

type Parser a b = ReaderT (Path, a) (ErrorT ParseErr Identity) b

runParser p a path = runIdentity $ runErrorT $ runReaderT p (path,a)

satisfy :: Show a => (a -> Bool) -> Parser a ()
satisfy p = do
  (path,a) <- ask
  if p a
     then return ()
     else throwError $ ParseErr (ErrMsg (show a ++ " is wrong")) path


(<?>) :: Parser a b -> Err ->  Parser a b
p <?> e = do
  (path,a) <- ask
  case runParser p a path of
    Left _ -> throwError $ ParseErr e path
    Right b -> return b

--------------------------------------------------------------------------------

type ElemParser a = Parser XML.Element a

runElemParser :: Path -> XML.Element -> ElemParser a -> Either ParseErr a
runElemParser path elem p = runIdentity $ runErrorT $ runReaderT p (path,elem)

-- | @parseElem p element@ parses @element@ with @p@.
parseElem :: ElemParser a -> XML.Element -> Either ParseErr a
parseElem p elem = runElemParser [] elem p


-- | Extract the value of the attribute with given name.
attrib :: String -- ^ name of attribute to extract
       -> ElemParser String
attrib name = do
  (p,x) <- ask
  case XML.lookupAttr (qname name) (elAttribs x) of
    Nothing -> throwError (ParseErr (ErrAttr name x) p)
    Just v  -> return v


-- | @contents p@ parse contents with @p@.
contents :: ContentsParser a -> ElemParser a
contents p = do
  (path,x) <- ask
  case runContentsParser (elemName x : path) (elContent x) p of
    Left e -> throwError e
    Right (a,_) -> return a


-- | @children p@ parse children elements with @p@. Other contents will be ignored.
children :: ContentsParser a -> ElemParser a
children p = do
  (path,x) <- ask
  case runContentsParser (elemName x : path) (map XML.Elem $ XML.elChildren x) p of
    Left e -> throwError e
    Right (a,_) -> return a


-- | Lift a string function to an element extractor.
liftToElem :: (String -> Either Err a) -> String -> ElemParser a
liftToElem f s = do
  (path,x) <- ask
  case f s of
    Left e   -> throwError (ParseErr e path)
    Right a  -> return a
  

--------------------------------------------------------------------------------

type Ctx = (Path, Int, [XML.Content])

type ContentsParser a = StateT Ctx (ErrorT ParseErr Identity) a

runContentsParser :: Path -> [Content] -> ContentsParser a -> Either ParseErr (a, Ctx)
runContentsParser path contents p = 
  runIdentity $ runErrorT $ runStateT p (path, 1, contents)


-- | @parseContents p contents@ parses the contents with @p@.
parseContents :: ContentsParser a -> [XML.Content] -> Either ParseErr a
parseContents p cs = fst <$> runContentsParser [] cs p


first :: String -> (Content -> Path -> Either ParseErr a) -> ContentsParser a
first expect f = do
  (path,i,xs) <- get
  case xs of
    []     -> throwError $ ParseErr (ErrNull expect) path
    (x:xs) -> do
      let path' = addIdx i path
      case f x path' of
        Left e -> throwError e
        Right a -> do
          put (path,i+1,xs)
          return a


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
      | elemName x == name = runElemParser path x p
    go c          path     = Left (ParseErr (ErrExpect expect c) path)

    expect = "element " ++ show name


-- -- | Extracts text.
-- text :: ContentsParser String
-- text = first "text" go 
--   where
--     go (Text x) _    = return (cdData x)
--     go c        path = Left (ParseErr (ErrExpect "text" c) path)


-- | Lift a string function to a content extractor.
-- DOES NOT MAKE SENSE

-- liftToContent :: (String -> Either Err a) -> String -> ContentsParser a
-- liftToContent f s = do
--   (path,i,xs) <- get
--   case f s of
--     Left e  -> throwError (ParseErr e (addIdx i path))
--     Right a -> return a

--------------------------------------------------------------------------------

type ContentParser a = Parser (Maybe XML.Content) a

text :: ContentParser String
text = do
  (path,cm) <- ask
  case cm of
    Just (Text d) -> return (cdData d)
    other         -> throwError $ ParseErr (ErrExpect2 "text" cm) path

