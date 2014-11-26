import Control.Applicative

import Data.Maybe
import Data.List (find)

import Numeric (readFloat)

import Text.XML.Light.Extractors
import Text.XML.Light.Input


main = do
  s <- getContents
  let cs = parseXML s
  print (parseContents library cs)


test1 = foo

--------------------------------------------------------------------------------

foo =
  element "foo" $ do
    x <- attrib "x"
    y <- attrib "y"
    children $ do
      cs <- many (bar <|> baz)
      o  <- optional fum
      c  <- coz
      return (x,y, cs ++ maybeToList o ++ [c])


bar = element "bar" $ attrib "a"

baz = element "baz" $ attrib "a"

coz = element "coz" $ attrib "b"

fum = element "fum" $ attrib "c"

--------------------------------------------------------------------------------

data Book = Book { title :: String
                 , author :: String
                 , isbn :: String
                 , published :: Maybe Double
                 }
 deriving Show

type Library = [Book]


book =
  element "book" $ do
    author <- attrib "author"
    isbn   <- attrib "isbn"
    pub    <- optional (attrib "published" >>= double)

    contents $ do
      title <- text
  
      return $ Book title author isbn pub


library = 
  element "library" $ children $ many book


parseDouble :: String -> Maybe Double
parseDouble = fmap fst . find (null . snd) . readFloat 


double = liftToElem (f . parseDouble)  where
  f Nothing  = Left "Not a double"
  f (Just x) = Right x
