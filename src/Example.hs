import Control.Applicative

import Data.Maybe

import Text.XML.Light.Extractors
import Text.XML.Light.Input


main = do
  contents <- parseXML <$> getContents
  putStrLn $ either showExtractionErr show (extractContents example2 contents)


example1 = foo
example2 = library

--------------------------------------------------------------------------------

foo =
  element "foo" $ do
    x <- attrib "x"
    y <- attrib "y"
    children $ do
      cs <- many (bar <|> baz)
      ola <- ola
      o  <- optional fum
      c  <- coz
      return (x,y, cs ++ [ola] ++ maybeToList o ++ [c])


bar = element "bar" $ attrib "a"

baz = element "baz" $ attrib "a"

coz = element "coz" $ attrib "b"

fum = element "fum" $ attrib "c"

ola = element "ola" $ children $ only coz

--------------------------------------------------------------------------------

data Book = Book { title     :: String
                 , author    :: String
                 , isbn      :: String
                 , published :: Maybe Double
                 }
 deriving Show

type Library = [Book]


book =
  element "book" $ do
    author <- attrib "author"
    isbn   <- attrib "isbn"
    pub    <- optional $ attribAs "published" float
    contents $ do
      title <- text
      return $ Book title author isbn pub


library = 
  element "library" $ children $ many book
