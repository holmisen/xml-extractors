import Control.Applicative

import Data.Maybe

import Text.XML.Light.Wrapper
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
                 , published :: String 
                 }
 deriving Show

type Library = [Book]


book =
  element "book" $ do
    author <- attrib "author"
    isbn   <- attrib "isbn"
    pub    <- optional (attrib "published")

    contents $ do
      title <- text
  
      return $ Book title author isbn (maybe "<unknown>" id pub)


library = 
  element "library" $ children $ many book
