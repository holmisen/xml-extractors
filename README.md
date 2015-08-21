xml-extractors
==============

This is an extension to the
[xml](http://hackage.haskell.org/package/xml) package, providing
functions to extract data from parsed xml.


## Motivation

The `xml` package provides functions to parse and get information from
xml data. It will parse an xml string into a generic tree
representation. Extracting information from such a tree to some custom 
data structure while keeping track of location to handle errors is
tricky. This library helps with that.

If there is an error during extraction (expected information is absent
or wrong), it will return an error value with position
information. The idea is to provide decent error messages.


## Example usage

Suppose you have an xml file of books like this:

```xml
<?xml version="1.0"?>
<library>
  <book id="1" isbn="23234-1">
    <author>John Doe</author>
    <title>Some book</title>
  </book>
  <book id="2">
    <author>You</author>
    <title>The Great Event</title>
  </book>
  ...
</library>
```

and a Haskell data type to represent a book:

```haskell
data Book = Book { bookId        :: Int
                 , isbn          :: Maybe String
                 , author, title :: String
                 }
```

You can parse the xml file into a generic tree structure using
`parseXMLDoc` from the `xml` package. Then to transform this generic
xml tree into `Book` objects you define extractors for books, like
so:

```haskell
book = element "book" $ do
         i <- attribAs "id" integer
         s <- optional (attrib "isbn")
         children $ do
           a <- element "author" $ contents $ text
           t <- element "title" $ contents $ text
           return Book { bookId = i, author = a, title = t, isbn = s }

library = element "library" $ children $ only $ many book

extractLibrary :: Element -> Either ExtractionErr [Book]
extractLibrary = extractDocContents library
```
