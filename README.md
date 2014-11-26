xmllightwrapper
===============

Simple wrapper over Text.XML.Light to extract data from parsed xml.

When parsing xml strings with XML.Light you get a list of Content. Then you can use functions from Text.XML.Light.Proc to extract data from that. It is however kind of tricky to deal with potential errors -- we have a parsing problem again.

Instead of Text.XML.Light.Proc one can use this library. It simplifies the process of extracting data from Content while dealing with errors.
