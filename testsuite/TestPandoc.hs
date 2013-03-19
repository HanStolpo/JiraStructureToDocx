-- Blah blah
 {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Typeable
import Data.Data
import Control.Monad.Error
import GHC.Generics
import Control.Exception
import Control.Applicative
import Text.Pandoc
import Data.ByteString.Lazy.Char8 as BS
import System.Directory

 
main =  do
    cd <- getCurrentDirectory
    dc <- writeDocx def testDoc
    BS.writeFile (cd ++ "/test.docx") dc

testDocMeta = Meta {docTitle = [Str "TestDoc"], docAuthors=[[Str "Test"]], docDate=[Str "Bla"]}
testDocBlocks = 
    [
        Header 1 nullAttr [Str "Heading 1"],
        Para [Str "Some text", LineBreak, Str "Some more text", Space, Space, Str "Text before spaces"],
        Header 2 nullAttr [Str "Heading 1.1"],
        Para [Str "Some text", LineBreak, Str "Some more text", Space, Space, Str "Text before spaces"],
        Header 1 nullAttr [Str "Heading 2"],
        Header 2 nullAttr [Str "Heading 2.2"],
        Para [Str "Some text", LineBreak, Str "Some more text", Space, Space, Str "Text before spaces"],
        Para [Image [Str "the image"] ("SomeImage.png", "My some image")],
        OrderedList (10, DefaultStyle, DefaultDelim) [[Para [Str "xx1 ", Str "blah |"]]
                                                     ,[Para [Str "xx2 ", Str "lhsl |"]] 
                                                     ,[OrderedList (1, DefaultStyle, DefaultDelim)[[Para [Str "yyy 1", Str "blah |"]]
                                                                                                  ,[Para [Str "yyy 2", Str "lhsl |"]] 
                                                                                                  ]]
                                                     ,[OrderedList (1, DefaultStyle, DefaultDelim)[[Para [Str "yyy 3", Str "blah |"]]
                                                                                                  ,[Para [Str "yyy 4", Str "lhsl |"]] 
                                                                                                  ]]
                                                     ]
    ]
testDoc = Pandoc testDocMeta testDocBlocks
