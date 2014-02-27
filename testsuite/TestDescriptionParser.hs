{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables#-}

-- GHC_STATIC_OPTION_i=../src:../testsuite


module Main where

import DescriptionParserInner hiding (subscript, superscript, strong, bulletList,table, image)
import qualified DescriptionParserInner as D (subscript, superscript, strong, bulletList,table)
import Test.HUnit
import System.Exit (exitFailure)
import Text.Parsec
import Text.Pandoc
import Text.Pandoc.Builder hiding (space)
import Control.Monad
import qualified Text.Pandoc.Builder  as PB (space)
import Control.Applicative ((<$>), (<$))
import Data.Monoid (mempty)
import Debug.Trace

----------------------------------------------------------------------------------------
-- HUnit parser tests
----------------------------------------------------------------------------------------
testPS ::  ParseState -> MyParser b -> String -> Either String b
testPS s p i = 
    case res of 
        Left e -> Left $ show e ++ " for input : " ++ i
        Right a -> Right a
    where
        res = runParser p s "" i

testP :: MyParser b -> String -> Either String b
testP = testPS defaultParseState

testPrsLeftOver :: MyParser String
testPrsLeftOver = many anyChar

toInline :: Inlines -> Inline
toInline = head . toList

toBlock :: Blocks -> Block
toBlock = head . toList

olist :: Int -> [Blocks] -> Blocks
olist d = orderedListWith (d, listFmt d, DefaultDelim)

tbl :: Int -> [Blocks] -> [[Blocks]] -> Blocks
tbl c = table (str "") (replicate c (AlignLeft,0))

matchError ::  Show a => Either t a -> Bool
matchError (Right a) = trace (show a) False
matchError (Left _) = True

(<|) :: (b -> a) -> b -> a
infixl 8 <|
f <| a = f a

descriptionParserTests ::  Test
descriptionParserTests = test
        [   "eol 1" ~: Right ()  ~=? testP eol "\n",
            "eol 2" ~: Right ()  ~=? testP eol "\r\n",
            "eol 3" ~: Right ()  ~=? testP eol "\r",
            "eol 4" ~: Right ()  ~=? testP eol "\n\r",
            "eol 5" ~: Right "a"  ~=? testP (eol >> testPrsLeftOver) "\n\ra",
            "eol 6" ~: Right "a"  ~=? testP (eol >> testPrsLeftOver) "\na",
            "eol 7" ~: Right "a"  ~=? testP (eol >> testPrsLeftOver) "\ra",
            "eol 8" ~: Right "a"  ~=? testP (eol >> testPrsLeftOver) "\r\na",

            "bol 1" ~:  True ~=? matchError (testP (anyChar >> bol) "a\n"),
            "bol 2" ~:  Right () ~=? testP (anyChar >> anyChar >> bol) "a\n",
            "bol 3" ~:  Right () ~=? testP bol "a\n",

            "bof 1" ~:  True ~=? matchError (testP (anyChar >> bof) "a\n"),
            "bof 2" ~:  True ~=? matchError (testP (anyChar >> bof) "\n\n"),
            "bof 3" ~:  Right () ~=? testP bof "a\n",

            "lookBack' 1" ~: Right ('s','b') ~=? testP (lookBack' 1 (char 's') (char 's' >> char 'b')) "sb",
            "lookBack' 2" ~: Right ("ssss",'b') ~=? testP (lookBack' 1 (many . char $ 's') (char 's' >> char 'b')) "ssssb",
            "lookBack' 3" ~: Right ("ssss",'b') ~=? testP (lookBack' 2 (many . char $ 's') (char 's' >> char 's' >> char 'b')) "ssssb",

            "notFollowedBy'  1" ~: Right 'c'
                ~=? testP (char 'c' >>= (<$ (notFollowedBy . char $ 'b'))) "ca",
            "notFollowedBy'  2"
                ~: Right 'c' ~=? testP (char 'c' >>= (<$ notFollowedBy (mzero :: MyParser Int))) "ca",
            "notFollowedBy'  2"
                ~: Right 'c' ~=? testP (char 'c' >>= (<$ notFollowedBy (return ()))) "ca",
            "notFollowedBy'  3"
                ~: True ~=? matchError (testP (char 'c' >> notFollowedBy (char 'a')) "ca"),
            "notFollowedBy'  4"
                ~: Right "a" ~=? testP (char 'c' >> notFollowedBy (char 'b') >> testPrsLeftOver) "ca",
            "notFollowedBy'  5"
                ~: Right "ca" ~=? testP (try (char 'c' >> notFollowedBy (char 'a') >> return "") <|>  testPrsLeftOver) "ca",

            "nonSkipChar 1" ~:  Right 'a' ~=? testP (nonSkipChar 'a') "a",
            "nonSkipChar 2" ~:  True  ~=? matchError (testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (nonSkipChar 'a') "a"),
            "nonSkipChar 3" ~:  Right "cc" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (many . nonSkipChar $ 'c') "cca",
            "nonSkipChar 4" ~:  Right "a" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) ((many . nonSkipChar $ 'c')>>testPrsLeftOver) "cca",

            "anyNonSkipChar 1" ~:  Right 'a' ~=? testP anyNonSkipChar  "a",
            "anyNonSkipChar 2" ~:  True ~=? matchError (testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) anyNonSkipChar  "a"),
            "anyNonSkipChar 3" ~:  Right "cc" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (many  anyNonSkipChar) "cca",
            "anyNonSkipChar 4" ~:  Right "a" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (many  anyNonSkipChar >> testPrsLeftOver) "cca",

            "oneOfNonSkipChar 1" ~:  Right 'a' ~=? testP (oneOfNonSkipChar "a") "a",
            "oneOfNonSkipChar 2" ~:  True ~=? matchError (testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (oneOfNonSkipChar "a") "a"),
            "oneOfNonSkipChar 3" ~:  Right "cc" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (many . oneOfNonSkipChar $ "ca") "cca",
            "oneOfNonSkipChar 4" ~:  Right "a" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) ((many . oneOfNonSkipChar $ "ca")>>testPrsLeftOver) "cca",

            "noneOfNonSkipChar 1" ~:  Right 'c' ~=? testP (noneOfNonSkipChar "a") "c",
            "noneOfNonSkipChar 2" ~:  True ~=? matchError (testP  (noneOfNonSkipChar "a") "a"),
            "noneOfNonSkipChar 2" ~:  True ~=? matchError (testPS (defaultParseState {psSkipCharsP = [void (char 'c')]}) (noneOfNonSkipChar "a") "c"),

            "escapedChar 1" ~:  Right '*' ~=? testP escapedChar "\\*",
            "escapedChar 2" ~:  Right '_' ~=? testP escapedChar "\\_",
            "escapedChar 3" ~:  Right '^' ~=? testP escapedChar "\\^",
            "escapedChar 4" ~:  Right '+' ~=? testP escapedChar "\\+",
            "escapedChar 5" ~:  Right '-' ~=? testP escapedChar "\\-",
            "escapedChar 6" ~:  Right '#' ~=? testP escapedChar "\\#",
            "escapedChar 7" ~:  Right '\\' ~=? testP escapedChar "\\a",

            "spaceChar 1" ~:  Right ' ' ~=? testP spaceChar " ",
            "spaceChar 2" ~:  Right '\t' ~=? testP spaceChar "\t",
            "spaceChar 3" ~:  True ~=? matchError (testP spaceChar "\n"),
            "spaceChar 4" ~:  True ~=? matchError (testP spaceChar "\n\r"),

            "skipSpaces 1" ~:  Right "" ~=? testP (skipSpaces >> testPrsLeftOver) " ",
            "skipSpaces 2" ~:  Right () ~=? testP skipSpaces "    \n",
            "skipSpaces 2" ~:  Right "\n" ~=? testP (skipSpaces >> testPrsLeftOver) "    \n",
            "skipSpaces 3" ~:  Right "\r\n" ~=? testP (skipSpaces >> testPrsLeftOver) "   \r\n",

            "restOfLine 1" ~:  Right () ~=? testP restOfLine "    \n",
            "restOfLine 2" ~:  Right "" ~=? testP (restOfLine >> testPrsLeftOver) "    \n",
            "restOfLine 2" ~:  Right "a" ~=? testP (restOfLine >> testPrsLeftOver) "    \na",

            "blankLine 1" ~:  Right () ~=? testP blankLine "    \n",
            "blankLine 2" ~:  Right "" ~=? testP (blankLine >> testPrsLeftOver) "    \n",
            "blankLine 2" ~:  Right "a" ~=? testP (blankLine >> testPrsLeftOver) "    \na",
            "blankLine 4" ~:  True ~=? matchError (testP (anyChar >> blankLine) "a    \n"),
            "blankLine 5" ~:  Right "a" ~=? testP (anyChar >> restOfLine >> blankLine >> testPrsLeftOver) "a\n    \na",

            "printChar 1" ~:  Right 'a' ~=? testP printChar  "a",
            "printChar 2" ~:  True ~=? matchError (testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) printChar  "a"),
            "printChar 3" ~:  True ~=? matchError (testP printChar  " "),
            "printChar 4" ~:  True ~=? matchError (testP printChar  "\t"),
            "printChar 5" ~:  True ~=? matchError (testP printChar  "\n"),
            "printChar 6" ~:  True ~=? matchError (testP printChar  "\r"),
            "printChar 7" ~:  Right "cc" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (many  printChar) "cca",
            "printChar 8" ~:  Right "a" ~=? testPS (defaultParseState {psSkipCharsP = [void (char 'a')]}) (many  printChar >> testPrsLeftOver) "cca",
            "printChar 9" ~:  Right " a" ~=? testP (many  printChar >> testPrsLeftOver) "cc a",

            "emptyLines 1" ~: Right Null ~=? testP emptyLines "\n\n",
            "emptyLines 2" ~: Right Null ~=? testP emptyLines "\n \n",
            "emptyLines 3" ~: Right Null ~=? testP (anyChar >> emptyLines) "a\n",
            "emptyLines 4" ~: Right Null ~=? testP emptyLines "\n",
            "emptyLines 5" ~: Right "a" ~=? testP (anyChar >> restOfLine >> emptyLines >> testPrsLeftOver) "a\n    \na",
            "emptyLines 6" ~: Right "a" ~=? testP (anyChar >> restOfLine >> emptyLines >> testPrsLeftOver) "a\n\n\n    \na",

            "normalWord 1" ~: Right (Str "blah") ~=? testP normalWord "blah blah  ",
            "normalWord 2" ~: Right " blah  " ~=? testP (normalWord >> testPrsLeftOver) "blah blah  ",

            "mediumDash 1" ~: Right (Str "\2013") ~=? testP mediumDash "--",
            "mediumDash 2" ~: Right  " a" ~=? testP (mediumDash >> testPrsLeftOver) "-- a",

            "longDash 1" ~: Right (Str "\2014") ~=? testP longDash "---",
            "longDash 2" ~: Right  " a" ~=? testP (longDash >> testPrsLeftOver) "--- a",

            "horizRule 1" ~: Right HorizontalRule ~=? testP horizRule "----",
            "horizRule 2" ~: Right HorizontalRule ~=? testP horizRule "-----",
            "horizRule 3" ~: Right HorizontalRule ~=? testP horizRule "------",
            "horizRule 4" ~: Right HorizontalRule ~=? testP horizRule "-------\n",

            "lineBreak 1" ~: Right LineBreak ~=? testP lineBreak "\\\\",

            "emphasis 1" ~: Right (toInline $ emph $ text "blah") ~=? testP emphasis "_blah_",
            "emphasis 2" ~: Right (toInline $ emph $ text "blah bleh") ~=? testP emphasis "_blah bleh_",
            "emphasis 3" ~: Right " a" ~=? testP (emphasis >> testPrsLeftOver) "_blah bleh_ a",

            "strong 1" ~: Right (Strong [Str "blah"]) ~=? testP D.strong "*blah*",

            "superscript 1" ~: Right (Superscript [Str "blah"]) ~=? testP D.superscript "^blah^",

            "subscript 1" ~: Right (Subscript [Str "blah"]) ~=? testP D.subscript "~blah~",

            "nonTrailingSpace 1" ~: Right Space ~=? testP nonTrailingSpace " c",

            "heading 1" ~: Right (1, [Str "Henry"]) ~=? (\(Header i _ c) -> (i, c)) <$> testP heading "h1. Henry",
            "heading 2" ~: Right (1, [Str "Henry"]) ~=? (\(Header i _ c) -> (i, c)) <$> testP heading "h1.Henry",

            "paragraph 1" ~: Right (toBlock $ para . text $ "Henry the\nbig bad \nwolf")
                          ~=? testP paragraph "Henry the\nbig bad \nwolf",

            -- we drop the trailing spaces
            "paragraph 2" ~: Right (toBlock $ para . text $ "Henry the\nbig bad \nwolf")
                           ~=? testP paragraph "Henry the\nbig bad \nwolf\n\n",

            "paragraph 3" ~: True ~=? matchError (testP paragraph "* Gordon *gordon*\n* Ramsy"),

            "bulletlist 1"
                ~: Right (toBlock $ bulletList [ para (text "Gordon " <> (strong . text) "gordon") , para . text $ "Ramsy"])
                ~=? testP D.bulletList "* Gordon *gordon*\n* Ramsy",
        
            "numberedList 1"
                ~: Right (toBlock $ olist 1
                                [ para (text "Gordon " <> (strong . text) "gordon")
                                , para . text $ "Ramsy"])
                ~=? testP numberedList "# Gordon *gordon*\n# Ramsy",
                
            "numberedList 2"
                ~: Right  [toBlock $ olist 1
                                [ para (text "Gordon " <> (strong . text) "gordon")
                                , para . text $ "Ramsy"]
                          ]
                ~=? testP (many anyBlock) "# Gordon *gordon*\n# Ramsy",

            "numberedList 3"
                ~: Right [ toBlock $ olist 1
                                     [ para (text "Gordon " <> (strong . text) "gordon") ]
                         , toBlock $ header 1 (str "Ramsy")
                         ]
                ~=? testP (many anyBlock) "# Gordon *gordon*\nh1. Ramsy",

            "numberedList 4"
                ~: Right (toBlock $ olist 1
                                    [ para (str "Gordon" <> PB.space <> (strong . str $ "gordon"))
                                      <> olist 2 [para . str $ "Ramsy"]
                                    ])
                ~=? testP numberedList "# Gordon *gordon*\n## Ramsy",

            "numberedList 5"
                ~: Right (toBlock $ olist 1
                                    [ para (str "Gordon") <> olist 2 [para . str $ "Ramsy"]
                                    , para (str "Sally") <> olist 2 [para . str $ "Blah"]
                                    ])
                ~=? testP  numberedList "# Gordon\n## Ramsy\n# Sally\n## Blah",

            "imageRaw 1"
                ~: Right (ImageLink "blah.png" Nothing)
                ~=? testP imageRaw "!blah.png!",
            "imageRaw 2"
                ~: Right (ImageLink "blah.png" (Just "a=b, c=d"))
                ~=? testP imageRaw "!blah.png|a=b, c=d!",
            "imageRaw 3"
                ~: Right (ImageLink "http://blah.blah.blah.com:8090/secure/attachment/12175/ATSP3_CFI_Gathered%20Media_Lynx_Helicopter_Instrumentation_InterseatCon_CDF.png" (Just "caption"))
                ~=? testP imageRaw "!http://blah.blah.blah.com:8090/secure/attachment/12175/ATSP3_CFI_Gathered%20Media_Lynx_Helicopter_Instrumentation_InterseatCon_CDF.png|caption!",

            "imagesFromDescription 1"
                ~: [ImageLink {imgLink = "bla", imgAttrs = Nothing}]
                ~=? imagesFromDescription "gordon !bla! ramsy",
            "imagesFromDescription 2"
                ~: [ImageLink {imgLink = "bla", imgAttrs = Nothing}, ImageLink {imgLink = "ble", imgAttrs = Nothing}]
                ~=? imagesFromDescription "gordon !bla! ramsy!ble!",

            "replaceImageLinks 1"
                ~: "gordon !Sally! ramsy"
                ~=? replaceImageLinks (const ImageLink {imgLink = "Sally", imgAttrs = Nothing}) "gordon !bla! ramsy",

            "table 1"
                ~: Right ( toBlock $ tbl 2 [ para . str $ "Gordon", para . str $ "Ramsy"]
                                           [ [para . str $ "Sally", para . str $ "Storm"]
                                           , [para . str $ "Blah",  para . str $ "Bleh"]
                                           ]
                         )
                ~=? testP D.table "||Gordon||Ramsy||\n|Sally|Storm|   \n|Blah|Bleh|",
            "table 2" ~:
                    Right (toList $ header 4 . text <| "Requirements" <>
                                          tbl 2
                                          [para . text <| "Requirement",  para . text <| "Description"]
                                          [
                                          [para (str "LYNX" <> str "-" <> str "1008"),  para . text <| "DVS Control"],
                                          [para (str "LYNX" <> str "-" <> str "1009"),  para . text <| "DVS Emulation"],
                                          [para (str "LYNX" <> str "-" <> str "438"),   para . text <| "DVS Tabular View Value s"],
                                          [para (str "LYNX" <> str "-" <> str "473"),   para . text <| "DVS Instructor Events and Degraded Performance"]
                                          ] <>
                                    header 4 . text <| "Test Inputs"
                           )
                    ~=? testP prsDesc ("h4. Requirements\n"
                                    ++ "||Requirement              ||Description||\n"
                                    ++ "|LYNX-1008|DVS Control|\n"
                                    ++ "|LYNX-1009|DVS Emulation|\n"
                                    ++ "|LYNX-438|DVS Tabular View Value s|\n"
                                    ++ "|LYNX-473|DVS Instructor Events and Degraded Performance|\n"
                                    ++ "h4. Test Inputs"
                                    ),

            "heading with content" ~:
                    Right (toList $ header 4 . text <| "Overview"
                           <> para . text <| "This test verifies the Doppler Velocity Sensor simulation."
                           <> header 4 . text <| "Requirements"
                           <> table (str "") [(AlignLeft, 0), (AlignLeft, 0)]
                                          [para . text <| "Requirement",  para . text <| "Description"]
                                          [
                                          [para . text <| "LYNX-1008",  para . text <| "DVS Control"],
                                          [para . text <| "LYNX-1009",  para . text <| "DVS Emulation"],
                                          [para . text <| "LYNX-438",  para . text <| "DVS Tabular View Value s"],
                                          [para . text <| "LYNX-473",  para . text <| "DVS Instructor Events and Degraded Performance"]
                                          ]
                           <> header 4 . text <| "Test Inputs"
                           <> olist 1 [ para . text <| "ATSTest application"
                                      , para . text <| "Script : ATS\\Tests\\ATSTest\\SuperLynxTestScripts\\Sensors\\LoadSuperLynxDopplerVelocitySensor.lua"]
                           )
                    ~=? testP prsDesc ("h4. Overview\n"
                                    ++ "This test verifies the Doppler Velocity Sensor simulation.\n\n"
                                    ++ "h4. Requirements\n"
                                    ++ "||Requirement              ||Description||\n"
                                    ++ "|LYNX-1008|DVS Control|\n"
                                    ++ "|LYNX-1009|DVS Emulation|\n"
                                    ++ "|LYNX-438|DVS Tabular View Value s|\n"
                                    ++ "|LYNX-473|DVS Instructor Events and Degraded Performance|\n\n\n"
                                    ++ "h4. Test Inputs\n"
                                    ++ "# ATSTest application\n"
                                    ++ "# Script : ATS\\Tests\\ATSTest\\SuperLynxTestScripts\\Sensors\\LoadSuperLynxDopplerVelocitySensor.lua"
                                    ),
                
            "minus no strike through 1" ~:
                    Right (toBlock $ para (text "-140 to -70 dBm" <> PB.space <> (strikeout . text $ "strike through")))
                    ~=? testP paragraph "-140 to -70 dBm -strike through-",

            "minus no strike through 2" ~:
                    Right (toInline . str $ "-140 to -70 dBm -strike through-")
                    ~=? testP (deleted <|> (toInline . str <$> testPrsLeftOver)) "-140 to -70 dBm -strike through-",

            "minus no strike through 3" ~:
                    Right (toBlock $ para (text "-140 to -70 dBm" <> PB.space <> (strikeout . text $ "strike through")))
                    ~=? testP (Para . collapseInlines <$> sequence
                                                        [ punctuation <?> "a"
                                                        , normalWord <?> "b"
                                                        , nonTrailingSpace <?> "c"
                                                        , normalWord <?> "d"
                                                        , nonTrailingSpace <?> "e"
                                                        , punctuation <?> "f"
                                                        , normalWord <?> "g"
                                                        , nonTrailingSpace <?> "h"
                                                        , normalWord <?> "i"
                                                        , nonTrailingSpace <?> "j"
                                                        , deleted <?> "k"
                                                        ]) "-140 to -70 dBm -strike through-",

            "no strike through"
                ~: Right (toList $ para (text "FIRE WARNING - TEST switch is activated (LYNX-350)"))
                ~=? testP prsDesc "FIRE WARNING - TEST switch is activated (LYNX-350)",

            "no dash mono 1"
                ~: Right [Para [Str "(ie,",Space,Str "'",Span ("",[],[]) [Code ("",[],[]) "---",Space,Code ("",[],[]) "---"],Str "')"]]
                ~=? testP prsDesc "(ie, '{{--- ---}}')",
            
            "no dash mono 2"
                ~: Right [Para [Str "(ie,",Space,Str "'",Span ("",[],[]) [Code ("",[],[]) "---"],Str "')"]]
                ~=? testP prsDesc "(ie, '{{---}}')",

            "escaped" ~:
                    Right (toList $ para (text "[140 *70*" <> PB.space <> (strong . text $ "dBm-")))
                    ~=? testP prsDesc "\\[140 \\*70\\* *dBm\\-*",

            -- note file encoding must be unix ie new line characters should be \n and not \r or \r\n
            "immediate table trailing whitespace" ~:
                    Right (toList $ para . text <| "Blah bleh:"
                           <> tbl 2
                                    [para . text <| "DSC",  para . text <| "REQ"]
                                    [
                                    [para . text <| "cell on1",  para . text <| "ERSL"],
                                    [para . text <| "cell two",  para . text <| "ERSL"]
                                    ]
                           )
                    ~=? testP prsDesc (    "Blah bleh: \n"
                                        ++ "||DSC||REQ|| \n"
                                        ++ "|cell on1|ERSL| \n"
                                        ++ "|cell two|ERSL| \n"),


            "multiple images with retard dashes in front" ~:
                Right (toList $ para . str <| "Blah:" <>
                                olist 1 [ para . str <| "A"
                                            <> olist 2 [para . str <| "AA"]
                                        , para . str <| "B"
                                            <> olist 2 [para . str <| "BB"
                                                    ,para . str <| "BD"
                                                    ]
                                        ]
                                        <> para (text "I1\n--"      <> image "url1" "" (str "url1") )
                                        <> para (text "I2\n--"      <> image "url2" "" (str "url2") )
                                        <> para (text "I3\n\2013 "  <> image "url3" "" (str "url3") )
                                        
                      )
                ~=? testP prsDesc ("Blah:\n"
                                ++ "# A \n"
                                ++ "## AA\n"
                                ++ "# B \n"
                                ++ "## BB\n"
                                ++ "## BD\n"
                                ++ "\n"
                                ++ "I1\n"
                                ++ "--!url1!\n"
                                ++ "\n"
                                ++ "I2\n"
                                ++ "--!url2!\n"
                                ++ "\n"
                                ++ "I3\n"
                                ++ "-- !url3!"),

            "table with cells starting with spaces" ~:
                Right (toList $ header 4 . str <| "Requirements"
                                <> tbl 2
                                            [para . text <| "Requirement",  para . text <| "Description"]
                                            [
                                            [para . text <| "LYNX-2523",  para . text <| "CDNU WPT.14 Basic Functionality"]
                                            ]
                                        
                      )
                ~=? testP prsDesc ("h4. Requirements\n"
                                   ++ "|| Requirement || Description ||\n"
                                   ++ "| LYNX-2523| CDNU WPT.14 Basic Functionality | \n"),

            "table with inline fmt" ~:
                Right (toList $ header 4 . str <| "Requirements"
                                <> tbl 2
                                            [para . strong . text <| "Requirement",  para . emph . text <| "Description"]
                                            [
                                            [para . strikeout . text <| "LYNX-2523",  para . text <| "CDNU WPT.14 Basic Functionality"]
                                            ]
                                        
                      )
                ~=? testP prsDesc ("h4. Requirements\n"
                                   ++ "||*Requirement*|| _Description_ ||\n"
                                   ++ "|-LYNX-2523- | CDNU WPT.14 Basic Functionality | \n"),


            "multi format 1" ~:
                Right (toList $ para . emph . strong . str $ "a")
                ~=? testP prsDesc ("_*a*_"),

            "multi format 2" ~:
                Right (toList $ para $ emph (str "X_." <> smallcaps (strong . str $ "a") <> text " - B key.") )
                ~=? testP prsDesc ("_X\\_.+*a*+ - B key._"),

            "multi format 3" ~:
                Right (toList $ para $ emph (strong . str $ "LFK1.a"))
                ~=? testP prsDesc ("_*LFK1.a*_"),
                

            "dummy end" ~: True ~=? True
        ]


main :: IO Int
main = do
   -- putStrLn $ show $ testP prsDesc $ filter (\c -> c /= '\r') text
   _ <- runTestTT descriptionParserTests
   exitFailure

