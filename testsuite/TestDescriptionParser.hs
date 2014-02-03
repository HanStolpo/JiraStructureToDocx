{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
 -- GHC_STATIC_OPTION_i=../src:../testsuite


module Main where

import DescriptionParserInner
import Test.HUnit
import System.Exit (exitFailure)
import Text.Parsec
import Text.Pandoc
import Text.Pandoc.Builder hiding (space)
import qualified Text.Pandoc.Builder  as PB (space)
import Control.Applicative ((<$>))
import Debug.Trace

----------------------------------------------------------------------------------------
-- HUnit parser tests
----------------------------------------------------------------------------------------
testPS ::  ParseState -> MyParser b -> String -> Either String b
testPS s p i = 
    case r of 
        Left e -> Left $ show e ++ " for input : " ++ i
        Right a -> Right a
    where
        r = runParser p s "" i

testP :: MyParser b -> String -> Either String b
testP = testPS defaultParseState

testPrsLeftOver :: MyParser String
testPrsLeftOver = many anyChar


matchError ::  Show a => Either t a -> Bool
matchError (Right a) = trace (show a) False
matchError (Left _) = True

(<|) :: (b -> a) -> b -> a
infixl 8 <|
f <| a = f a

descriptionParserTests ::  Test
descriptionParserTests = test
        [   "newline" ~: Right '\n'  ~=? testP newline "\n",

            "prsBOF" ~:  True ~=? matchError (testP (anyChar >> prsBOF) "a\n"),
            "prsBOF 2" ~:  Right () ~=? testP prsBOF "a\n",

            "prsEmptyLines" ~: Right Null ~=? testP prsEmptyLines "\n\n",
            "prsEmptyLines 2" ~: Right Null ~=? testP prsEmptyLines "\n \n",
            "prsEmptyLines 3" ~:  True ~=? matchError (testP (anyChar >> prsEmptyLines) "a\n"),

            "prsIsBlockStartB" ~: Right True ~=? testP prsIsBlockStartB "\n\n",
            "prsIsBlockStartB 2" ~: Right True ~=? testP prsIsBlockStartB "\n \n",
            "prsIsBlockStartB 3" ~: Right False ~=? testP (anyChar >> prsIsBlockStartB) "a\n",
            
            "prsInlsTillBlock1 0" ~: Right (toList $ text "some text") ~=? testP prsInlsTillBlock1 "some text",
            "prsInlsTillBlock1" ~: Right (toList $ text "some text " <> emph "emp") ~=? testP prsInlsTillBlock1 "some text _emp_",
            "prsInlsTillBlock1 2" ~: Right (toList $ text "some text " <> emph "emp") ~=? testP prsInlsTillBlock1 "some text\n_emp_",
            "prsInlsTillBlock1 3" ~: Right (toList $ text "some text") ~=? testP prsInlsTillBlock1 "some text\n\n_emp_",
            "prsInlsTillBlock1 4" ~: Right [Str "Henry", Space, Str "the", Space, Str "big", Space, Str "bad", Space, Str "wolf"]
                          ~=? testP prsInlsTillBlock1 "Henry the\nbig bad \nwolf",
            "prsEndl" ~: Right ()  ~=? testP prsEndl "   \n \n",
            "prsSpaceNotAtEol" ~: Right Space ~=? testP prsSpaceNotAtEol " i",
            "(prsSpaceNotAtEol >> testPrsLeftOver)" ~: Right "i" ~=? testP (prsSpaceNotAtEol >> testPrsLeftOver) " i",
            "prsWord 1" ~: Right (Str "blah") ~=? testP prsWord "blah blah  ",
            "prsWord 2" ~: Right " blah  " ~=? testP (prsWord >> testPrsLeftOver) "blah blah  ",

            "prsMediumDash" ~: Right (Str "\2013") ~=? testP prsMediumDash "--",
            "prsLongDash" ~: Right (Str "\2014") ~=? testP prsLongDash "---",

            "prsHorizRule 1" ~: Right HorizontalRule ~=? testP prsHorizRule "----",
            "prsHorizRule 2" ~: Right HorizontalRule ~=? testP prsHorizRule "-----",
            "prsHorizRule 3" ~: Right HorizontalRule ~=? testP prsHorizRule "------",
            "prsHorizRule 4" ~: Right HorizontalRule ~=? testP prsHorizRule "-------\n",

            "prsLineBreak 1" ~: Right LineBreak ~=? testP prsLineBreak "\\\\",

            "prsEmph 1" ~: Right (Emph [Str "blah"]) ~=? testP prsEmph "_blah_",

            "prsStrong" ~: Right (Strong [Str "blah"]) ~=? testP prsStrong "*blah*",

            "Superscript" ~: Right (Superscript [Str "blah"]) ~=? testP prsSuperScript "^blah^",

            "prsInlSpace" ~: Right Space ~=? testP prsInlSpace " ",
            
            "prsSubScript" ~: Right (Subscript [Str "blah"]) ~=? testP prsSubScript "~blah~",

            "prsInlsTillEol 1" ~: Right [ Subscript[Str "blah"], Str "bleh"] ~=? testP prsInlsTillEol "~blah~bleh",
            "prsInlsTillEol 2" ~: Right [ Subscript[Str "blah"], Space, Str "bleh"] ~=? testP prsInlsTillEol "~blah~   bleh ",
            "prsInlsTillEol 3" ~: Right [ Subscript[Str "blah"], Space, Str "bleh"] ~=? testP prsInlsTillEol "~blah~   bleh \nJhonny",
            "prsInlsTillEol 4" ~: Right " \nJhonny" ~=? testP (prsInlsTillEol >> testPrsLeftOver) "~blah~   bleh \nJhonny",

            "prsHdng" ~: Right (1, [Str "Henry"]) ~=? (\(Header i _ c) -> (i, c)) <$> testP prsHdng "h1. Henry",

            "prsParagraph" ~: Right (Para [Str "Henry", Space, Str "the", Space, Str "big", Space, Str "bad", Space, Str "wolf"])
                          ~=? testP prsParagraph "Henry the\nbig bad \nwolf",

            "prsParagraph" ~: Right (Para [Str "Henry", Space, Str "the", Space, Str "big", Space, Str "bad", Space, Str "wolf"])
                           ~=? testP prsParagraph "Henry the\nbig bad \nwolf\n\n",

            "prsBulletList" ~: Right (BulletList [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ], [ Para [Str "Ramsy"] ]  ])
                            ~=? testP prsBulletList "* Gordon *gordon*\n* Ramsy",
        
            "prsNumberedList" ~: Right (OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ], [ Para [Str "Ramsy"] ] ])
                            ~=? testP prsNumberedList "# Gordon *gordon*\n# Ramsy",
                
            "prsNumberedList 2" ~: Right  [OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ], [ Para [Str "Ramsy"] ] ]]
                             ~=? testP prsBlocks "# Gordon *gordon*\n# Ramsy",

            "prsNumberedList 3" ~: Right [ OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ]  ]
                                         , Header 1 nullAttr [ Str "Ramsy" ]
                                         ]
                             ~=? testP prsBlocks "# Gordon *gordon*\nh1. Ramsy",

            "prsNumberedList 4" ~: Right (OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]], OrderedList (1, LowerRoman, DefaultDelim)[ [ Para [Str "Ramsy"] ] ] ]  ])
                            ~=? testP prsNumberedList "# Gordon *gordon*\n## Ramsy",

            "prsNumberedList 5" ~: Right (OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon"] , OrderedList (1, LowerRoman, DefaultDelim)[ [ Para [Str "Ramsy"] ] ] ]
                                                                                      ,  [ Para [Str "Sally"] ,  OrderedList (1, LowerRoman, DefaultDelim)[ [ Para [Str "Blah"] ] ] ]
                                                                                     ])
                            ~=? testP prsNumberedList "# Gordon\n## Ramsy\n# Sally\n## Blah",

            "prsStopList2 1" ~: Right False ~=? testP prsStopList2 "# blah my end",
            "prsStopList2 2" ~: Right True ~=? testP prsStopList2 "h1. blah my end",
            "prsStopList2 4" ~: Right True ~=? testP prsStopList2 "--------------------",
            "prsStopList2 5" ~: Right True ~=? testPS defaultParseState {psDepth = 1, psListPrefix = "#"} prsStopList2 "# blah my end",

            "prsTable" ~: Right ( Table [Str ""] [AlignLeft,AlignLeft] [0.0,0.0] [ [ Para [Str "Gordon"]], [ Para [Str "Ramsy"]] ]
                                               [ [[ Para [Str "Sally"]], [Para [Str "Storm"]] ]
                                               , [[ Para [Str "Blah"]], [Para [Str "Bleh"]] ]
                                               ])
                            ~=? testP prsTable "||Gordon||Ramsy||\n|Sally|Storm|\n|Blah|Bleh|",

            "prsImageRaw 1" ~: Right (ImageLink "blah.png" Nothing) ~=? testP prsImageRaw "!blah.png!",

            "prsImageRaw 2" ~: Right (ImageLink "blah.png" (Just "a=b, c=d")) ~=? testP prsImageRaw "!blah.png|a=b, c=d!",

            "prsImageRaw 3" ~: Right (ImageLink "http://srv1.za.5dt.com:8090/secure/attachment/12175/ATSP3_CFI_Gathered%20Media_Lynx_Helicopter_Instrumentation_InterseatCon_CDF.png" (Just "caption")) ~=? testP prsImageRaw "!http://srv1.za.5dt.com:8090/secure/attachment/12175/ATSP3_CFI_Gathered%20Media_Lynx_Helicopter_Instrumentation_InterseatCon_CDF.png|caption!",

            "imagesFromDescription 1" ~: [ImageLink {imgLink = "bla", imgAttrs = Nothing}] ~=? imagesFromDescription "gordon !bla! ramsy",
            "imagesFromDescription 2" ~: [ImageLink {imgLink = "bla", imgAttrs = Nothing}, ImageLink {imgLink = "ble", imgAttrs = Nothing}] ~=? imagesFromDescription "gordon !bla! ramsy!ble!",

            "replaceImageLinks 1" ~: "gordon !Sally! ramsy" ~=? replaceImageLinks (const ImageLink {imgLink = "Sally", imgAttrs = Nothing}) "gordon !bla! ramsy",

            "prsTbl" ~:
                    Right (toList $ header 4 . text <| "Requirements" <>
                                        table (str "") [(AlignLeft, 0), (AlignLeft, 0)]
                                          [para . text <| "Requirement",  para . text <| "Description"]
                                          [
                                          [para . text <| "LYNX-1008",  para . text <| "DVS Control"],
                                          [para . text <| "LYNX-1009",  para . text <| "DVS Emulation"],
                                          [para . text <| "LYNX-438",  para . text <| "DVS Tabular View Value s"],
                                          [para . text <| "LYNX-473",  para . text <| "DVS Instructor Events and Degraded Performance"]
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
                           <> orderedListWith (1, LowerAlpha, DefaultDelim) [para . text <| "ATSTest application"
                                                                            ,para . text <| "Script : ATS\\Tests\\ATSTest\\SuperLynxTestScripts\\Sensors\\LoadSuperLynxDopplerVelocitySensor.lua"]
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
                
            "minus no strike through" ~:
                    Right (toList $ para (text "-140 to -70 dBm" <> PB.space <> (strikeout . text $ "strike through")))
                    ~=? testP prsDesc "-140 to -70 dBm -strike through-",

            "escaped" ~:
                    Right (toList $ para (text "[140 *70*" <> PB.space <> (strong . text $ "dBm-")))
                    ~=? testP prsDesc "\\[140 \\*70\\* *dBm\\-*",

            "dummy end" ~: True ~=? True
        ]


main :: IO Int
main = do
   -- putStrLn $ show $ testP prsDesc $ filter (\c -> c /= '\r') text
   _ <- runTestTT descriptionParserTests
   exitFailure

