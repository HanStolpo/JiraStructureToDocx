{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, ScopedTypeVariables, BangPatterns #-}
 -- GHC_STATIC_OPTION_i=../src:../testsuite
module DescriptionParser    ( parseDescription
                            , descriptionParserTests
                            , imagesFromDescription
                            , ImageLink(..)
                            , replaceImageLinks
                            ) where

import qualified Data.Char as C
import Control.Monad
import Text.Parsec
import Text.Pandoc
import Text.Pandoc.Builder hiding (space)
import qualified Text.Pandoc.Builder  as PB (space)
import Test.HUnit
import Control.Applicative ((<$>))
import Debug.Trace
import Data.List

data InlFmtType = InlStrong | InlEmph | InlSub | InlSup | InlDel | InlIns deriving (Eq, Show)
data InlFmtRes = InlFmtBussy InlFmtType | InlFmtPassed InlFmtType| InlFmtFailed InlFmtType deriving (Eq, Show)
data ParseState = PS
    {
        psLevel :: Int,
        psDepth :: Int,
        psListPrefix :: String,
        psIgnoreChars :: String,
        psInlineFmtStack :: [(InlFmtRes, MyParser Char)]
    }
fmtStackToS :: [(InlFmtRes, MyParser Char)] -> String
fmtStackToS l= "[" ++ (concatMap ((++",") . show . fst) l) ++ "]"

defaultParseState :: ParseState
defaultParseState = PS {psLevel = 0, psDepth = 0, psListPrefix = "", psIgnoreChars = "", psInlineFmtStack = []}

type MyParser = Parsec [Char] ParseState

parseDescription :: Int -> String -> [Block]
-- parseDescription l ds = Para [Str $ filter (\a -> a /= '\r') ds ]
parseDescription l ds = 
    case res of
        Left e -> [RawBlock "" (show e)]
        Right b -> b
    where
        res = runParser prsDesc  defaultParseState {psLevel = l} "Desc" ds

prsDesc :: MyParser [Block]
prsDesc = liftM (filter noNull) prsBlocks1
    where
        noNull Null = False
        noNull _ = True
    
-- trace' !msg !s !a  = trace ("\nt-" ++ msg ++ " -> " ++ (show s) ++ "\n") $! a 
-- trace'' !msg !a  = trace' msg a a
_traceStack :: String -> a -> a
_traceStack = traceStack
{-_traceStack s a = a-}

---------------------------------------------------------------------------------------------
-- Block parsing
---------------------------------------------------------------------------------------------

prsBOL :: MyParser ()
prsBOL = do
    src <-  getPosition
    let cn = sourceColumn src 
    if cn == 1 then return () else fail "expecting beginning of line"

prsBOF :: MyParser ()
prsBOF = do
    src <-  getPosition
    let cn = sourceColumn src 
    let ln = sourceLine src 
    if cn == 1 && ln == 1 then return () else fail "expecting beginning of file"


prsBlocks :: MyParser [Block]
prsBlocks = many $ choice prsBlockOrder

prsBlocks1 :: MyParser [Block]
prsBlocks1 = many1 $ choice prsBlockOrder
    
prsBlockOrder :: [MyParser Block]
prsBlockOrder = map try [prsEmptyLines, prsTable, prsHdng, prsHorizRule, prsNumberedList, prsBulletList, prsParagraph
                        ]

prsIsBlockStart :: MyParser ()
prsIsBlockStart = do
    _ <- lookAhead $ choice $ map try [prsAnyNewListPfxS
                                 ,prsHorizRuleS
                                 ,prsHdngPfxS
                                 ,prsTableHdrStartS
                                 ,prsEmptyLines >> return ""
                                 ,prsTableCellStartS
                                 ,prsTableRowsStartS
                                 ,eof >> return ""]
    return ()
                                               

prsIsNotBlockStart :: MyParser ()
prsIsNotBlockStart = prsNot prsIsBlockStart -- (prsIsBlockStart >> fail "Not expecting block start") <|> (return ())

prsIsBlockStartB :: MyParser Bool
prsIsBlockStartB = (prsIsBlockStart >> return True) <|> (return False)

prsRestOfLine :: MyParser ()
prsRestOfLine = optional (blankline <|> (skipSpaces >> prsPassIgnoreChar'))


prsEmptyLines :: MyParser Block
prsEmptyLines = many1 (   (newline >> many spaceChar >> newline)  -- empty lines or file starts with newlines
                      <|> (prsBOF >> many spaceChar >> newline)   -- file starts with one empty line
                      ) >> return Null

prsHdngPfx :: MyParser (String, Int)
prsHdngPfx = do
    prsBOL
    skipSpaces
    h <- oneOf "hH"
    d <- digit 
    p <- oneOf "."
    _ <- many1 spaceChar
    return ([h,d,p], C.digitToInt d)

prsHdngPfxS :: MyParser String 
prsHdngPfxS = (liftM fst) prsHdngPfx

prsHdng :: MyParser Block
prsHdng = do
    l <- liftM psLevel $ getState
    d <- (liftM snd) prsHdngPfx
    inls <- prsInlsTillEol >>~ prsRestOfLine
    return $ Header (d + l) nullAttr inls

prsHorizRuleS :: MyParser String
prsHorizRuleS = do
    prsBOL
    s <- string "----"
    s' <- many (char '-')
    prsRestOfLine
    return $ s ++ s'

prsHorizRule :: MyParser Block
prsHorizRule = do
    _ <- prsHorizRuleS
    return HorizontalRule

prsParagraph :: MyParser Block
prsParagraph = prsInlsTillBlock1 >>= toPara
    where 
        toPara inls = return $ Para $ dropTailSpace inls
        dropTailSpace inls = reverse . (dropWhile matchSpace) . reverse $ inls
        matchSpace Space = True
        matchSpace _     = False


prsAnyNewListPfxS :: MyParser String
prsAnyNewListPfxS = prsBOL >> skipSpaces >> many1 (oneOf "*#-") >>~ spaceChar

prsLstContent :: MyParser [Block]
prsLstContent = many (choice $ map try [prsNumberedList, prsBulletList, prsParagraph, prsTable])  

prsBulletList :: MyParser Block
prsBulletList = do
    let prsPrefix = (\pfx -> oneOfStrings [ pfx ++ "*", pfx ++ "-" ] >>~ spaceChar >>~ skipSpaces)
    let maker = BulletList  
    prsAnyListList maker prsPrefix

getListFmt :: MyParser ListNumberStyle
getListFmt = do
    ps <- getState
    let pfxDepth = psDepth ps `mod` 4
    case pfxDepth of
        0 -> return LowerAlpha
        1 -> return LowerRoman
        2 -> return UpperAlpha
        3 -> return UpperRoman
        _ -> error "should never happen"

prsNumberedList :: MyParser Block
prsNumberedList = do
    lstFmt <- getListFmt
    let prsPrefix = (\pfx -> string ( pfx ++ "#") >>~ spaceChar >>~ skipSpaces)
    let maker = OrderedList (1, lstFmt, DefaultDelim)  
    prsAnyListList maker prsPrefix

prsAnyListList :: ([[Block]] -> Block) -> (String -> MyParser String) ->  MyParser Block
prsAnyListList maker prsPrefix = do
    -- Match beginning of line
    prsBOL
    -- bail out of prsLstContent
    bStop <- prsStopList2
    if bStop then fail "" else return ()
    -- Read list starter and set list prefix if successful
    ps <- getState
    let pfx =  psListPrefix ps
    let pfxDepth = psDepth ps
    let prsPrefix' = prsPrefix pfx
    s <- prsPrefix'
    setState $ ps {psListPrefix = s, psDepth = pfxDepth  + 1}
    -- Skip any leading spaces
    skipSpaces
    -- Recursively consume lists for this level
    bss <- _prsList prsPrefix' [] 
    -- Reset list prefix
    modifyState (\ps' -> ps'{psListPrefix = pfx, psDepth = pfxDepth})
    -- Create block
    return $ maker bss 
    where
        _prsList prsPrefix'' bss' = do
            -- get the list content 
            bss'' <- prsLstContent
            c <- optionMaybe $ lookAhead $ try $ prsPrefix''
            case c of
                 -- It does not match so return current content
                 Nothing -> return $  bss' ++ [bss'']
                 -- It does match consume start and then call self to get contetn
                 Just _ -> prsPrefix'' >> (_prsList prsPrefix'' $ bss' ++ [bss''])

prsListPfxDepth :: MyParser Bool
prsListPfxDepth = do
    let prsPrefix = (many1 $ oneOfStrings [ "#", "*", "-" ]) >>~ spaceChar
    r <- optionMaybe $ lookAhead $ try $ prsPrefix
    case r of
        Nothing -> return False
        Just pfx -> do
                    let d = length pfx
                    ps <- getState
                    let pfxDepth =  psDepth ps
                    return $ d <= pfxDepth

prsBlankLines :: MyParser Bool
prsBlankLines = do
    r <- optionMaybe $ lookAhead $ try $ blanklines
    case r of
        Nothing -> return False
        Just s -> return $ length s > 1

prsStopList2 :: MyParser Bool
prsStopList2 = do
     b <- prsListPfxDepth
     b' <- prsBlankLines
     if b || b' then return True
         else do
             r <- optionMaybe $ lookAhead $ choice $ map try [prsHorizRuleS, prsHdngPfxS]
             case r of 
                 Nothing -> return False
                 Just _ -> return True

prsTableContent :: MyParser [Block]
prsTableContent = many (choice $ map try [prsParagraph])  

prsTable :: MyParser Block
prsTable = do
    hdr <- prsTableHdr
    prsRestOfLine
    rows <-  many prsTableRows
    let aln = map (\_->AlignLeft) hdr
    let colw = map (\_->0) hdr
    let cap = [Str ""]
    return $  Table cap aln colw hdr rows


prsTableHdrStartS :: MyParser String
prsTableHdrStartS = do
    prsBOL
    skipSpaces >> string "||"

prsTableHdr :: MyParser [TableCell]
prsTableHdr = do
    _ <- prsTableHdrStartS
    updateState (\ps -> ps {psIgnoreChars =  '|':(psIgnoreChars ps)}) 
    r <- endBy1 prsTableContent $ string "||"
    updateState (\ps -> let (_:is) = psIgnoreChars ps in ps {psIgnoreChars = is }) 
    return r


prsTableRowsStartS :: MyParser String
prsTableRowsStartS = do
    i <- liftM (psIgnoreChars) getState
    case i of
        '|':_ -> ((newline >> return ()) <|> prsBOL) >> skipSpaces >> string "|"
        _ -> fail "Not inside table" >> return ""

prsTableCellStartS :: MyParser String
prsTableCellStartS = do
    i <- liftM (psIgnoreChars) getState
    case i of
        '|':_ -> string "|"
        _ -> fail "Not inside table" >> return ""

prsTableRows :: MyParser [TableCell]
prsTableRows = do
    prsBOL
    _ <- skipSpaces >> string "|"
    updateState (\ps -> ps {psIgnoreChars =  '|':(psIgnoreChars ps)}) 
    cs <- many1 (prsInlsTillEol >>~ string "|")
    updateState (\ps -> let (_:is) = psIgnoreChars ps in ps {psIgnoreChars = is }) 
    optional( blankline )
    return ( map ((:[]) . Para) $ cs )
    
    
---------------------------------------------------------------------------------------------
-- Inline parsing
---------------------------------------------------------------------------------------------
addLineBreakBeforeAfterImages :: [Inline] -> MyParser [Inline]
addLineBreakBeforeAfterImages ls = return $ foldr f [] $ ls
    where
        f img@(Image _ _) ls' = LineBreak : img : LineBreak : ls'
        f l ls' = l : ls'

prsInlsTillEol :: MyParser [Inline]
prsInlsTillEol =  (many  (prsFailIgnoreChar >> choice prsInlineOrder) ) >>= addLineBreakBeforeAfterImages
{-prsInlsTillEol1 = (many1  (prsFailIgnoreChar >> choice prsInlineOrder) ) >>= addLineBreakBeforeAfterImages-}

prsInlineOrder :: [MyParser Inline]
prsInlineOrder = map try [  prsLongDash, prsMediumDash, prsEmph, prsStrong, prsCitation, prsDeleted, prsInserted, prsSuperScript,
                            prsSubScript, prsMonoSpaced, prsImage, prsLineBreak,  prsWord, prsSpaceNotAtEol
                         ]

prsInlineOrder2 :: [MyParser Inline]
prsInlineOrder2 = map try [ prsLongDash, prsMediumDash, prsEmph, prsStrong, prsCitation, prsDeleted, prsInserted, prsSuperScript,
                            prsSubScript, prsMonoSpaced, prsImage, prsLineBreak,  prsWord, prsSpaceTillBlock, 
                            prsInlSpace
                          ]

prsInlsTillBlock :: MyParser [Inline]
prsInlsTillBlock = (liftM reverse $ prsInlsTillBlock1' []) >>= addLineBreakBeforeAfterImages
    where 
        prsInlsTillBlock1' ils = do
            stop <-  prsIsBlockStartB
            st <- getState 
            let fmtEnd = case psInlineFmtStack st of
                            []         -> False
                            ((InlFmtBussy _,_):_)  ->  False
                            _ -> True
            if stop || fmtEnd
                then do 
                    return ils
                else do 
                    prsFailIgnoreChar
                    !il <- choice prsInlineOrder2
                    prsInlsTillBlock1' (il:ils)

prsInlsTillBlock1 :: MyParser [Inline]
prsInlsTillBlock1 = prsInlsTillBlock >>= (\ils -> if null ils then fail "Expected at least one Inline" else return ils)
    
    
prsAnyInFmt :: MyParser Inline
prsAnyInFmt = choice $ map try  [prsEmph, prsStrong, prsCitation, prsDeleted, prsInserted
                                ,prsSuperScript, prsSubScript, prsMonoSpaced, prsImage ]

prsEnclosed :: MyParser start -> MyParser end -> MyParser content -> MyParser [content]
prsEnclosed s e = enclosed (s >> notFollowedBy space) (lookAhead (try nonspaceChar) >> e)

prsEnclosed' :: MyParser start_end ->  MyParser content -> MyParser [content]
prsEnclosed' s  = prsEnclosed s s

prsEscChar :: MyParser Char
prsEscChar = prsFailIgnoreChar >> (try (char '\\' >> oneOf "*_^~+-#![]|?{") <|> anyChar)

prsAnyChar :: MyParser Char
prsAnyChar = do
    st <-  getState
    case psInlineFmtStack st of
        [] -> prsEscChar
        ((InlFmtBussy _,f):_) -> f
        _ -> fail ""

{-prsAnyChar :: MyParser Char-}
{-prsAnyChar = do-}
    {-st <-  getState-}
    {-case psInlineFmtStack st of-}
        {-[] -> noneOf $ psIgnoreChars st-}
        {-((InlFmtBussy _,f):_) -> do-}
            {-c <- f-}
            {-if not . elem c . psIgnoreChars $ st then return c else fail ""-}
        {-_ -> fail ""-}
    
prsFailIgnoreChar' :: MyParser Char
prsFailIgnoreChar' = do
    ignr <- liftM psIgnoreChars getState
    lookAhead $ try $  satisfy (\c -> not $ elem c ignr)

prsFailIgnoreChar :: MyParser ()
prsFailIgnoreChar = do
    _ <- prsFailIgnoreChar'
    return ()

prsPassIgnoreChar' :: MyParser Char
prsPassIgnoreChar' = do
    ignr <- liftM psIgnoreChars getState
    lookAhead $ try $  satisfy (\c -> elem c ignr)

-- spaces
prsInlSpaceC :: MyParser Char
prsInlSpaceC = do
    st <- getState
    case psInlineFmtStack st of
        [] -> satisfy chk
        ((InlFmtBussy _,f):_) -> (do c <- f; if chk c then return c else fail "")
        _ -> fail ""
    where 
        chk c = C.isSpace c && (not . elem c $ "\n\r")

prsInlSpaceS :: MyParser String
prsInlSpaceS = do
    st <- getState
    case psInlineFmtStack st of
        [] -> many1 . try $ (satisfy chk)
        ((InlFmtBussy _,f):_) ->  (many1 . try $ do
                        c <- f
                        if chk c then return c else fail "")
        _ -> fail "prsInlSpace failed" ""
    where 
        chk c = C.isSpace c && (not . elem c $ "\n\r")
                    
prsInlSpace :: MyParser Inline
prsInlSpace =  prsInlSpaceS >> (return Space) 

prsSpaceNotAtEol :: MyParser Inline
prsSpaceNotAtEol = prsInlSpace >>~ notFollowedBy' (eof <|> (newline >> return ()))

prsSpaceTillBlock :: MyParser Inline
prsSpaceTillBlock = many1 ( prsIsNotBlockStart >> (prsInlSpaceC <|> newline) ) >> return Space

prsNot :: MyParser a -> MyParser ()
prsNot p = do
    matched <- lookAhead . try $ (p >> return True) <|> (return False)
    if matched then fail "" else return ()

prsNotEol :: MyParser Char
prsNotEol = prsAnyChar >>~ notFollowedBy newline

prsInlFmt :: InlFmtType -> ([Inline]->Inline) -> String -> MyParser Inline
prsInlFmt t fmt s = do
    st <- getState
    let f (InlFmtBussy  t', _) = t' == t
        f (InlFmtFailed t', _) = t' == t
        f (InlFmtPassed t', _) = t' == t
    case find f . psInlineFmtStack $ st of
        Nothing -> return ()
        Just _ ->  fail ""
    prsNot $ char '\\'
    string s >> notFollowedBy space
    setState $ st {psInlineFmtStack = ((InlFmtBussy t,  try ep <|> try bp <|> prsEscChar) : (psInlineFmtStack st))}
    inls <- try prsInlsTillBlock1
    stR <- getState
    modifyState (\st' -> st'{psInlineFmtStack = psInlineFmtStack st})
    case psInlineFmtStack stR of
        [] -> error "prsInlFmt - stact mis match"
        ((InlFmtBussy _,_):_) -> fail  "prsInlFmt - stact mis match bussy"
        ((InlFmtFailed _,_):_) -> fail  ""
        ((InlFmtPassed _,_):_) -> return $ fmt inls
    where
        ep :: MyParser Char
        ep = do
           lookAhead . try . prsNot $ space
           c <- prsEscChar >>~ string s
           modifyState (\st -> case psInlineFmtStack  st of
                                [] -> error "prsInlFmt - stack missmatch"
                                ((InlFmtBussy tp,f):t) -> st {psInlineFmtStack =  ((InlFmtPassed tp, f):t)}
                                _ -> st)
           return c
        bp :: MyParser Char
        bp = do
           lookAhead . try $ string s >> notFollowedBy space
           c <- prsEscChar 
           modifyState (\st -> case psInlineFmtStack st of
                                [] -> error "prsInlFmt - stack missmatch"
                                ((InlFmtBussy tp,f):t) -> st {psInlineFmtStack =  ((InlFmtFailed tp, f):t)}
                                _ -> st)
           return c


-- _emphasis_
prsEmph :: MyParser Inline
prsEmph = prsInlFmt InlEmph Emph "_"

-- *strong*
prsStrong :: MyParser Inline
prsStrong = prsInlFmt InlStrong Strong "*"

-- ??citation??
prsCitation :: MyParser Inline
prsCitation = do
    s <- prsEnclosed' (string "??") prsNotEol
    return $ Cite [Citation s [] [] NormalCitation 0 0] [Str s]

-- -deleted-
prsDeleted :: MyParser Inline
prsDeleted = prsInlFmt InlDel Strikeout "-"

-- +inserted+
prsInserted :: MyParser Inline
prsInserted = prsInlFmt InlIns SmallCaps "+"

-- ^superscript^
prsSuperScript :: MyParser Inline
prsSuperScript = prsInlFmt InlSup Superscript "^"

-- ~subscript~
prsSubScript :: MyParser Inline
prsSubScript = prsInlFmt InlSub Subscript "~"

-- {{monospaced}}
prsMonoSpaced :: MyParser Inline
prsMonoSpaced = do
    s <- prsEnclosed' (string "{{") prsNotEol
    return $ Code nullAttr s

-- \\ line break
prsLineBreak :: MyParser Inline
prsLineBreak = string "\\\\" >> notFollowedBy' (string "\\") >> (return $ LineBreak)


-- -- medium dash en dash U+2013
prsMediumDash :: MyParser Inline
prsMediumDash = do
    _ <- string "--"
    return $ Str "\2013"

-- --- long dash em dash U+2014
prsLongDash :: MyParser Inline
prsLongDash = do
    _ <- string "---"
    return $ Str "\2014"    


-- any normal word
prsWord :: MyParser Inline
prsWord = do
    st <- getState
    setState $ st {psIgnoreChars = psIgnoreChars st ++ " \t\n\r"}
    s <- many1 (try p)
    modifyState (\st' -> st' {psIgnoreChars = psIgnoreChars st})
    return $ Str s
    where 
        p = do
            prsNot (lookAhead . try $ prsAnyInFmt)
            prsAnyChar

prsEndl :: MyParser ()
prsEndl = do
    _ <- manyTill space $ lookAhead $ try newline
    _ <- newline
    return ()

prsImage :: MyParser Inline
prsImage = do
    ImageLink imgL _ <- prsImageRaw
    return $ Image [Str imgL] (imgL, "")

-- parse an image link
data ImageLink = ImageLink 
    {
        imgLink :: String,
        imgAttrs :: Maybe String
    }deriving (Show, Eq)

prsImageRaw :: MyParser ImageLink
prsImageRaw = do
    s <- prsEnclosed' (string "!") prsNotEol
    (l,a) <- parseFromString _prsLinkAttr s
    return $ ImageLink l a
    where
        _prsLinkAttr = do
            l <- many $ noneOf "|"
            a <- optionMaybe $ (char '|' >> (many anyChar))
            return (l,a)

prsImagesRaw :: MyParser [ImageLink]
prsImagesRaw = do
    liftM concat $ many $ (liftM (:[]) $ try prsImageRaw) <|> (anyChar >> return [])

imagesFromDescription :: String -> [ImageLink]
imagesFromDescription d = 
    case r of
        Left e -> error $ show e
        Right a -> a
    where
        r = runParser prsImagesRaw defaultParseState "" d 

replaceImageLinks :: (ImageLink -> ImageLink) -> String -> String
replaceImageLinks _ [] = []
replaceImageLinks replacer d =
    case r of
        Left e -> error $ show e
        Right a -> foldl collapse "" a
    where
        r = runParser p defaultParseState "" d 
        -- Parse image links and prefixes
        p :: MyParser [Either ImageLink String]
        p = many pe
        pe :: MyParser (Either ImageLink String)
        pe = pImage <|> pText 
            where
                pImage = liftM Left $ try $ prsImageRaw 
                pText =  liftM Right $ many1 pTextChar
                pTextChar = (lookAhead $ try $ pNotImage) >> anyChar
                pNotImage = prsNot prsImageRaw 
        -- Transform parse results and turn into string again
        collapse :: String -> Either ImageLink String -> String 
        collapse sAcc (Left iml ) = let ImageLink m as = replacer iml in sAcc ++ "!" ++ m ++ (maybe "" (\a-> "|" ++ a) as) ++ "!"
        collapse sAcc (Right s) = sAcc ++ s

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

            "prsBOF" ~:  True ~=? (matchError $ testP (anyChar >> prsBOF) "a\n"),
            "prsBOF 2" ~:  Right () ~=? testP (prsBOF) "a\n",

            "prsEmptyLines" ~: Right Null ~=? testP prsEmptyLines "\n\n",
            "prsEmptyLines 2" ~: Right Null ~=? testP prsEmptyLines "\n \n",
            "prsEmptyLines 3" ~:  True ~=? (matchError $ testP (anyChar >> prsEmptyLines) "a\n"),

            "prsIsBlockStartB" ~: Right True ~=? testP prsIsBlockStartB "\n\n",
            "prsIsBlockStartB 2" ~: Right True ~=? testP prsIsBlockStartB "\n \n",
            "prsIsBlockStartB 3" ~: Right False ~=? testP (anyChar >> prsIsBlockStartB) "a\n",
            
            "prsInlsTillBlock1 0" ~: Right (toList $ text "some text") ~=? testP prsInlsTillBlock1 "some text",
            "prsInlsTillBlock1" ~: Right (toList $ text "some text " <> emph "emp") ~=? testP prsInlsTillBlock1 "some text _emp_",
            "prsInlsTillBlock1 2" ~: Right (toList $ text "some text " <> emph "emp") ~=? testP prsInlsTillBlock1 "some text\n_emp_",
            "prsInlsTillBlock1 3" ~: Right (toList $ text "some text") ~=? testP prsInlsTillBlock1 "some text\n\n_emp_",
            "prsInlsTillBlock1 4" ~: Right ([Str "Henry", Space, Str "the", Space, Str "big", Space, Str "bad", Space, Str "wolf"])
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

            "prsInlSpace" ~: Right (Space) ~=? testP prsInlSpace " ",
            
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
                
            "prsNumberedList 2" ~: Right  [(OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ], [ Para [Str "Ramsy"] ] ])]
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

            "replaceImageLinks 1" ~: "gordon !Sally! ramsy" ~=? replaceImageLinks (\_->ImageLink {imgLink = "Sally", imgAttrs = Nothing}) "gordon !bla! ramsy",

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
                    ~=? testP prsDesc ("-140 to -70 dBm -strike through-"),

            "escaped" ~:
                    Right (toList $ para (text "[140 *70*" <> PB.space <> (strong . text $ "dBm-")))
                    ~=? testP prsDesc ("\\[140 \\*70\\* *dBm\\-*"),

            "dummy end" ~: True ~=? True
        ]


------------------------------------------------------------------------
-- From Pandoc.Parsing
------------------------------------------------------------------------

-- | Like @manyTill@, but reads at least one item.
many1Till :: Parsec [tok] st a
          -> Parsec [tok] st end
          -> Parsec [tok] st [a]
many1Till p end = do
         first <- p
         rest <- manyTill p end
         return (first:rest)
-- | Parses a space or tab.
spaceChar :: Parsec [Char] st Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Parsec [Char] st Char
nonspaceChar = satisfy $ \x -> x /= '\t' && x /= '\n' && x /= ' ' && x /= '\r'

-- | Skips zero or more spaces or tabs.
skipSpaces :: Parsec [Char] st ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Parsec [Char] st Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: Parsec [Char] st [Char]
blanklines = many1 blankline

-- | Parses material enclosed between start and end parsers.
enclosed :: Parsec [Char] st t   -- ^ start parser
         -> Parsec [Char] st end  -- ^ end parser
         -> Parsec [Char] st a    -- ^ content parser (to be used repeatedly)
         -> Parsec [Char] st [a]
enclosed start end parser = try $
  start >> notFollowedBy space >> many1Till parser end

{--- | Parse string, case insensitive.-}
{-stringAnyCase :: [Char] -> Parsec [Char] st String-}
{-stringAnyCase [] = string ""-}
{-stringAnyCase (x:xs) = do-}
  {-firstChar <- char (C.toUpper x) <|> char (C.toLower x)-}
  {-rest <- stringAnyCase xs-}
  {-return (firstChar:rest)-}

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: Parsec [tok] st a -> [tok] -> Parsec [tok] st a
parseFromString parser _str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput _str
  result <- parser
  setInput oldInput
  setPosition oldPos
  return result

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x


-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: Show b => Parsec [a] st b -> Parsec [a] st ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())


oneOfStrings' :: (Char -> Char -> Bool) -> [String] -> Parsec [Char] st String
oneOfStrings' _ []   = fail "no strings"
oneOfStrings' matches strs = try $ do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x `matches` c]
  case strs' of
       []  -> fail "not found"
       _   -> (c:) `fmap` oneOfStrings' matches strs'
               <|> if "" `elem` strs'
                      then return [c]
                      else fail "not found"

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: [String] -> Parsec [Char] st String
oneOfStrings = oneOfStrings' (==)
