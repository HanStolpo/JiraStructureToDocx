{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, TupleSections, BangPatterns #-}
 -- GHC_STATIC_OPTION_i=../src:../testsuite
module DescriptionParserInner where

import qualified Data.Char as C
import Control.Monad
import Control.Applicative ((<$>), (<$), (<*>))
import Control.Arrow
import Data.Maybe
import Text.Parsec
import Text.Parsec.Pos
import Text.Pandoc
import Data.List

---------------------------------------------------------------------------------------------
-- debug helpers
---------------------------------------------------------------------------------------------
import Debug.Trace

traceMsgS :: Show a => String -> a -> a
traceMsgS msg a = trace ("\n" ++ msg ++ "{" ++ show a ++ "}") a

traceMS :: Show a => String -> a -> MyParser a
traceMS msg a = traceM' (msg ++ "{" ++ show a ++ "}") >> return a

traceMS' :: Show a => String -> a -> MyParser ()
traceMS' msg = void . traceMS msg

traceM :: String -> a -> MyParser a
traceM msg a = traceM' msg >> return a

traceM' :: String -> MyParser ()
traceM' msg = getState >>= (\s -> return $! trace ("\n" ++ msg) s) >>= setState

---------------------------------------------------------------------------------------------
-- Enclosed parsing
---------------------------------------------------------------------------------------------
data ParseState = PS
    {
        psLevel         :: Int,
        psSkipChars     :: String,
        psEncStack      :: [EncType],
        psInlStack      :: [MyParser Inline],
        psBlockInlStack :: [Inline]
    }

defaultParseState :: ParseState
defaultParseState = PS {psLevel = 0, psSkipChars = "", psEncStack = [], psInlStack = [inlineEol], psBlockInlStack = []}

type MyParser = Parsec String ParseState

---------------------------------------------------------------------------------------------
-- Enclosed parsing
---------------------------------------------------------------------------------------------

data EncType = EncStrong | EncEmph | EncSub | EncSup | EncDel | EncIns | EncTable | EncTableCell | EncTableRow | EncList String | EncPara | EncCite | EncMono | EncImage deriving (Show, Eq, Ord)

isInEnc :: EncType -> MyParser Bool
isInEnc t =  isJust . find (t==) . psEncStack <$> getState

pushEnc :: EncType -> MyParser ()
pushEnc e = modifyState $ \st -> st {psEncStack = e : psEncStack st}

popEnc :: MyParser ()
popEnc = modifyState $ \st -> st {psEncStack = pop . psEncStack $ st}
    where
        pop [] = []
        pop (_:es) = es

nonReEntrant :: EncType -> MyParser a -> MyParser a
nonReEntrant t p = (not <$> isInEnc t) >>=  guard >> pushEnc t >> p >>= (<$ popEnc)

getInline :: MyParser (MyParser Inline)
getInline =  head . psInlStack <$> getState

pushInline :: MyParser Inline -> MyParser ()
pushInline i = modifyState $ \st -> st {psInlStack = i : psInlStack st}

popInline :: MyParser ()
popInline = modifyState $ \st -> st {psInlStack = pop . psInlStack $ st}
    where
        pop [] = error "popInline - can never be here"
        pop is@(_:[]) = is
        pop (_:is) = is

withInline :: MyParser Inline -> MyParser a -> MyParser a
withInline i p = pushInline i >> p >>= (<$ popInline)

manyEnd  :: String          -- Skip chars
         -> MyParser ()     -- end parser
         -> MyParser a      -- inner parser
         -> MyParser [a]    
manyEnd = manyEnd'' (const mzero) return

manyEnd'  :: String         -- Skip chars
         -> MyParser ()     -- end parser
         -> MyParser a      -- inner parser
         -> MyParser [a]
manyEnd'= manyEnd'' return return

manyEnd1 :: String          -- Skip chars
         -> MyParser ()     -- end parser
         -> MyParser a      -- inner parser
         -> MyParser [a]
manyEnd1 = manyEnd'' (const mzero) (\as -> if null as then mzero else return as)

manyEnd1':: String          -- Skip chars
         -> MyParser ()     -- end parser
         -> MyParser a      -- inner parser
         -> MyParser [a]
manyEnd1'= manyEnd'' return (\as -> if null as then mzero else return as)

manyEnd'' :: ([a] -> MyParser [a]) -- on inner failed
          -> ([a] -> MyParser [a]) -- post check
          -> String                -- Skip chars
          -> MyParser ()           -- end parser
          -> MyParser a            -- inner parser
          -> MyParser [a]
manyEnd'' f p s e i = do
    sWas <- psSkipChars <$> getState
    modifyState $ \st -> st {psSkipChars = s ++ sWas}
    let
        scan as = try (end as) <|> cont as <|> f as
        end as = e >> return as
        cont as = (:as) <$> i >>= scan 
    as <- scan []
    modifyState $ \st -> st {psSkipChars = sWas}
    p . reverse $ as

manyEndHist  :: String          -- Skip chars
             -> Int             -- samples to look back for look back parser
             -> MyParser ()     -- lookBack parser
             -> MyParser ()     -- Pass end parser
             -> MyParser String -- Fail end parser
             -> MyParser a      -- inner parser
             -> MyParser [a]
manyEndHist s h l p f i = do
    sWas <- psSkipChars <$> getState
    modifyState $ \st -> st {psSkipChars = s ++ sWas}
    let
        scan as a = let as' = a:as in try (endPass as') <|> endCont as'
        endPass as = l >> p >> return as
        endCont as = count h anyToken >> lookBack h i' (scan as)
        guardFail = (((False,) <$> (lookAhead . try) f) <|> return (True,"")) >>= \(c,e) -> if c then return () else unexpected e
        i'  = guardFail >> try i
    as <- lookBack h i' (scan [])
    modifyState $ \st -> st {psSkipChars = sWas}
    return (reverse as)

---------------------------------------------------------------------------------------------
-- common parsing
---------------------------------------------------------------------------------------------

lookBack :: Int               -- The legth of the history
         -> MyParser a        -- Parser applied normally
         -> (a -> MyParser b) -- Parser applied N characters back from where previous one stopped
         -> MyParser b        -- The result of both parsers
lookBack h p l = do
    (src, pos) <- (,) <$> getInput <*> getPosition
    a <- p
    (srcO, posO) <- (,) <$> getInput <*> getPosition
    let
        -- mv [] _ _ _                   = error $ "lookBack - input exhausted before catching up\n:" ++ show src ++ "\n" ++ show pos ++ "\n" ++ show srcO ++ "\n" ++ show posO
        mv [] ph _ _                   = ([],ph, Just $ "lookBack - input exhausted before catching up\n:" ++ show src ++ "\n" ++ show pos ++ "\n" ++ show srcO ++ "\n" ++ show posO)
        mv sh ph pc _      | ph == pc  = (sh, ph, Nothing)
        mv (c:sh) ph pc h' | h' <= 0   = mv sh (updatePosChar ph c) pc 0
        mv sh@(c:_) ph pc h'           = mv sh (updatePosChar ph c) pc (h' - 1)
        (src', pos', err) = mv src pos posO h
    case err of
        Just msg -> unexpected msg
        Nothing -> do
                    setInput src' >> setPosition pos'
                    b <- l a
                    pos'' <- getPosition
                    when (pos'' < posO) (setInput srcO >> setPosition posO)
                    return b

lookBack' :: Int               -- The legth of the history
          -> MyParser a        -- Parser applied normally
          -> MyParser b        -- Parser applied N characters back from where previous one stopped
          -> MyParser (a,b)    -- The result of both parsers
lookBack' h p l = lookBack h p (\a -> (,) a <$> l)

eol :: MyParser ()
eol = (char '\r' >> optional (char '\n')) <|> (char '\n' >> optional (char '\r'))

bol :: MyParser ()
bol = (==1) . sourceColumn <$> getPosition >>= guard

bof :: MyParser ()
bof = bol >> (==1) . sourceLine <$> getPosition >>= guard

nonSkipChar :: Char -> MyParser Char
nonSkipChar c = psSkipChars <$> getState >>= \s -> satisfy (\x -> notElem x s && x == c)

anyNonSkipChar :: MyParser Char
anyNonSkipChar = psSkipChars <$> getState >>= \s -> satisfy (`notElem` s)

oneOfNonSkipChar :: String -> MyParser Char
oneOfNonSkipChar sy = psSkipChars <$> getState >>= \sn -> satisfy (\x -> notElem x sn && elem x sy)

noneOfNonSkipChar :: String -> MyParser Char
noneOfNonSkipChar sy = psSkipChars <$> getState >>= \sn -> satisfy (\x -> notElem x sn && notElem x sy)

escapedChar :: MyParser Char
escapedChar =  fst <$> escapedChar'

escapedChar' :: MyParser (Char, Bool)
escapedChar' =  try (char '\\' >> ((,True) <$> oneOf "*_^~+-#![]|?{")) <|> ((,False) <$> anyNonSkipChar)

spaceChar :: MyParser Char
spaceChar = try (anyChar >>= \c -> if C.isSpace  c && notElem c "\r\n" then return c else mzero)

skipSpaces :: MyParser ()
skipSpaces = skipMany spaceChar

skipSpaces1 :: MyParser ()
skipSpaces1 = skipMany1 spaceChar

restOfLine :: MyParser ()
restOfLine = skipSpaces >> (eof <|> eol)

restOfLineNotEol :: MyParser ()
restOfLineNotEol = skipSpaces >> (lookAhead . try $ (eol <|> eof))

blankLine :: MyParser ()
blankLine =  try (eol >> skipSpaces >> (eof <|> eol))
         <|> try (bol >> skipSpaces >> eol) 
         <|> try (bol >> skipSpaces1 >> eof) 

printChar :: MyParser Char
printChar = try (escapedChar >>= \c -> if C.isSpace c then mzero else return c)

identity :: MyParser ()
identity = return ()

notFollowedBy' :: MyParser a -> MyParser ()
notFollowedBy' p = try $ ((False <$ try p) <|> return True) >>= guard
---------------------------------------------------------------------------------------------
-- Block parsing
---------------------------------------------------------------------------------------------

anyBlock :: MyParser Block
anyBlock = oneOfBlocks [emptyLines, table, heading, horizRule, numberedList, bulletList, paragraph]

oneOfBlocks :: [MyParser Block] -> MyParser Block
oneOfBlocks bs = choice . map perB $ bs
    where
        perB bp = try (modifyState (\s -> s{psBlockInlStack = []}) >> bp) 

emptyLines :: MyParser Block
emptyLines = Null <$ many1 blankLine


table :: MyParser Block
table = do
    tableStart 
    hs <- tableCells "||"
    rs <- tableRows
    let aln = map (const AlignLeft) hs
    let colw = map (const 0) hs 
    let cap = [Str ""]
    return $  Table cap aln colw hs rs

tableStart :: MyParser ()
tableStart = ((optional . try $ eol) >> bol >> skipSpaces >> void (string "||")) <?> "tableStart" 

tableRowStart :: MyParser ()
tableRowStart = bol >> skipSpaces >> void (string "|")

tableCells :: String -> MyParser [TableCell]
tableCells s = withInline inlineNoEol . nonReEntrant EncTableCell $ manyTill (manyEnd "|" e anyBlock) restOfLine 
    where 
        e = try (skipSpaces >> string s >> optional (try restOfLineNotEol)) <|> try restOfLineNotEol

tableRows :: MyParser [[TableCell]]
tableRows = withInline inlineNoEol . nonReEntrant EncTableRow $ r
    where
        r = manyEnd "" er (tableRowStart >> tableCells "|")
        er = eof <|> (lookAhead . try $ (bol >> skipSpaces >> (try eol <|> try eof <|> void (noneOfNonSkipChar "|"))))

heading :: MyParser Block
heading = psLevel <$> getState >>= \l -> headingStart >>= \d -> Header (d + l) nullAttr <$> h
    where
        h = manyEnd "" restOfLine inlineNoEol

headingStart :: MyParser Int
headingStart = (optional . try $ eol) >> bol >> skipSpaces >> oneOf "hH" >> C.digitToInt <$> digit >>= (<$ (oneOf "." >> skipSpaces))

horizRule :: MyParser Block
horizRule = HorizontalRule <$ (bol >> string "----" >> many (char '-') >> restOfLine)

numberedList :: MyParser Block
numberedList = anyList ListN

bulletList :: MyParser Block
bulletList = anyList ListB

listFmt :: Int -> ListNumberStyle
listFmt d = case d `mod` 4 of
    0 -> LowerAlpha
    1 -> LowerRoman
    2 -> UpperAlpha
    3 -> UpperRoman
    _ -> error "listFmt - should never happen"

listContent :: MyParser Block
listContent = oneOfBlocks [numberedList, bulletList, paragraph, table]

data ListType = ListB | ListN deriving (Eq, Show)
anyListStart :: MyParser (ListType, String)
anyListStart = (optional . try $ eol) >> bol >> skipSpaces 
    >> (fromJust . (`lookup`m) . last &&& id) <$> many1 (oneOfNonSkipChar s) 
    >>= (<$ skipSpaces1)
    where 
        m = [('*', ListB), ('#', ListN), ('-', ListB)]
        s = map fst m

anyList :: ListType -> MyParser Block
anyList t = do
    (t', pfx) <- anyListStart
    guard (t == t')
    nonReEntrant (EncList pfx) $ do
            let 
                d = length pfx
                enc = manyEnd' "" end listContent 
                end = choice [ lookAhead . try $ lowerList
                             , lookAhead . try $ changeList
                             , lookAhead . try $ thisList
                             , void . lookAhead . try $ blankLine
                             , void . lookAhead . try $ horizRule
                             , void . lookAhead . try $ headingStart
                             ]
                -- check for lower list 
                lowerList = (< d) . length . snd <$> anyListStart >>= guard
                -- check this list
                thisList = (pfx==) . snd <$> anyListStart >>=  guard 
                -- list depth is the same or higher but the prefix doesn't match
                changeList = (\a -> d <= length a && (not . isPrefixOf pfx) a) . snd <$> anyListStart >>= guard
                -- repeatedly parse list elements
                scan xss = enc >>= \xs -> (try thisList >> scan (xs:xss)) <|> return (xs:xss)
            case t of
                ListN -> OrderedList (d, listFmt d, DefaultDelim) . reverse <$> scan []
                ListB -> BulletList . reverse <$> scan []
            

paragraph :: MyParser Block
paragraph = getInline >>= \i -> nonReEntrant EncPara $ Para . collapseInlines <$> manyEnd1' "" e i >>= (<$ (optional . try $ restOfLine))
    where
        e = choice $ map (lookAhead . try) [ void blankLine
                                           , void tableStart
                                           , void headingStart
                                           , void anyListStart
                                           , void horizRule
                                           , void eof]

---------------------------------------------------------------------------------------------
-- Inline parsing
---------------------------------------------------------------------------------------------
inlineNoEolList :: [MyParser Inline]
inlineNoEolList = [ longDash <?> "longDash"
                  , mediumDash <?> "mediumDash"
                  , emphasis <?> "emphasis"
                  , strong <?> "strong"
                  , citation <?> "citation"
                  , deleted <?> "deleted"
                  , inserted <?> "inserted"
                  , superscript <?> "superscript"
                  , subscript <?> "subscript"
                  , monospaced <?> "monospaced"
                  , image <?> "image"
                  , lineBreak <?> "lineBreak"
                  , punctuation <?> "punctuation"
                  , normalWord <?> "normalWord"
                  , nonTrailingSpace <?> "nonTrailingSpace"
                  ]

inlineNoEol :: MyParser Inline
inlineNoEol = oneOfInlines inlineNoEolList

inlineEol :: MyParser Inline
inlineEol = oneOfInlines (inlineNoEolList ++ [inlEol <?> "inlEol"])

collapseInlines :: [Inline] -> [Inline]
collapseInlines = foldr f [] 
    where 
        f (Str !s1) (Str !s2:is')= Str (s1++s2):is'
        f i is' = i:is'

oneOfInlines :: [MyParser Inline] -> MyParser Inline
oneOfInlines  = try . perI
    where
        --perI ip = try ((try ip >>= \i -> i <$ modifyState (\s -> s{psBlockInlStack = i:psBlockInlStack s})) >>= traceMS "inline")
        perI [] = mzero
        perI (i:is) = try (i >>= success) <|> try (perI is) 
        success i = modifyState (\s -> s{psBlockInlStack = i:psBlockInlStack s}) >> return i



inlEol :: MyParser Inline
inlEol =  Space <$ try (skipSpaces >> eol) >>= (<$ notFollowedBy (failInlineTry <|> try ("eol" <$ restOfLine)))

failInlineLookAhead :: MyParser String
failInlineLookAhead = choice $ map (lookAhead . try) failInlineList

failInlineTry :: MyParser String
failInlineTry = choice $ map try failInlineList

failInlineList :: [MyParser String]
failInlineList = [ void blankLine >> return "blankLine"
                 , void tableStart >> return "tableStart"
                 , void headingStart >> return "headingStart"
                 , void anyListStart >> return "anyListStart"
                 , void horizRule >> return "horizRule"
                 , void eof >> return "eof"]
                                

addLineBreakBeforeAfterImages :: [Inline] -> MyParser [Inline]
addLineBreakBeforeAfterImages ls = return $ foldr f [] ls
    where
        f img@(Image _ _) ls' = LineBreak : img : LineBreak : ls'
        f l ls' = l : ls'

inlineFormat :: EncType                 -- Type
             -> String                  -- Delimeter
             -> MyParser [Inline]
inlineFormat t s = inlineFormat' t s s

inlineFormat':: EncType                 -- Type
             -> String                  -- Delimeter
             -> String                  -- Delimeter
             -> MyParser [Inline]
inlineFormat' t s e = let
        ps = try (matchedSpaceOrFirst >>= guard >> fmap (++) (string s) <*> fmap (:[])(lookAhead . try $ printChar))
        in try(getInline >>= \i ->  collapseInlines <$> nonReEntrant t (ps >> manyEndHist s (length s) (void printChar) (try . void . string $ e) (choice . map try $ (ps:failInlineList)) i) )

inlineVerbatim :: EncType               -- Type
             -> String                  -- Delimeter
             -> MyParser String
inlineVerbatim t s = inlineVerbatim' t s s

inlineVerbatim' :: EncType              -- Type
             -> String                  -- start delim
             -> String                  -- end delim
             -> MyParser String
inlineVerbatim' t s e = let
    h = length s
    l = void printChar
    end = void . string $ e
    inner = anyNonSkipChar 
    in nonReEntrant t (string s >> manyEndHist e h l end mzero inner)

nonTrailingSpace :: MyParser Inline
nonTrailingSpace = Space <$ (many1 spaceChar  >> (lookAhead . try) (anyChar >>= \c -> if C.isSpace c then mzero else return c))

-- _emphasis_
emphasis :: MyParser Inline
emphasis = Emph <$> inlineFormat EncEmph  "_"

-- *strong*
strong :: MyParser Inline
strong = Strong <$> inlineFormat EncStrong  "*"

-- -deleted-
deleted :: MyParser Inline
deleted =  Strikeout <$> inlineFormat EncDel "-"

-- +inserted+
inserted :: MyParser Inline
inserted = SmallCaps <$> inlineFormat EncIns "+"

-- ^superscript^
superscript :: MyParser Inline
superscript = Superscript <$> inlineFormat EncSup "^" 

-- ~subscript~
subscript :: MyParser Inline
subscript = Subscript <$> inlineFormat EncSub "~"

-- ??citation??
citation :: MyParser Inline
citation = Emph . (Str "\2014":) <$> inlineFormat EncCite "??"

-- {{monospaced}}
monospaced :: MyParser Inline
monospaced = trans . Span nullAttr <$> inlineFormat' EncMono "{{" "}}"
    where
        trans LineBreak         = LineBreak
        trans Space             = Space
        trans a@(Code _ _)      = a
        trans a@(Math _ _)      = a
        trans a@(RawInline _ _) = a
        trans a@(Note _ )       = a
        trans (Str s)           = Code nullAttr s
        trans (Emph is)         = Emph . map trans $ is
        trans (Strong is)       = Strong . map trans $ is
        trans (Strikeout is)    = Strikeout . map trans $ is
        trans (Superscript is)  = Superscript . map trans $ is
        trans (Subscript is)    = Subscript . map trans $ is
        trans (SmallCaps is)    = SmallCaps . map trans $ is
        trans (Quoted q is)     = Quoted q . map trans $ is
        trans (Cite c is)       = Cite c . map trans $ is
        trans (Link is t)       = (`Link` t) . map trans $ is
        trans (Image is t)      = (`Image` t) . map trans $ is
        trans (Span a is)       = Span a . map trans $ is

-- \\ line break
lineBreak :: MyParser Inline
lineBreak = LineBreak <$ (string "\\\\" >> (notFollowedBy . try) (string "\\"))

matchedSpaceOrFirst :: MyParser Bool
matchedSpaceOrFirst = chk . psBlockInlStack <$> getState
    where 
    chk []        = True
    chk (Space:_) = True
    chk _         = False

-- -- medium dash en dash U+2013
mediumDash :: MyParser Inline
mediumDash =  Str "\2013" <$ (matchedSpaceOrFirst >>= guard >> string "--" >> lookAhead ((try . void) spaceChar <|> void failInlineTry))

-- --- long dash em dash U+2014
longDash :: MyParser Inline
longDash = Str "\2014" <$ (matchedSpaceOrFirst >>= guard >> string "---" >> lookAhead ((try . void) spaceChar <|> void failInlineTry))

-- any normal word
normalWord :: MyParser Inline
normalWord = Str <$> many1 (try (printChar >>= (\c -> if C.isAlphaNum c then return c else mzero)))

punctuation :: MyParser Inline
punctuation = Str <$> do
    b <- matchedSpaceOrFirst
    let
        chk (c, _)      | C.isAlphaNum c         = mzero
        chk (c, _)      | C.isSpace c            = mzero
        chk (c, True)                            = return c
        chk (c, False)  | b && c `elem` "{?"     = notFollowedBy (try (nonSkipChar c >> printChar)) >> return c
        chk (c, False)  | b && c `elem`  "_*-+~" = notFollowedBy (try printChar) >> return c
        chk (c, _)                               = return c
    many1 (try (escapedChar' >>=  chk))

image :: MyParser Inline
image = do
    ImageLink imgL _ <- imageRaw
    return $ Image [Str imgL] (imgL, "")

-- parse an image link
data ImageLink = ImageLink 
    {
        imgLink :: String,
        imgAttrs :: Maybe String
    }deriving (Show, Eq)

imageRaw :: MyParser ImageLink
imageRaw = do
    s <- inlineVerbatim EncImage "!" 
    (l,a) <- parseFromString _prsLinkAttr s
    return $ ImageLink l a
    where
        _prsLinkAttr = do
            l <- many $ noneOf "|"
            a <- optionMaybe (char '|' >> many anyChar)
            return (l,a)

parseFromString :: MyParser a -> String -> MyParser a
parseFromString parser _str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput _str
  result <- parser
  setInput oldInput
  setPosition oldPos
  return result

imagesRaw :: MyParser [ImageLink]
imagesRaw = liftM concat $ many $ liftM (:[]) (try imageRaw) <|> (anyChar >> return [])

imagesFromDescription :: String -> [ImageLink]
imagesFromDescription d = 
    case r of
        Left e -> error $ show e
        Right a -> a
    where
        r = runParser imagesRaw defaultParseState "" d 

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
                pImage = liftM Left $ try imageRaw 
                pText =  liftM Right $ many1 pTextChar
                pTextChar = lookAhead (try pNotImage) >> anyChar
                pNotImage = ((False <$ imageRaw) <|> return True) >>= guard
        -- Transform parse results and turn into string again
        collapse :: String -> Either ImageLink String -> String 
        collapse sAcc (Left iml ) = let ImageLink m as = replacer iml in sAcc ++ "!" ++ m ++ maybe "" (\a-> '|' : a) as ++ "!"
        collapse sAcc (Right s) = sAcc ++ s
---------------------------------------------------------------------------------------------
parseDescription :: Int -> String -> [Block]
parseDescription l ds = 
    case res of
        Left e -> [RawBlock "" (show e)]
        Right b -> b
    where
        res = runParser prsDesc  defaultParseState {psLevel = l} "Desc" ds

prsDesc :: MyParser [Block]
prsDesc = liftM (filter noNull) (many1 anyBlock)
    where
        noNull Null = False
        noNull _ = True
