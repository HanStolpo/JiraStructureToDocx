-- Blah blah
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, TupleSections, DoAndIfThenElse#-}

module JiraStructureToIssueHierarchy (fetchHierarchy) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Data.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BW8
import Network (withSocketsDo)
import Data.Maybe
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C
import Data.Aeson as AS
import Data.Typeable
import Data.Data
import Control.Monad.Error
import GHC.Generics
import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Debug.Trace
import Text.PrettyPrint.GenericPretty as GP
import Text.JSON as JS
import Text.JSON.Pretty as JS
import qualified Data.Map as M
import Data.List
import System.Directory
import System.FilePath
-- local files
import IssueHierarchy
import ImageStripper
import ProgramOptions

 
fetchHierarchy :: Options -> IO ()
fetchHierarchy opts = withSocketsDo $ runResourceT $ do
    liftIO $ createDirectoryIfMissing True $ dropFileName . optHierarchyFile $ opts
    manager <- liftIO $ newManager def
    let usrn = B.pack.fromJust.optUsr $ opts
    let pwd = B.pack.fromJust.optPwd $ opts
    let baseUrl = fromJust.optBaseUrl $ opts
    let structureId = show.fromJust.optStructureId $ opts
    let url =  baseUrl ++ "/rest/structure/1.0/structure/" ++ structureId ++"/forest"
    let req = applyBasicAuth  usrn pwd (fromJust $ parseUrl url)
    liftIO $ putStrLn "Requesting structure from jira"
    res <- httpLbs req manager
    liftIO $ putStrLn "Decoding strucuture forest"
    forest <- decodeForest $ responseBody res
    liftIO $ putStrLn "Getting structure issues from jira"
    hierarchy <- forestToHierarchy manager usrn pwd (baseUrl ++ "/rest/api/2/issue/") forest
    liftIO $ putStrLn "Localising images"
    let images = extractImages hierarchy
    imagesLoc <- localizeImages manager usrn pwd baseUrl (dropFileName . optHierarchyFile $ opts) images
    let hierarchy' = replaceImagesUri imagesLoc hierarchy
    liftIO $ putStrLn "Writing out hierarchy file"
    liftIO $ writeFile (optHierarchyFile opts) $ GP.pretty hierarchy'
    return ()

---------------------------------------------------------------------------
-- The JASON representation of a fores
data JsForest = JsForest
    {
        {-jsfStructure :: Int,           -- The structure ID-}
        {-jsfVersion :: Int,             -- The version-}
        {-jsFroot :: Maybe Int,          -- The optional root ID-}
        jsfFormula :: BW8.ByteString   -- The structure forest formula
    } 
    deriving (Data, Typeable, Show, Generic)

-- Specify JSON parser for JsForest
instance FromJSON JsForest where
    parseJSON (AS.Object v) =   JsForest <$>
                                {-v AS..: "structure" <*>-}
                                {-v AS..: "version" <*>-}
                                {-v AS..:? "root" <*>-}
                                v AS..: "formula"
    parseJSON _ = error "Expecting a JASON object for JsForest"

-- Get JsForest given query resutl
decodeJsForest :: Monad m => L.ByteString -> m JsForest
decodeJsForest s = 
    case r of
        Left e -> fail e
        Right f -> return f
    where
        r = AS.eitherDecode s
    


---------------------------------------------------------------------------
-- Intermediate representation of a forest ie structure 
data Forest = Forest 
    { 
        ftIssueId :: Maybe Int,     -- The possible issue ID which is Nothing for root
        ftChildren :: [Forest]      -- The children at this level in the hierarchy
    }
    deriving (Data, Typeable, Show, Generic)

instance Out Forest

-- Decode the forest structure from the query
decodeForest :: Monad m => L.ByteString -> m Forest
decodeForest s = do
      jsForest <- decodeJsForest s
      listIdDepth <- decodeForestFormula $ jsfFormula jsForest
      return $ buildForest listIdDepth
 
-- Decode the forest formula into list of (issueId, issueDepth) pairs
decodeForestFormula :: Monad m => BW8.ByteString -> m [(Int, Int)]
decodeForestFormula s = 
    case (r :: Either String [(Int, Int)]) of 
        Left e -> fail e
        Right l -> return l
    where
        r = AP.parseOnly p s
        p = pTpl `AP.sepBy` AP.string ","
        pTpl = makeTpl <$> AP.number <* AP.string ":" <*> AP.number
        makeTpl (AP.I a) (AP.I b) = (fromIntegral a, fromIntegral b)
        makeTpl _ _ = error "Decoding forest expecting integer tuples"

-- Build the Forest hierarchy given the decoded forest formula as list of (issueId, issueDepth) pairs
buildForest :: [(Int,Int)] -> Forest
buildForest listIdDepth = Forest Nothing (fst $ makeC (-1) listIdDepth)
    where
        makeC _ [] = ([], [])
        makeC d allts@((i,d'):ts)
            | d' > d =  
                let 
                    (cs, ts') = makeC d' ts
                    (ss, ts'') = makeS d' ts'
                in (Forest (Just i) cs : ss, ts'')
            | otherwise = ([], allts)
        makeS _ [] = ([], [])
        makeS d allts@((i,d'):ts)
            | d' == d = 
                let
                    (cs, ts') = makeC d' ts
                    (ss, ts'') = makeS d' ts'
                in (Forest (Just i) cs : ss, ts'')
            | otherwise = ([], allts)
                    
                

---------------------------------------------------------------------------
-- Class to represent the JASON of an issue

data JsIssue = JsIssue
    {
        jsiKey :: String,               -- The issue key
        jsiSummary :: String,           -- The summary field 
        jsiDescription :: Maybe String, -- The optional description field
        jsiAttachments :: [Attachment]  -- The list of optional attachments
    } 
    deriving (Show, Generic)

-- Specify JSON parser for JsIssue 
instance FromJSON JsIssue where
    parseJSON (AS.Object v) = do
        key <- v AS..: "key"
        fields <- v AS..: "fields"
        summary <- fields AS..: "summary"
        description <- fields AS..:? "description"
        attachments <- fields AS..:? "attachment" AS..!= []
        return $ JsIssue key summary description attachments
    parseJSON _ = error "Expecting JSON object for JsIssue"

-- From some reason the aeson parser concs out on valid input when running the build executable from
-- the command line but it does not happen when running as a script though runghc. I am not sure why
-- obviously I am doing something wrong. So parsing JSON rencoding it then giving it to aeson.
decodeJsIssue :: Monad m => L.ByteString -> m JsIssue
decodeJsIssue s = decJs >>= encJs >>= decIssue
    where
        s' = L8.unpack s 
        decJs = case JS.decode s' of
            JS.Ok a -> return $ JS.pp_js_object a
            JS.Error e -> fail ("JASON response failure : " ++ e ++ ": \noriginal was:\n" ++ s')
        encJs d = return $ render d
        decIssue s'' = case AS.eitherDecode . L8.pack $ s'' of
                        Left e -> fail $ "Error decoding JASON for issue : " ++ e ++ ":\nreceived was:\n" ++ s' ++ "\npretty printed:\n" ++ s''
                        Right f -> return f


---------------------------------------------------------------------------
-- 
forestToHierarchy :: (MonadBaseControl IO m, MonadResource m) => 
                     Manager
                     -> BW8.ByteString
                     -> BW8.ByteString
                     -> String
                     -> Forest
                     -> m IssueHierarchy
forestToHierarchy manager usrn pwd baseUrl forest = do
    let issueId = ftIssueId forest
    case issueId of
        Just i -> forestToHierarchyIssue manager usrn pwd baseUrl forest i
        _ -> forestToHierarchyNoIssue manager usrn pwd baseUrl forest
   
forestToHierarchyIssue :: (MonadBaseControl IO m, MonadResource m) => 
                          Manager
                          -> BW8.ByteString
                          -> BW8.ByteString
                          -> String
                          -> Forest
                          -> Int
                          -> m IssueHierarchy
forestToHierarchyIssue manager usrn pwd baseUrl forest i = do
    let url = baseUrl ++ show i ++ "/?fields=summary,description,attachment"
    let req =  applyBasicAuth  usrn pwd  (fromJust $ parseUrl url)
    res <- trace ("fetching " ++ show i) $ httpLbs req manager
    jsIssue <- decodeJsIssue $ responseBody res
    children <- makeChildren manager usrn pwd baseUrl $ ftChildren forest
    return $ IssueHierarchy (jsiKey jsIssue) (jsiSummary jsIssue) (fromMaybe "" (jsiDescription jsIssue)) (jsiAttachments jsIssue) children

forestToHierarchyNoIssue :: (MonadBaseControl IO m, MonadResource m) => 
                            Manager
                            -> BW8.ByteString
                            -> BW8.ByteString
                            -> String
                            -> Forest
                            -> m IssueHierarchy
forestToHierarchyNoIssue manager usrn pwd baseUrl forest = do
    children <- makeChildren manager usrn pwd baseUrl $ ftChildren forest
    return $ IssueHierarchy "root" "" "" [] children

makeChildren :: (MonadBaseControl IO m, MonadResource m) =>
                Manager
                -> BW8.ByteString
                -> BW8.ByteString
                -> String
                -> [Forest]
                -> m [IssueHierarchy]
makeChildren _ _ _ _ [] = return []
makeChildren manager usrn pwd baseUrl (f:fs) =  do
    i <- forestToHierarchy manager usrn pwd baseUrl f
    is <- makeChildren manager usrn pwd baseUrl fs
    return $ i : is

localizeImages :: (Monad m, MonadIO m, MonadBaseControl IO m, MonadResource m) => 
                  Manager 
                  -> B.ByteString 
                  -> B.ByteString 
                  -> String 
                  -> FilePath
                  -> ImageMap 
                  -> m ImageMap
localizeImages manager usrn pwd baseurl outDir im = do
    liftIO $ createDirectoryIfMissing True outDir'
    liftM M.fromList $ forM (M.toList im) getImg 
        where
            outDir' = outDir </> "Images"
            getImg (k, img@(Image _ url _))
                | baseurl `isPrefixOf` url =  liftM (k,) $ getImage manager usrn pwd outDir' img
                | otherwise = return (k, img)


getImage :: (MonadBaseControl IO m, MonadResource m) =>
            Manager 
            -> BW8.ByteString 
            -> BW8.ByteString 
            -> FilePath
            -> Image 
            -> m Image
getImage manager usrn pwd outDir img@(Image _ url _) = do
    let req = (applyBasicAuth usrn pwd  (fromJust $ parseUrl url)) {checkStatus = \_ _ _ -> Nothing}
    res <- http req manager
    if responseStatus res == ok200 
        then do
            responseBody res C.$$+- sinkFile (outDir </> fn)
            return $ img {imgUrl = outDir </> fn}
        else 
            return $ trace ("Failed looking up image" ++ GP.pretty (img, fn)) img 
    where 
        fn = reverse . takeWhile cnd . reverse $ url
        cnd '/' = False
        cnd _ = True
    


{-txt = L8.pack "{\"expand\":\"renderedFields,names,schema,transitions,operations,editmeta,changelog\",\"id\":\"16253\",\"self\":\"http://srv1.za.5dt.com:8090/rest/api/2/issue/16253\",\"key\":\"LYNX-1055\",\"fields\":{\"summary\":\"EGI Gyro-Compass Mode\",\"description\":\"The EGI gyro-compass mode will be emulated as follows:\\r\\n# The EGI will enter gyro-compass alignment mode when commanded to do so (by NAV mode selection on the CDNU).\\r\\n# The EGI will remain in gyro-compass mode until motion is detected, or another mode is selected.\\r\\n\\r\\n\\r\\n\\r\\nThe EGI sensor will have a valid initialization position, when provided\\r\\n# manually, \\r\\n# or by valid GPS data\\r\\n\\r\\nWhen the EGI sensor is in gyro-compass alignment mode, it will\\r\\n# Provide align status \\r\\n## on reset will assume an align status of 1\\r\\n## after 2 seconds, it will assume an align status of 2\\r\\n## after 90 seconds, and has a valid initialization position, it will assume an align status of 3\\r\\n## after 240 seconds, it will assume an align status of 4\\r\\n# Reset align status when a different initialization position is provided.\\r\\n\\r\\n \",\"attachment\":[]}}"-}
{-main :: IO ()-}
{-main = decodeJsIssue txt >>= print-}
