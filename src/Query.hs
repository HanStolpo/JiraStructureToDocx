{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, DeriveGeneric, RankNTypes #-}
-- GHC_STATIC_OPTION_i=../src:../testsuite

module Query  (query
                    ) where

import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Char8 as B (pack)

import GHC.Generics
import Data.Maybe
import Data.Aeson
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Applicative
import Network.Socket(withSocketsDo)
-- local files
import ProgramOptions
import JiraTypes


data QueryRes_ = QueryRes_  { total      :: Int
                            , issues     :: [Issue]
                            } deriving (Show, Generic)
instance FromJSON QueryRes_

query :: Options -> IO [Issue]
query opts = case optQueryString opts of
    Nothing -> return []
    Just qs -> withSocketsDo $ withManager (_query 0)
        where 
            usr = pack . fromJust . optUsr $ opts
            pwd = pack . fromJust . optPwd $ opts
            baseUrl = fromJust . optBaseUrl $ opts  
            _query :: Int -> Manager -> ResourceT IO [Issue]
            _query fetched manager = do
                resBody <- liftM  responseBody $ httpLbs req manager
                case _decode resBody of
                    Left  e     -> fail $ "Error decoding response body : " ++ e
                    Right qr    -> let  is = issues qr
                                        totalFetched = length is + fetched
                                        in if totalFetched >= total qr then return is  else (is++) <$> _query totalFetched manager
                where 
                    url = baseUrl ++ "/rest/api/2/search?jql=" ++ qs ++ "&startAt=" ++ show (fetched) ++ "&fields=summary,description,attachment,issuelinks,status,labels,customfield_10900,customfield_10003" 
                    req = applyBasicAuth  usr pwd $ (fromJust $ parseUrl url)
                    _decode :: ByteString -> Either String QueryRes_
                    _decode = eitherDecode

