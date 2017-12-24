module HttpStuff where

import Data.ByteString.Lazy
-- import Network.Wreq -- help wasn't installed well.
{-
streaming-commons-0.1.15.5 failed during the building phase. The exception
was:
ExitFailure 1
wreq-0.4.1.0 depends on streaming-commons-0.1.15.5 which failed to install.-}


urls :: [String]
urls = ["http://httpbin.com/ip", "http://httpbin.org/bytes/5"]
{-
mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls-}

