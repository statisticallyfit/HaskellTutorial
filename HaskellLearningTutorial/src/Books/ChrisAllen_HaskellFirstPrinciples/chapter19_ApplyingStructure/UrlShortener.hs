module UrlShortener where

import Control.Monad (replicateM)

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty



-- help Scotty failed to install:
{-
wai-extra-3.0.15.1 depends on streaming-commons-0.1.15.5 which failed to
install.

-}