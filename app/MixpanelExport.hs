module MixpanelExport (exportRaw) where

import Codec.Binary.Base64.String (encode)
import Control.Exception (catch, throwIO)
import Control.Lens ((&), (.~), view)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Data.Semigroup((<>))
import qualified Data.Text as T (pack)
import Network.Wreq (defaults, header, getWith, manager, param, responseBody)
import Network.HTTP.Client (HttpException(..), managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit(die)

exportRaw :: String -> String -> String -> IO ByteString
exportRaw apiSecret startDate endDate =
    view responseBody <$> getEvents apiSecret (T.pack startDate) (T.pack endDate)

    where
        exportEndpoint = "https://data.mixpanel.com/api/2.0/export"

        options secret fromDate toDate = defaults
            & param "format" .~ ["json"]
            & param "from_date" .~ [fromDate]
            & param "to_date" .~ [toDate]
            & header "Authorization" .~ ["Basic " <> (pack $ encode secret)]
            & manager .~ Left (tlsManagerSettings { managerResponseTimeout = timeout })

        timeout = responseTimeoutMicro 600000000

        getEvents secret fromDate toDate =
            getWith (options secret fromDate toDate) exportEndpoint `catch` httpErrorHandler

        -- Avoid printing out the api secret
        httpErrorHandler (HttpExceptionRequest _ c) = die $ "mixpanel-export: " <> show c
        httpErrorHandler e = throwIO e
