module Main where

import qualified Data.ByteString.Lazy as B (putStr)
import MixpanelExport (exportRaw)
import Options.Generic
import System.Environment (getEnv)

type StartDate = String <?> "Start of date range in YYYY-MM-DD format. Inclusive"
type EndDate = String <?> "End of date range in YYYY-MM-DD format. Inclusive"

data Options = Options StartDate EndDate
    deriving (Generic, Show)

instance ParseRecord Options

main = do
    (Options (Helpful startDate) (Helpful endDate)) <- getRecord "mixpanel-export"
    mixpanelApiSecret <- getEnv "MIXPANEL_API_SECRET"
    body <- exportRaw mixpanelApiSecret startDate endDate
    B.putStr body
