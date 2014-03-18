-- | Run a simple Conduit pipeline as a socket-based analysis.
module Analysis where

import           ClassyPrelude.Conduit
import qualified Control.Concurrent.Async     as Async
import qualified Control.Monad.Trans.Resource as Res
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Network         as DCN
import           StockTicker                  (StockTick (..), sinkTick)

type Analysis = Conduit StockTick IO Text

-- | Dummy analysis: groups prices into vectors of size 4 and prints them.
sampleAnalysis :: Analysis
sampleAnalysis = mapC stPrice
             =$= conduitVector 4
             =$= mapC (\v -> "Next 4 prices: " ++ tshow (asVector v))

-- | Takes a stock ticker to connect to, a broadcast function for output,
-- and an analysis, and runs the analysis in a separate thread.
resAnalysis :: DCN.ClientSettings IO -- ^ connection info for ticker
            -> (Text -> IO ()) -- ^ broadcast
            -> Analysis
            -> Res.Resource (Async.Async ())
resAnalysis tickerSettings broadcast analysis =
    Res.mkResource start Async.cancel
  where
    start = Async.async $ forever $ do
        res <- tryAny $ DCN.runTCPClient tickerSettings $ \ad ->
            DCN.appSource ad
            $$ CL.sequence sinkTick
            =$ analysis
            =$ mapM_C broadcast
        print res