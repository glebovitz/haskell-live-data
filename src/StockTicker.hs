-- | This is a server application. Clients connect on a socket, and are sent
-- packets whenever an updated price is available. The format for an update is
--
-- SYMBOL YYYY-MM-DD PRICE\n
--
-- Price is a decimal value.
module StockTicker
    ( StockTicker
    , StockTick (..)
    , resTicker
    , sendUpdate
    , tickToBS
    , sinkTick
    ) where

import           ClassyPrelude.Conduit
import qualified Control.Concurrent.Async     as Async
import           Control.Concurrent.STM
import qualified Control.Monad.Trans.Resource as Res
import qualified Data.Conduit.Network         as DCN
import qualified Data.Scientific              as Sc

data StockTick = StockTick
    { stSymbol :: !Text
    , stDay    :: !Day
    , stPrice  :: !Sc.Scientific
    }
    deriving (Show, Eq, Ord, Read, Typeable)

data StockTicker = StockTicker
    { stChan  :: !(TChan StockTick)
    , stAsync :: !(Async.Async ())
    }

resTicker :: Int -- ^ port
          -> Res.Resource StockTicker
resTicker port = Res.mkResource (startTicker port) stopTicker

startTicker :: Int -> IO StockTicker
startTicker port = do
    baton <- atomically newEmptyTMVar
    chan <- atomically newBroadcastTChan
    let settings = (DCN.serverSettings port "*" :: DCN.ServerSettings IO)
            { DCN.serverAfterBind = const $ atomically $ putTMVar baton ()
            }
        app = tickerApp chan
    a <- Async.async $ DCN.runTCPServer settings app
    atomically $
        (StockTicker chan a <$ takeTMVar baton) <|>
        (Async.waitSTM a >> throwSTM TickerDidn'tStart)

tickerApp :: TChan StockTick -> DCN.Application IO
tickerApp chan0 ad = do
    chan <- atomically $ dupTChan chan0
    forever $ do
        tick <- atomically $ readTChan chan
        yield (tickToBS tick) $$ DCN.appSink ad

tickToBS :: StockTick -> ByteString
tickToBS (StockTick s d v) = encodeUtf8 $ toStrict $ builderToLazy $
    toBuilder s ++
    toBuilder ' ' ++
    toBuilder (tshow d) ++
    toBuilder ' ' ++
    Sc.scientificBuilder v ++
    toBuilder '\n'

sinkTick :: MonadThrow m => Consumer ByteString m StockTick
sinkTick =
    StockTick <$> sinkSymbol <*> sinkDay <*> sinkScientific
  where
    expect w = do
        mw <- headCE
        case mw of
            Just w' | w == w' -> return ()
            _ -> monadThrow $ UnexpectedByte w mw
    sinkSymbol = (takeWhileCE (/= 32) =$= decodeUtf8C =$= foldC) <* expect 32
    sinkDay = (takeWhileCE (/= 32) =$= decodeUtf8C =$= (foldC >>= parseDay)) <* expect 32
    parseDay t =
        case readMay t of
            Nothing -> monadThrow $ InvalidDay t
            Just d -> return d
    sinkScientific = (takeWhileCE (/= 10) =$= decodeUtf8C =$= (foldC >>= parseSci)) <* expect 10
    parseSci t =
        case readMay t of
            Nothing -> monadThrow $ InvalidScientific t
            Just d -> return d

data StockTickerException
    = TickerDidn'tStart
    | UnexpectedByte !Word8 !(Maybe Word8)
    | InvalidDay !Text
    | InvalidScientific !Text
    deriving (Show, Typeable)
instance Exception StockTickerException

stopTicker :: StockTicker -> IO ()
stopTicker = Async.cancel . stAsync

sendUpdate :: MonadIO m => StockTicker -> StockTick -> m ()
sendUpdate ticker = liftIO . atomically . writeTChan (stChan ticker)
