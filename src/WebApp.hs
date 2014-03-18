module WebApp where

import ClassyPrelude.Yesod
import Analysis
import StockTicker
import Control.Concurrent.STM
import Control.Monad.Trans.Resource (with)
import Data.Scientific (fromFloatDigits)
import Yesod.EventSource (repEventSource)
import Network.Wai.EventSource (ServerEvent (ServerEvent))
import qualified Data.Conduit.Network as DCN
import Yesod.Form.Jquery (YesodJquery (..))

data App = App
    { ticker :: !StockTicker
    , broadcast :: !(TChan Text)
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/new-price PriceR POST
/stream StreamR GET
/blank BlankR GET
|]

type Form x = Html -> MForm Handler (FormResult x, Widget)

instance Yesod App
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
instance YesodJquery App

priceForm :: Form StockTick
priceForm = renderBootstrap $ StockTick
    <$> areq textField "Symbol" (Just "FPCO")
    <*> areq dayField "Date" (Just $ fromGregorian 2014 03 01)
    <*> (fromFloatDigits <$> areq doubleField "Price" (Just 1234.56))

getHomeR :: Handler Html
getHomeR = do
    (form, enctype) <- generateFormPost priceForm
    defaultLayout $ do
        setTitle "Event system demo"
        getYesod >>= addScriptEither . urlJqueryJs
        [whamlet|
            <form action=@{PriceR} method=post enctype=#{enctype} target=ignored>
                ^{form}
                <input type=submit value="Add new tick">
            <div #output>
            <iframe src="/blank" style=display:none name=ignored>
        |]
        toWidget [julius|
            $(function(){
                var output = document.getElementById("output");
                var src = new EventSource("@{StreamR}");
                src.onmessage = function(msg){
                    var p = document.createElement("p");
                    p.appendChild(document.createTextNode(msg.data));
                    output.appendChild(p);
                }
            });
        |]

postPriceR :: Handler ()
postPriceR = do
    ((res, _), _) <- runFormPost priceForm
    case res of
        FormSuccess tick -> do
            app <- getYesod
            sendUpdate (ticker app) tick
        _ -> return ()

getStreamR :: Handler TypedContent
getStreamR = do
    app <- getYesod
    chan <- liftIO $ atomically $ dupTChan $ broadcast app
    repEventSource $ \_ -> forever $ do
        t <- liftIO $ atomically $ readTChan chan
        yield $ ServerEvent Nothing Nothing [toBuilder t]

getBlankR :: Handler ()
getBlankR = return ()

main :: IO ()
main = do
    chan <- atomically newBroadcastTChan
    let port = 23456
        res =
            (resTicker port) <*
            (resAnalysis
                (DCN.clientSettings port "127.0.0.1")
                (atomically . writeTChan chan)
                sampleAnalysis)
    with res $ warpEnv . flip App chan
