{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Hdbc
import           Database.HDBC.Sqlite3
import           Text.Templating.Heist

data App = App
    { _heist       :: Snaplet (Heist App)
    , _sess        :: Snaplet (SessionManager)
    , _db          :: Snaplet (HdbcSnaplet Connection)
    }

type Message = String

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "myapp" "My example application" Nothing $ do
    hs' <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    ss' <- nestSnaplet "session" sess $
            initCookieSessionManager "config/session.txt" "_session"
            (Just 3600)
    let sqli = connectSqlite3 "resources/tutorial.db"
    db' <- nestSnaplet "hdbc" db $ hdbcInit sqli
    addRoutes [ ("/hello", writeText "hello world")
              , ("/session", sessionHandler)
              , ("/getmsg", someNumHandler)
              , ("", heistServe)
              ]
    wrapHandlers $ withSession sess
    return $ App hs' ss' db'

sessionHandler :: Handler App App ()
sessionHandler = method GET getter <|> method POST setter
  where
    getter = do
        sessionList <- with sess $ sessionToList
        heistLocal (bindSnapletSplices (subSnaplet heist) [("sessions", liftHeist $ renderKeyValues sessionList)]) $ (render "session")
    setter = do
        mkey <- getParam "key"
        mvalue <- getParam "value"
        with sess $ setInSession (convert mkey) (convert mvalue)
        getter
    convert = T.pack . B.unpack . (fromMaybe "x_x Ooops!")

someNumHandler :: Handler App App ()
someNumHandler = do
  mnum <- getParam "num"
  let n = fromMaybe 0 $ bsToInt <$> mnum
  msgs <- getMessages n
  heistLocal (bindSnapletSplices (subSnaplet heist) [("messages", liftHeist $ renderMessages $ map T.pack msgs)]) $ render "queryresult"
  where
    bsToInt bs = read $ B.unpack bs :: Int

type Row = Map String SqlValue
getMessages :: HasHdbc m c => Int -> m [Message]
getMessages n = do
  rows <- query "SELECT * FROM message WHERE somenum = ?" [toSql n]
  return $ map toMsg rows
  where toMsg :: Row -> Message
        toMsg row = fromMaybe "x_x" (fromSql <$> M.lookup "msg" row)

-- TODO: renderKeyValue and renderMessage should be generalized
renderKeyValue :: Monad m => (Text, Text) -> Splice m
renderKeyValue pair = do
    runChildrenWithText [ ("key", fst pair)
                        , ("value", snd pair) ]

renderKeyValues :: Monad m => [(Text, Text)] -> Splice m
renderKeyValues = mapSplices renderKeyValue

renderMessage :: Monad m => Text -> Splice m
renderMessage msg = runChildrenWithText [("message", msg)]

renderMessages :: Monad m => [Text] -> Splice m
renderMessages = mapSplices renderMessage

instance HasHeist App where
  heistLens = subSnaplet heist
instance HasHdbc (Handler App App) Connection where
  getPool = with db $ gets hdbcPool
-- TODO: why getPool?

main :: IO ()
main = serveSnaplet defaultConfig appInit
