{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Text.Templating.Heist

data App = App
    { _heist       :: Snaplet (Heist App)
    , _sess        :: Snaplet (SessionManager)
    }

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "myapp" "My example application" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    ss <- nestSnaplet "session" sess $ initCookieSessionManager "config/session.txt" "_session" (Just 3600)
    addRoutes [ ("/hello", writeText "hello world")
              , ("/session", sessionHandler)
              , ("", heistServe)
              ]
    wrapHandlers $ withSession sess
    return $ App hs ss

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


renderKeyValue :: Monad m => (Text, Text) -> Splice m
renderKeyValue pair = do
    runChildrenWithText [ ("key", fst pair)
                        , ("value", snd pair) ]

renderKeyValues :: Monad m => [(Text, Text)] -> Splice m
renderKeyValues = mapSplices renderKeyValue




instance HasHeist App where heistLens = subSnaplet heist

main :: IO ()
main = serveSnaplet defaultConfig appInit
