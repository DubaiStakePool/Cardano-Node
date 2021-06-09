{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.WebServer
  ( runWebServer
  ) where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad (void)
import qualified Data.ByteString.Char8 as BSC
import           Data.Text (Text)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, liftIO, onEvent, set, (#), (#+))
--import           Graphics.UI.Threepenny.Timer (interval, start, tick, timer)

--import           Cardano.RTView.CLI (RTViewParams (..))
--import           Cardano.RTView.GUI.CSS.Style (ownCSS)
--import           Cardano.RTView.GUI.Elements (TmpElements, pageTitle)
import           Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody (mkPageBody)
--import           Cardano.RTView.GUI.Updater (updateGUI)
--import           Cardano.RTView.NodeState.Types (NodesState)
--import           Cardano.RTView.Notifications.Types (NotificationSettings)

import           Cardano.Tracer.Configuration
-- import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own (ownCSS)
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma (bulmaCSS)

runWebServer :: Endpoint -> IO ()
runWebServer (Endpoint host port) = UI.startGUI config mainPage
 where
  config = UI.defaultConfig
    { UI.jsPort = Just port
    , UI.jsAddr = Just $ BSC.pack host
    }

mainPage :: UI.Window -> UI ()
mainPage window = do
  void $ return window # set UI.title "RTView" -- pageTitle
  embedCSS window bulmaCSS
  -- embedCSS window ownCSS
  pageBody <- mkPageBody window
  void $ UI.element pageBody

embedCSS :: UI.Window -> String -> UI ()
embedCSS window css = void $ do
  el <- UI.mkElement "style" # set UI.html css
  UI.getHead window #+ [UI.element el]

  {-
mainPage
  :: Trace IO Text
  -> Configuration
  -> TVar NodesState
  -> TVar TmpElements
  -> TVar NotificationSettings
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> UI.Window
  -> UI ()
mainPage tr config nsTVar tmpElsTVar notifyTVar params acceptors window = do
  liftIO $ logNotice tr "Web page loading..."

  void $ return window # set UI.title pageTitle

  -- It is assumed that CSS files are available at 'pathToStatic/css/'.
  UI.addStyleSheet window "w3.css"
  embedOwnCSS window

  -- It is assumed that JS files are available at 'pathToStatic/js/'.
  addJavaScript window "chart.js"

  -- Make page's body (HTML markup).
  (pageBody, nodesStateElems) <- mkPageBody config
                                            nsTVar
                                            tmpElsTVar
                                            notifyTVar
                                            params
                                            window
                                            acceptors
  let guiTr = appendName "GUI" tr
  -- Start the timer for GUI update. Every second it will
  -- call a function which updates node state elements on the page.
  guiUpdateTimer <- timer # set interval 1000
  void $ onEvent (tick guiUpdateTimer) $ \_ ->
    updateGUI guiTr window nsTVar tmpElsTVar params nodesStateElems
  start guiUpdateTimer

  void $ UI.element pageBody

-- | Add JS library stored locally.
addJavaScript
  :: UI.Window
  -> FilePath
  -> UI ()
addJavaScript w filename = void $ do
  el <- UI.mkElement "script" # set UI.src ("/static/js/" ++ filename)
  UI.getHead w #+ [UI.element el]

-}


