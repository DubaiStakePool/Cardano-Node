{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.WebServer
  ( runWebServer
  ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Monad (void)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, liftIO, on, onEvent, set, (#), (#+))
--import           Graphics.UI.Threepenny.Timer (interval, start, tick, timer)

--import           Cardano.RTView.CLI (RTViewParams (..))
--import           Cardano.RTView.GUI.CSS.Style (ownCSS)
--import           Cardano.RTView.GUI.Elements (TmpElements, pageTitle)
import           Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody (mkPageBody)
--import           Cardano.RTView.GUI.Updater (updateGUI)
--import           Cardano.RTView.NodeState.Types (NodesState)
--import           Cardano.RTView.Notifications.Types (NotificationSettings)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own (ownCSS)
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma (bulmaCSS)
import           Cardano.Tracer.Handlers.RTView.UI.Elements (PageElements)
import           Cardano.Tracer.Types (AcceptedItems)

runWebServer
  :: Endpoint
  -> AcceptedItems
  -> IO ()
runWebServer (Endpoint host port) _acceptedItems =
  UI.startGUI config mainPage
 where
  config = UI.defaultConfig
    { UI.jsPort = Just port
    , UI.jsAddr = Just $ BSC.pack host
    }

  mainPage window = do
    void $ return window # set UI.title "RTView" -- pageTitle
    void $ UI.getHead window #+
      [ UI.link # set UI.rel "icon" # set UI.href "data:,"
      , UI.meta # set UI.name "viewport" # set UI.content "width=device-width, initial-scale=1"
      , UI.mkElement "style" # set UI.html bulmaCSS
      , UI.mkElement "style" # set UI.html ownCSS
      -- , UI.mkElement "script" # set UI.html chartJS
      ]
    
    pageElementsTVar :: TVar PageElements <- liftIO $ newTVarIO HM.empty
    
    (pageBody, noNodesNotify, rootElemForNodePanels) <- mkPageBody window

    -- Prepare and run the timer, which will call 'updateUI' function once per second .
    uiUpdateTimer <- UI.timer # set UI.interval 1000
    on UI.tick uiUpdateTimer $ const $
      -- updateUI acceptedItems
      return ()
    UI.start uiUpdateTimer

    on UI.disconnect window $ const $ do
      -- If we're here, it means that the connection with the browser
      -- was interrupted. So:
      -- 1. All the Elements we stored for the regular updating,
      --    are "invalid" now (we cannot manipulate them anymore),
      --    so we must delete them explicitly.
      -- 2. Previous timer should be stopped.
      UI.stop uiUpdateTimer

    void $ UI.element pageBody


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


