{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
  ( mkPageBody
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo (mkOwnInfo)

mkPageBody :: UI.Window -> UI Element
mkPageBody window = do
  --di <- image "rt-view-node-panel-down" downSVG
  --on UI.click di $ const $ 

  body <- UI.getBody window #+
    [ topNavigation
    , UI.mkElement "section" #. "section" #+
        [ UI.div #. "container is-max-widescreen" #+
            [ UI.div #. "panel" #+
                [ UI.p #. "panel-heading" #+
                    [ UI.div #. "columns" #+
                        [ UI.div #. "column" #+
                            [ string "Node: core-1"
                            ]
                        , UI.div #. "column has-text-right" #+
                            [ image "rt-view-node-panel-down" downSVG
                            ]
                        ]
                    ]
                , UI.p #. "panel-tabs is-size-5" #+
                    [ UI.anchor #. "is-active" #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Overview"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Blockchain"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" mempoolSVG,    string "Mempool"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Errors"]
                    , UI.anchor                #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "RTS GC"]
                    ]
                , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                    [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                        [ UI.div #. "column is-half has-text-right" #+
                            [ UI.p #. "mb-1" #+ [ string "Node protocol" ]
                            , UI.p #. "mb-1" #+ [ string "Node version" ]
                            , UI.p #. "mb-1" #+ [ string "Node commit" ]
                            , UI.p #. "mb-1" #+ [ string "Node platform" ]
                            , UI.p #. "mb-1" #+ [ string "Node start time" ]
                            , UI.p           #+ [ string "Node uptime" ]
                            ]
                        , UI.div #. "column is-half has-text-weight-semibold" #+
                            [ UI.p #. "mb-1" #+ [image "rt-view-overview-icon" protocolSVG, string "Shelley"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" versionSVG,  string "1.0"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" commitSVG,   string "abcdefg"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" linuxSVG,    string "Linux"]
                            , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" calendarSVG, string "2021-01-01 01:01:01 UTC"]
                            , UI.p           #+ [image "rt-view-overview-icon" clockSVG,    string "00:11:05"]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
  return body

topNavigation :: UI Element
topNavigation = do
  closeInfo <- UI.button #. "modal-close is-large" #+ []
  info <- mkOwnInfo closeInfo
  rtViewInfoIcon <- image "rt-view-info-icon" rtViewInfoSVG # set UI.title__ "See RTView info"
  on UI.click rtViewInfoIcon $ const $ element info #. "modal is-active"
  on UI.click closeInfo      $ const $ element info #. "modal"

  closeNotifications <- UI.button #. "modal-close is-large" #+ []
  notifications <- mkOwnInfo closeNotifications
  rtViewNotifyIcon <- image "rt-view-notify-icon" rtViewNotifySVG # set UI.title__ "RTView notifications"
  on UI.click rtViewNotifyIcon   $ const $ element info #. "modal is-active"
  on UI.click closeNotifications $ const $ element info #. "modal"

  nodes <- UI.div #. "navbar-dropdown is-boxed" #+
    [ UI.div #. "navbar-item" #+ [string "core-1"]
    , UI.div #. "navbar-item" #+ [string "core-2"]
    , UI.div #. "navbar-item" #+ [string "core-3"]
    , UI.div #. "navbar-item" #+ [string "relay-3"]
    , UI.div #. "navbar-item" #+ [string "relay-3"]
    , UI.div #. "navbar-item" #+ [string "relay-3"]
    ]

  UI.div #. "navbar rt-view-top-bar" #+
    [ element info
    , element notifications
    , UI.div #. "navbar-brand" #+
        [ UI.div #. "navbar-item" #+
            [ image "rt-view-cardano-logo" cardanoLogoSVG
            , UI.span #. "rt-view-name" # set text "Node Real-time View"
            ]
        ]
    , UI.div #. "navbar-menu" #+
        [ UI.div #. "navbar-start" #+
            [ --UI.div #. "navbar-item has-dropdown is-hoverable has-text-info-light" #+
              --  [ string "Node"
              --  , element nodes
              --  ]
            ]
        , UI.div #. "navbar-end" #+
            [ UI.div #. "navbar-item" #+
                [ element rtViewNotifyIcon
                ]
            , UI.div #. "navbar-item" #+
                [ element rtViewInfoIcon
                , UI.span #. "mr-4" #+ []
                ]
            ]
        ]
    ]



  {-
  <div id="navbarExampleTransparentExample" class="navbar-menu">
    <div class="navbar-start">
      <div class="navbar-item has-dropdown is-hoverable">
        <a class="navbar-link" href="https://bulma.io/documentation/overview/start/">
          Docs
        </a>
        <div class="navbar-dropdown is-boxed">
          <a class="navbar-item" href="https://bulma.io/documentation/overview/start/">
            Overview
          </a>
          <a class="navbar-item" href="https://bulma.io/documentation/overview/modifiers/">
            Modifiers
          </a>
          <a class="navbar-item" href="https://bulma.io/documentation/columns/basics/">
            Columns
          </a>
          <a class="navbar-item" href="https://bulma.io/documentation/layout/container/">
            Layout
          </a>
          <a class="navbar-item" href="https://bulma.io/documentation/form/general/">
            Form
          </a>
          <hr class="navbar-divider">
          <a class="navbar-item" href="https://bulma.io/documentation/elements/box/">
            Elements
          </a>
          <a class="navbar-item is-active" href="https://bulma.io/documentation/components/breadcrumb/">
            Components
          </a>
        </div>
      </div>
    </div>
-}




  {-
      <div class="navbar-item has-dropdown is-hoverable">
        <a class="navbar-link" href="https://bulma.io/documentation/overview/start/">
          Docs
        </a>
        
      </div>
-}


  {-
  UI.div #. [W3Bar, W3Large, TopBar] #+
    [ UI.anchor #. [W3BarItem, W3Mobile] # set UI.href "https://cardano.org/" #+
        [ UI.img #. [CardanoLogo] # set UI.src "/static/images/cardano-logo.svg"
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Node"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+ nodesSelector
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Tab"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+ tabs
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Columns"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+
            [ element nodesColumns1
            , element nodesColumns2
            , element nodesColumns3
            ]
        ]
    , UI.span #. [W3BarItem, W3Mobile] #+
        [ element rtViewInfoButton
        , element rtViewInfo
        ]
    , UI.span #. [W3BarItem, W3Mobile] #+
        [ element rtViewNotificationsButton
        , element rtViewNotifications
        ]
    , UI.span #. [W3Right, W3HideMedium, W3HideSmall, ServiceName] #+
        [ string "Cardano Node Real-time View"
        ]
    ]
-}






image :: String -> String -> UI Element
image imgClass svg = UI.span #. imgClass # set UI.html svg

  {-

    <a role="button" class="navbar-burger" aria-label="menu" aria-expanded="false" data-target="navbarBasicExample">
      <span aria-hidden="true"></span>
      <span aria-hidden="true"></span>
      <span aria-hidden="true"></span>
    </a>
  </div>

  <div id="navbarBasicExample" class="navbar-menu">
    <div class="navbar-start">
      <a class="navbar-item">
        Home
      </a>

      <a class="navbar-item">
        Documentation
      </a>

      <div class="navbar-item has-dropdown is-hoverable">
        <a class="navbar-link">
          More
        </a>

        <div class="navbar-dropdown">
          <a class="navbar-item">
            About
          </a>
          <a class="navbar-item">
            Jobs
          </a>
          <a class="navbar-item">
            Contact
          </a>
          <hr class="navbar-divider">
          <a class="navbar-item">
            Report an issue
          </a>
        </div>
      </div>
    </div>

    <div class="navbar-end">
      <div class="navbar-item">
        <div class="buttons">
          <a class="button is-primary">
            <strong>Sign up</strong>
          </a>
          <a class="button is-light">
            Log in
          </a>
        </div>
      </div>
    </div>
  </div>
</nav>
-}

  {-
  UI.div #. [W3Bar, W3Large, TopBar] #+
    [ UI.anchor #. [W3BarItem, W3Mobile] # set UI.href "https://cardano.org/" #+
        [ UI.img #. [CardanoLogo] # set UI.src "/static/images/cardano-logo.svg"
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Node"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+ nodesSelector
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Tab"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+ tabs
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Columns"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+
            [ element nodesColumns1
            , element nodesColumns2
            , element nodesColumns3
            ]
        ]
    , UI.span #. [W3BarItem, W3Mobile] #+
        [ element rtViewInfoButton
        , element rtViewInfo
        ]
    , UI.span #. [W3BarItem, W3Mobile] #+
        [ element rtViewNotificationsButton
        , element rtViewNotifications
        ]
    , UI.span #. [W3Right, W3HideMedium, W3HideSmall, ServiceName] #+
        [ string "Cardano Node Real-time View"
        ]
    ]
-}

{-

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad (forM, forM_, void, when)
import           Control.Monad.Extra (whenJustM)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, set, string, (#), (#+))

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

import           Cardano.RTView.CLI (RTViewParams (..))
import qualified Cardano.RTView.GUI.JS.Charts as Chart
import           Cardano.RTView.GUI.JS.Utils (goToTab)
import           Cardano.RTView.GUI.Elements (HTMLClass (..), HTMLId (..),
                                              NodesStateElements, TmpElements,
                                              hideIt, showIt, (##), (#.))
import           Cardano.RTView.GUI.Markup.Notifications (mkNotifications)
import           Cardano.RTView.GUI.Markup.OwnInfo (mkOwnInfo)
import           Cardano.RTView.GUI.Markup.Pane (mkNodesPanes)
import           Cardano.RTView.NodeState.Types
import           Cardano.RTView.Notifications.Types

mkPageBody
  :: Configuration
  -> TVar NodesState
  -> TVar TmpElements
  -> TVar NotificationSettings
  -> RTViewParams
  -> UI.Window
  -> [RemoteAddrNamed]
  -> UI (Element, NodesStateElements)
mkPageBody config nsTVar tmpElsTVar notifyTVar params window acceptors = do
  (paneNodesRootElem, paneNodesElems, panesWithNames)
    <- mkNodesPanes window nsTVar tmpElsTVar acceptors

  -- Register clickable selector for nodes (to be able to show only one or all of them).
  nodesSelector <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    nodeCheckbox
      <- UI.input #. [W3Check, SelectNodeCheck]
                  # set UI.type_ "checkbox"
                  # set UI.checked True
                  #+ []
    nodeButton <-
      UI.div #. [SelectNodeCheckArea] #+
        [ element nodeCheckbox
        , UI.label #+ [UI.string $ T.unpack nameOfNode]
        ]

    void $ UI.onEvent (UI.checkedChange nodeCheckbox) $ \isChecked -> do
      let action = if isChecked then showIt else hideIt
      forNodePane nameOfNode panesWithNames action
      changeStatusOfShowAllButton window ShowAllNodesButton SelectNodeCheck
      changeStatusOfHideAllButton window HideAllNodesButton SelectNodeCheck

    return $ element nodeButton

  showAndHideAllNodesButtons
    <- if length nodesSelector > 1
         then do
           showAllNodesButton <- showAllButton ShowAllNodesButton
           hideAllNodesButton <- hideAllButton HideAllNodesButton

           void $ UI.onEvent (UI.click showAllNodesButton) $ \_ -> do
             showAllNodes window panesWithNames
             void $ element showAllNodesButton #. [W3BarItem, W3Button, W3Disabled]
             void $ element hideAllNodesButton #. [W3BarItem, W3Button, W3BorderBottom]

           void $ UI.onEvent (UI.click hideAllNodesButton) $ \_ -> do
             hideAllNodes window panesWithNames
             void $ element showAllNodesButton #. [W3BarItem, W3Button]
             void $ element hideAllNodesButton #. [W3BarItem, W3Button, W3BorderBottom, W3Disabled]

           return [element showAllNodesButton, element hideAllNodesButton]
         else
           return []

  let allNodesSelectors = showAndHideAllNodesButtons ++ nodesSelector

  body
    <- UI.getBody window #+
         [ topNavigation window acceptors config params notifyTVar allNodesSelectors
         , element paneNodesRootElem
         ]

  UI.runFunction $ UI.ffi Chart.prepareChartsJS

  forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    -- Charts for different metrics.
    UI.runFunction $ UI.ffi Chart.memoryUsageChartJS  (showt MemoryUsageChartId  <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.cpuUsageChartJS     (showt CPUUsageChartId     <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.diskUsageChartJS    (showt DiskUsageChartId    <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.networkUsageChartJS (showt NetworkUsageChartId <> nameOfNode)

  return (body, paneNodesElems)

showt :: Show a => a -> Text
showt = T.pack . show

topNavigation
  :: UI.Window
  -> [RemoteAddrNamed]
  -> Configuration
  -> RTViewParams
  -> TVar NotificationSettings
  -> [UI Element]
  -> UI Element
topNavigation window acceptors config params notifyTVar nodesSelector = do
  rtViewInfo <- mkOwnInfo config params
  rtViewInfoButton <- UI.img #. [RTViewInfoIcon]
                             # set UI.src "/static/images/info-light.svg"
                             # set UI.title__ "See RTView info"
  void $ UI.onEvent (UI.click rtViewInfoButton) $ \_ ->
    element rtViewInfo # showIt

  rtViewNotificationsButton <- UI.img #. [NotificationsIcon]
                                      # set UI.src "/static/images/bell.svg"
                                      # set UI.title__ "Set RTView notifications"
  rtViewNotifications <- mkNotifications window config params notifyTVar rtViewNotificationsButton
  void $ UI.onEvent (UI.click rtViewNotificationsButton) $ \_ ->
    element rtViewNotifications # showIt

  nodesColumns1 <- UI.anchor #. [W3BarItem, W3Button, W3Mobile]            #+ [UI.string "1 node"]
  nodesColumns2 <- UI.anchor #. [W3BarItem, W3Button, W3Mobile, ActiveTab] #+ [UI.string "2 nodes"]
  nodesColumns3 <- UI.anchor #. [W3BarItem, W3Button, W3Mobile]            #+ [UI.string "3 nodes"]

  makeItemsActive [nodesColumns1, nodesColumns2, nodesColumns3]

  changeColumns window acceptors nodesColumns1 [W3L12, W3M12, W3S12]
  changeColumns window acceptors nodesColumns2 [W3L6,  W3M12, W3S12]
  changeColumns window acceptors nodesColumns3 [W3L4,  W3M12, W3S12]

  let anchorWithIcon icon title =
        UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                  # set UI.title__ ("Open " <> title <> " tab for all nodes")
                  #+
          [ UI.img #. [AllTabsIcon]
                   # set UI.src ("/static/images/" <> icon)
          , UI.string title
          ]

  nodeInfoTab   <- anchorWithIcon "info.svg"       "Node info"
  kesTab        <- anchorWithIcon "key.svg"        "KES"
  peersTab      <- anchorWithIcon "peers.svg"      "Peers"
  blockchainTab <- anchorWithIcon "blockchain.svg" "Blockchain"
  mempoolTab    <- anchorWithIcon "mempool.svg"    "Mempool"
  resTabMemory  <- anchorWithIcon "memory.svg"     "Memory usage"
  resTabCPU     <- anchorWithIcon "cpu.svg"        "CPU usage"
  ghcRTSTab     <- anchorWithIcon "rts.svg"        "RTS GC"
  errorsTab     <- anchorWithIcon "bugs.svg"       "Errors"
                     #. [W3BarItem, W3Button, W3Mobile, W3Disabled]
                     ## show ErrorsTabsSwitcher

  let tabsItems =
        [ (nodeInfoTab,   NodeInfoTab)
        , (kesTab,        KESTab)
        , (peersTab,      PeersTab)
        , (blockchainTab, BlockchainTab)
        , (mempoolTab,    MempoolTab)
        , (resTabMemory,  ResTabMemory)
        , (resTabCPU,     ResTabCPU)
        , (errorsTab,     ErrorsTab)
        , (ghcRTSTab,     RTSGCTab)
        ]

  makeItemsActiveIfEnabled window $ map fst tabsItems

  tabs :: [UI Element] <-
    forM tabsItems $ \(tab, tabId) -> do
      void $ UI.onEvent (UI.click tab) $ \_ ->
        forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) ->
          UI.runFunction $ UI.ffi goToTab (show tabId <> T.unpack nameOfNode)
      return $ element tab

  UI.div #. [W3Bar, W3Large, TopBar] #+
    [ UI.anchor #. [W3BarItem, W3Mobile] # set UI.href "https://cardano.org/" #+
        [ UI.img #. [CardanoLogo] # set UI.src "/static/images/cardano-logo.svg"
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Node"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+ nodesSelector
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Tab"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+ tabs
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Columns"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] #+
            [ element nodesColumns1
            , element nodesColumns2
            , element nodesColumns3
            ]
        ]
    , UI.span #. [W3BarItem, W3Mobile] #+
        [ element rtViewInfoButton
        , element rtViewInfo
        ]
    , UI.span #. [W3BarItem, W3Mobile] #+
        [ element rtViewNotificationsButton
        , element rtViewNotifications
        ]
    , UI.span #. [W3Right, W3HideMedium, W3HideSmall, ServiceName] #+
        [ string "Cardano Node Real-time View"
        ]
    ]

makeItemsActive
  :: [Element]
  -> UI ()
makeItemsActive cols = do
  let nodesCols = zip cols [1 :: Int .. length cols]
  forM_ nodesCols $ \(el, num) ->
    void $ UI.onEvent (UI.click el) $ \_ ->
      forM_ nodesCols $ \(el', num') ->
        if num == num'
          then void $ element el' #. [W3BarItem, W3Button, W3Mobile, ActiveTab]
          else void $ element el' #. [W3BarItem, W3Button, W3Mobile]

makeItemsActiveIfEnabled
  :: UI.Window
  -> [Element]
  -> UI ()
makeItemsActiveIfEnabled window cols = do
  let nodesCols = zip cols [1 :: Int .. length cols]
  forM_ nodesCols $ \(el, num) ->
    void $ UI.onEvent (UI.click el) $ \_ ->
      forM_ nodesCols $ \(el', num') ->
        if num == num'
          then void $ element el' #. [W3BarItem, W3Button, W3Mobile, ActiveTab]
          else do
            void $ element el' #. [W3BarItem, W3Button, W3Mobile]
            -- By default Errors switcher is disabled (it will be cative only if there are some errors).
            whenJustM (UI.getElementById window (show ErrorsTabsSwitcher)) $ \switcher ->
              void $ element switcher #. [W3BarItem, W3Button, W3Mobile, W3Disabled]
                                      # set UI.title__ "Good news: there are no errors!"

changeColumns
  :: UI.Window
  -> [RemoteAddrNamed]
  -> Element
  -> [HTMLClass]
  -> UI ()
changeColumns window acceptors colItem colWidthClasses =
  void $ UI.onEvent (UI.click colItem) $ \_ -> do
    changeColumnsWidth
    changeChartsWidth
 where
  changeColumnsWidth =
    UI.getElementsByClassName window (show NodePaneArea) >>=
      mapM_ (\area -> void $ element area #. ([W3Col, NodePaneArea] ++ colWidthClasses))

  changeChartsWidth =
    forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
      UI.runFunction $ UI.ffi Chart.resizeChartJS (showt MemoryUsageChartId  <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.resizeChartJS (showt CPUUsageChartId     <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.resizeChartJS (showt DiskUsageChartId    <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.resizeChartJS (showt NetworkUsageChartId <> nameOfNode)

forNodePane
  :: Text
  -> [(Text, Element)]
  -> (UI Element -> UI Element)
  -> UI ()
forNodePane nameOfNode panesWithNames' action =
  forM_ panesWithNames' $ \(aName, pane) ->
    when (aName == nameOfNode) $
      void $ element pane # action

showAllNodes, hideAllNodes
  :: UI.Window
  -> [(Text, Element)]
  -> UI ()
showAllNodes = changeNodesVisibility True
hideAllNodes = changeNodesVisibility False

changeNodesVisibility
  :: Bool
  -> UI.Window
  -> [(Text, Element)]
  -> UI ()
changeNodesVisibility showThem window panesWithNames' = do
  forM_ panesWithNames' $ \(_, pane) ->
    void $ element pane # if showThem then showIt else hideIt
  nodesCheckboxes <- UI.getElementsByClassName window (show SelectNodeCheck)
  forM_ nodesCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked showThem

-- | If all checkboxes are checked - "Show all" button should be disabled.
--   If at least one of them are unchecked - "Show all" button should be enabled.
changeStatusOfShowAllButton
  :: UI.Window
  -> HTMLId
  -> HTMLClass
  -> UI ()
changeStatusOfShowAllButton window anId aClass =
  whenJustM (UI.getElementById window (show anId)) $ \button -> do
    checkboxes <- UI.getElementsByClassName window (show aClass)
    statuses <- mapM (UI.get UI.checked) checkboxes
    if all (True ==) statuses
      then void $ element button #. [W3BarItem, W3Button, W3Disabled]
      else void $ element button #. [W3BarItem, W3Button]

-- | If all checkboxes are unchecked - "Hide all" button should be disabled.
--   If at least one of them are checked - "Hide all" button should be enabled.
changeStatusOfHideAllButton
  :: UI.Window
  -> HTMLId
  -> HTMLClass
  -> UI ()
changeStatusOfHideAllButton window anId aClass =
  whenJustM (UI.getElementById window (show anId)) $ \button -> do
    checkboxes <- UI.getElementsByClassName window (show aClass)
    statuses <- mapM (UI.get UI.checked) checkboxes
    if all (False ==) statuses
      then void $ element button #. [W3BarItem, W3Button, W3BorderBottom, W3Disabled]
      else void $ element button #. [W3BarItem, W3Button, W3BorderBottom]

showAllButton, hideAllButton :: HTMLId -> UI Element
showAllButton anId = mkButton anId [W3BarItem, W3Button, W3Disabled]     "show.svg" "Show all"
hideAllButton anId = mkButton anId [W3BarItem, W3Button, W3BorderBottom] "hide.svg" "Hide all"

mkButton
  :: HTMLId
  -> [HTMLClass]
  -> String
  -> String
  -> UI Element
mkButton anId classes icon label =
  UI.anchor ## show anId #. classes # set UI.href "#" #+
    [ UI.img #. [ShowHideIcon] # set UI.src ("/static/images/" <> icon)
    , string label
    ]
    -}
