module Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
  ( mkPageBody
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo (mkOwnInfo)
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPageBody
  :: UI.Window
  -> UI (Element, Element, Element)
mkPageBody window = do
  noNodesNotify
    <- UI.div #. "container is-max-widescreen has-text-centered" #+
         [ image "rt-view-no-nodes-icon" noNodesSVG
         , UI.p #. "rt-view-no-nodes-message" #+
             [ string "There are no connected nodes. Yet." ]
         ]
  rootElemForNodePanels
    <- UI.div #. "container is-max-widescreen" #+
         [ UI.div #. "panel is-link rt-view-node-panel" #+
              [ UI.p #. "panel-heading" #+
                  [ UI.div #. "columns" #+
                      [ UI.div #. "column" #+
                          [ string "Name:" #. "rt-view-node-name"
                          , string "core-1"
                          , UI.span #. "tag is-warning is-medium ml-4" # set text "Producer"
                          ]
                      , UI.div #. "column has-text-right" #+
                          [ image "rt-view-node-panel-down" downSVG
                          ]
                      ]
                  ]
              , UI.div #. "tabs is-centered is-boxed is-size-5 mt-1 rt-view-tabs" #+
                  [ UI.ul #+
                      [ UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" healthSVG,     string "Health"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Node"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"] ]
                      , UI.li #. "is-active" #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Chain"] ]
                      , UI.li                #+ [ UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon",       string "Tx"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Bugs"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "GC"] ]
                      ]
                  ]
              , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                  [ UI.div #. "table-container rt-view-peers-table-container" #+
                      [ UI.table #. "table rt-view-peers-table" #+
                          [ UI.mkElement "thead" #+
                              [ UI.tr #+
                                  [ UI.th #+ [string "Endpoint"]
                                  , UI.th #+ [string "Slots number"]
                                  , UI.th #. "rt-view-narrow-th" #+ [UI.mkElement "abbr" # set UI.title__ "Bytes in flight" #+ [string "Bts"]]
                                  , UI.th #. "rt-view-narrow-th" #+ [UI.mkElement "abbr" # set UI.title__ "Requests in flight" #+ [string "Req"]]
                                  , UI.th #. "rt-view-narrow-th" #+ [UI.mkElement "abbr" # set UI.title__ "Blocks in flight" #+ [string "Blk"]]
                                  , UI.th #+ [string "Status"]
                                  ]
                              ]
                          , UI.mkElement "tbody" #+
                              [ UI.tr #+
                                  [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text "127.0.0.1:8100"]
                                  , UI.td #+ [string "346"]
                                  , UI.td #+ [string "1"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                                  ]
                              , UI.tr #+
                                  [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text "127.0.0.1:8200"]
                                  , UI.td #+ [string "346"]
                                  , UI.td #+ [string "1"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [string "1"]
                                  , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                                  ]
                              , UI.tr #+
                                  [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text "127.0.0.1:8300"]
                                  , UI.td #+ [string "344"]
                                  , UI.td #+ [string "1"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [string "0"]
                                  , UI.td #+ [UI.span #. "tag is-dark is-medium" # set text "Busy"]
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
         , UI.div #. "panel is-link rt-view-node-panel" #+
              [ UI.p #. "panel-heading" #+
                  [ UI.div #. "columns" #+
                      [ UI.div #. "column" #+
                          [ string "Name:" #. "rt-view-node-name"
                          , string "core-2"
                          , UI.span #. "tag is-warning is-medium ml-4" # set text "Producer"
                          ]
                      , UI.div #. "column has-text-right" #+
                          [ image "rt-view-node-panel-down" downSVG
                          ]
                      ]
                  ]
              , UI.div #. "tabs is-centered is-boxed is-size-5 mt-1 rt-view-tabs" #+
                  [ UI.ul #+
                      [ UI.li #. "is-active" #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" healthSVG,     string "Health"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Node"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Chain"] ]
                      , UI.li                #+ [ UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon",       string "Tx"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Bugs"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "GC"] ]
                      ]
                  ]
              , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                  [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                      [ UI.div #. "column is-half has-text-right" #+
                          [ UI.p #. "mb-1" #+ [ string "Node version" ]
                          , UI.p #+ [ string "Node uptime" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Epoch", string "/" #. "rt-view-slash", string "Slot" ]
                          , UI.p #. "mb-1" #+ [ string "Minted blocks, number" ]
                          , UI.p #+ [ string "KES periods remaining" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Connected peers" ]
                          ]
                      , UI.div #. "column is-half has-text-weight-semibold" #+
                          [ UI.p #. "mb-1" #+ [image "rt-view-overview-icon" versionSVG, string "1.27.0"]
                          , UI.p #+ [image "rt-view-overview-icon" clockSVG, string "00:12:15"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+
                              [ image "rt-view-overview-icon" epochSlotSVG
                              , string "125", string "/" #. "rt-view-slash", string "67"
                              ]
                          , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" hammerSVG, string "16"]
                          , UI.p #+ [image "rt-view-overview-icon" remainingSVG, string "230"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" peersNumSVG, string "3"]
                          ]
                      ]
                  ]
              ]
         , UI.div #. "panel is-link rt-view-node-panel" #+
              [ UI.p #. "panel-heading" #+
                  [ UI.div #. "columns" #+
                      [ UI.div #. "column" #+
                          [ string "Name:" #. "rt-view-node-name"
                          , string "relay-1"
                          , UI.span #. "tag is-light is-medium ml-4" # set text "Relay"
                          ]
                      , UI.div #. "column has-text-right" #+
                          [ image "rt-view-node-panel-down" downSVG
                          ]
                      ]
                  ]
              , UI.div #. "tabs is-centered is-boxed is-size-5 mt-1 rt-view-tabs" #+
                  [ UI.ul #+
                      [ UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" healthSVG,     string "Health"] ]
                      , UI.li #. "is-active" #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Node"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Chain"] ]
                      , UI.li                #+ [ UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon",       string "Tx"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Bugs"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "GC"] ]
                      ]
                  ]
              , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                  [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                      [ UI.div #. "column is-half has-text-right" #+
                          [ UI.p #. "mb-1" #+ [ string "Protocol" ]
                          , UI.p #. "mb-1" #+ [ string "Version" ]
                          , UI.p #. "mb-1" #+ [ string "Commit" ]
                          , UI.p #+ [ string "Platform" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Start time" ]
                          , UI.p #+ [ string "Uptime" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Connected to" ]
                          ]
                      , UI.div #. "column is-half has-text-weight-semibold" #+
                          [ UI.p #. "mb-1" #+ [image "rt-view-overview-icon" protocolSVG, string "Shelley"]
                          , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" versionSVG,  string "1.27.0"]
                          , UI.p #. "mb-1" #+
                              [ image "rt-view-overview-icon" commitSVG
                              , UI.anchor # set UI.href "#" # set UI.text "df98476"
                              ]
                          , UI.p #+ [image "rt-view-overview-icon" linuxSVG,    string "Linux"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" calendarSVG, string "2021-08-03 10:24:01 UTC"]
                          , UI.p #+ [image "rt-view-overview-icon" clockSVG,    string "00:11:05"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+
                              [ image "rt-view-overview-icon" connectedSVG
                              , UI.span #. "tag is-success is-light is-medium is-family-monospace" # set text "/var/run/node-1.sock"
                              ]
                          ]
                      ]
                  ]
              ]
         , UI.div #. "panel is-link rt-view-node-panel" #+
              [ UI.p #. "panel-heading" #+
                  [ UI.div #. "columns" #+
                      [ UI.div #. "column" #+
                          [ string "Name:" #. "rt-view-node-name"
                          , string "relay-2"
                          , UI.span #. "tag is-light is-medium ml-4" # set text "Relay"
                          ]
                      , UI.div #. "column has-text-right" #+
                          [ image "rt-view-node-panel-down" downSVG
                          ]
                      ]
                  ]
              , UI.div #. "tabs is-centered is-boxed is-size-5 mt-1 rt-view-tabs" #+
                  [ UI.ul #+
                      [ UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" healthSVG,     string "Health"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" overviewSVG,   string "Node"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" kesSVG,        string "KES"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" peersSVG,      string "Peers"] ]
                      , UI.li #. "is-active" #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" blockchainSVG, string "Chain"] ]
                      , UI.li                #+ [ UI.anchor #+ [string "₳" #. "rt-view-ada-node-icon",       string "Tx"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" errorsSVG,     string "Bugs"] ]
                      , UI.li                #+ [ UI.anchor #+ [image "rt-view-node-tab-icon" rtsGCSVG,      string "GC"] ]
                      ]
                  ]
              , UI.div #. "panel-block is-size-5 rt-view-node-panel-block" #+
                  [ UI.div #. "columns is-variable is-2-mobile is-3-desktop is-5-widescreen rt-view-node-panel-cols" #+
                      [ UI.div #. "column is-half has-text-right" #+
                          [ UI.p #+ [ string "Blockchain start time" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Epoch", string "/" #. "rt-view-slash", string "Slot" ]
                          , UI.p #. "mb-1" #+ [ string "Blocks number" ]
                          , UI.p #+ [ string "Chain density" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Slot leader, number" ]
                          , UI.p #+ [ string "Minted blocks number" ]
                          , UI.p #. "rt-view-vspace-with-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [ string "Cannot mint, number" ]
                          , UI.p #+ [ string "Missed slots, number" ]
                          ]
                      , UI.div #. "column is-half has-text-weight-semibold" #+
                          [ UI.p           #+ [image "rt-view-overview-icon" calendarSVG, string "2021-08-03 10:24:01 UTC"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+
                              [ image "rt-view-overview-icon" epochSlotSVG
                              , string "124", string "/" #. "rt-view-slash", string "5"
                              ]
                          , UI.p #. "mb-1" #+ [image "rt-view-overview-icon" blockSVG, string "568"]
                          , UI.p #+ [image "rt-view-overview-icon" chainSVG, string "2.4%"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" leaderSVG, string "15"]
                          , UI.p #+ [image "rt-view-overview-icon" hammerSVG, string "15"]
                          , UI.p #. "rt-view-vspace-for-hr" #+ []
                          , UI.p #. "mb-1 mt-3" #+ [image "rt-view-overview-icon" sadSVG, string "—"]
                          , UI.p #+ [image "rt-view-overview-icon" sadSVG, string "2" #. "has-text-danger"]
                          ]
                      ]
                  ]
              ] 
         ]
  body
    <- UI.getBody window #+
         [ topNavigation
         , UI.mkElement "section" #. "section" #+
             [ element noNodesNotify
               -- element rootElemForNodePanels
             ]
         ]
  return (body, noNodesNotify, rootElemForNodePanels)

topNavigation :: UI Element
topNavigation = do
  closeInfo <- UI.button #. "modal-close is-large" #+ []
  info <- mkOwnInfo closeInfo
  infoIcon <- image "mr-4 rt-view-info-icon" rtViewInfoSVG # set UI.title__ "RTView info"
  registerClicksForModal info infoIcon closeInfo

  closeNotifications <- UI.button #. "modal-close is-large" #+ []
  notifications <- mkOwnInfo closeNotifications
  notifyIcon <- image "rt-view-notify-icon" rtViewNotifySVG # set UI.title__ "RTView notifications"
  registerClicksForModal notifications notifyIcon closeNotifications

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
        [ UI.div #. "navbar-start" #+ []
        , UI.div #. "navbar-end" #+
            [ UI.div #. "navbar-item" #+ [ element notifyIcon ]
            , UI.div #. "navbar-item" #+ [ element infoIcon ]
            ]
        ]
    ]
 where
  registerClicksForModal modal iconToOpen iconToClose = do
    on UI.click iconToOpen  $ const $ element modal #. "modal is-active"
    on UI.click iconToClose $ const $ element modal #. "modal"
