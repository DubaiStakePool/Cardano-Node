{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Testnet.Parsers
  ( commands
  , runTestnetCmd
  , pref
  , opts
  ) where

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt
import           Parsers.Babbage
import           Parsers.Conway
import           Parsers.Byron
import           Parsers.Cardano
import           Parsers.Help
import           Parsers.Shelley
import           Parsers.Version

pref :: ParserPrefs
pref = Opt.prefs $ showHelpOnEmpty <> showHelpOnError

opts :: ParserInfo CardanoTestnetCommands
opts = Opt.info (commands <**> helper) idm

-- TODO: Remove StartBabbageTestnet and StartShelleyTestnet
-- by allowing the user to start testnets in any era (excluding Byron)
-- via StartCardanoTestnet
data CardanoTestnetCommands
  = StartBabbageTestnet BabbageOptions
  | StartConwayTestnet ConwayOptions
  | StartByrontestnet ByronOptions -- TODO: Do we care about being able to start a Byron only testnet?
  | StartCardanoTestnet CardanoOptions
  | StartShelleyTestnet ShelleyOptions
  | GetVersion VersionOptions
  | Help ParserPrefs (ParserInfo CardanoTestnetCommands) HelpOptions

commands :: Parser CardanoTestnetCommands
commands =
  asum
    [ fmap StartCardanoTestnet (subparser cmdCardano)
    , fmap StartByrontestnet (subparser cmdByron)
    , fmap StartShelleyTestnet (subparser cmdShelley)
    , fmap StartBabbageTestnet (subparser cmdBabbage)
    , fmap StartConwayTestnet (subparser cmdConway)
    , fmap GetVersion (subparser cmdVersion)
    , fmap (Help pref opts) (subparser cmdHelp)
    ]


runTestnetCmd :: CardanoTestnetCommands -> IO ()
runTestnetCmd = \case
  StartBabbageTestnet cmdOpts -> runBabbageOptions cmdOpts
  StartConwayTestnet cmdOpts -> runConwayOptions cmdOpts
  StartByrontestnet cmdOpts -> runByronOptions cmdOpts
  StartCardanoTestnet cmdOpts -> runCardanoOptions cmdOpts
  StartShelleyTestnet cmdOpts -> runShelleyOptions cmdOpts
  GetVersion cmdOpts -> runVersionOptions cmdOpts
  Help pPrefs pInfo cmdOpts -> runHelpOptions pPrefs pInfo cmdOpts
