module Parsers.Byron
  ( ByronOptions(..)
  , cmdByron
  , runByronOptions
  ) where

import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.Common.Parsers hiding (pNetworkId)

import           Testnet.Byron
import           Testnet.Run (runTestnet)
import           Testnet.Util.Cli

newtype ByronOptions = ByronOptions
  { testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsByron :: Parser ByronOptions
optsByron = ByronOptions <$> optsTestnet

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-bft-nodes"
      <>  OA.help "Number of BFT nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numBftNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (slotDuration defaultTestnetOptions)
      )
  <*> pNetworkId
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (securityParam defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "n-poor-addresses"
      <>  OA.help "N poor addresses"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (nPoorAddresses defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total Balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (totalBalance defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (enableP2P defaultTestnetOptions)
      )

runByronOptions :: ByronOptions -> IO ()
runByronOptions opts = runTestnet (Testnet.Byron.testnet (testnetOptions opts))

cmdByron :: Mod CommandFields ByronOptions
cmdByron = command' "byron" "Start a Byron testnet" optsByron
