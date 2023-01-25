module Parsers.Babbage
  ( cmdBabbage
  , runBabbageOptions
  ) where

import           Options.Applicative
import qualified Options.Applicative as OA
import           Prelude

import           Parsers.Cardano
import           Testnet
import qualified Testnet.Cardano as Cardano
import           Testnet.Cardano (CardanoTestnetOptions (..), TestnetNodeOptions (..),
                   cardanoNumPoolNodes)
import           Testnet.Options
import           Testnet.Run (runTestnet)
import           Testnet.Util.Runtime (readNodeLoggingFormat)



optsBabbageTestnet :: Parser CardanoTestnetOptions
optsBabbageTestnet = CardanoTestnetOptions
  <$> (numSpoNodes <$> OA.option auto
      (   OA.long "num-spo-nodes"
      <>  OA.help "Number of SPO nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (cardanoNumPoolNodes $ cardanoNodes Cardano.defaultTestnetOptions)
      ))
  <*> pure Cardano.Babbage
  <*> pEpochLength
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (cardanoSlotLength Cardano.defaultTestnetOptions)
      )
  <*> pure 0 -- Active slots coefficient does not make sense in Babbage
             -- because there is no leadership schedule and therefore
             -- not possible to run a BFT node.
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (cardanoMaxSupply Cardano.defaultTestnetOptions)
      )
  <*> pEnableP2P
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (babbageNodeLoggingFormat defaultTestnetOptions)
      )
 where
  numSpoNodes :: Int -> [TestnetNodeOptions]
  numSpoNodes = flip replicate (SpoTestnetNodeOptions [])

optsBabbage :: Parser CardanoOptions
optsBabbage = CardanoOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsBabbageTestnet

runBabbageOptions :: CardanoOptions -> IO ()
runBabbageOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.testnet (CardanoOnlyTestnetOptions $ testnetOptions options)

cmdBabbage :: Mod CommandFields (IO ())
cmdBabbage = command "babbage"  $ flip info idm $ runBabbageOptions <$> optsBabbage
