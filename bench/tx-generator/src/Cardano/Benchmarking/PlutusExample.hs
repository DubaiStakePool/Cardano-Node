{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.PlutusExample
where
import qualified Data.Map.Strict as Map
import           Prelude

import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BSC


import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), ProtocolParameters (..), fromAlonzoExUnits,
                   protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)

import qualified Plutus.V1.Ledger.Api as Plutus
import           Plutus.V1.Ledger.Contexts (ScriptContext (..), ScriptPurpose (..), TxInfo (..),
                   TxOutRef (..))

import           Cardano.CLI.Shelley.Run.Read

readScript :: FilePath -> IO (Script PlutusScriptV1)
readScript fp = do
  res <- runExceptT $ readFileScriptInAnyLang fp
  case res of
    Left err -> do
      print err
      error $ show err
    Right (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) script) -> return script
    Right _otherScript ->
      error "Wrong script version."

toScriptHash :: String -> Hash ScriptData
toScriptHash str =
  case deserialiseFromRawBytesHex (AsHash AsScriptData) (BSC.pack str) of
    Right x -> x
    Left e -> error $ "Invalid datum hash: " ++ displayError e

preExecuteScript ::
     ProtocolParameters
  -> Script PlutusScriptV1
  -> HashableScriptData
  -> HashableScriptData
  -> Either String ExecutionUnits
preExecuteScript protocolParameters (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer = do
  costModel <- case Map.lookup (AnyPlutusScriptVersion PlutusScriptV1) (protocolParamCostModels protocolParameters) of
    Just (CostModel x) -> Right x
    Nothing -> Left "costModel unavailable"
  evaluationContext <- case Plutus.mkEvaluationContext costModel of
    Right x  -> Right x
    Left err -> Left $ "evaluationContext unavailable: " <> show err
  let
    apiVersion = protocolParamProtocolVersion protocolParameters
    protocolVersion = Plutus.ProtocolVersion (fromIntegral $ fst apiVersion) (fromIntegral $ snd apiVersion)
    (_logout, res) = Plutus.evaluateScriptCounting protocolVersion Plutus.Verbose evaluationContext script
                              [ toPlutusData datum
                              , toPlutusData redeemer
                              , Plutus.toData dummyContext ]
  case res of
     Left err -> Left $ show err
     Right exBudget -> case exBudgetToExUnits exBudget of
       Just x -> Right $ fromAlonzoExUnits x
       Nothing -> Left "exBudgetToExUnits exBudget == Nothing"
  where
    dummyContext :: ScriptContext
    dummyContext = ScriptContext dummyTxInfo (Spending dummyOutRef)

    dummyOutRef :: TxOutRef
    dummyOutRef = TxOutRef (Plutus.TxId "") 0
    dummyTxInfo :: TxInfo
    dummyTxInfo = TxInfo
      { txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = mempty
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = Plutus.always
      , txInfoSignatories = []
      , txInfoData = []
      , txInfoId = Plutus.TxId ""
      }
