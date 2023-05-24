{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Cardano.Benchmarking.Script.Env
Description : State type for 'ActionM' monad stack and its accessors.

The 'Env' type is the ADT for the state component of the 'ActionM'
monad stack. Its actual definition isn't exported in part because of a
transition from an earlier very generic and polymorphic definition.
In a number of respects, this module covers more of the 'ActionM'
like 'runActionM' and 'liftTxGenError', but the only significant
structure is 'Env' for state. The accessors could likely be removed
in favour of just using the record syntax to trim a few lines of
code at the cost of exposing the structure's internals. Some of the
naming related to the fact that "Cardano.Benchmarking.Script.Action"
ran into circular dependency issues during the above transition.
 -}
module Cardano.Benchmarking.Script.Env (
        ActionM
        , Error(..)
        , runActionM
        , runActionMEnv
        , liftToAction
        , liftTxGenError
        , liftIOSafe
        , withTxGenError
        , askIOManager
        , traceDebug
        , traceError
        , traceBenchTxSubmit
        , getBenchTracers
        , setBenchTracers
        , getEnvGenesis
        , setEnvGenesis
        , getEnvKeys
        , setEnvKeys
        , getEnvNetworkId
        , setEnvNetworkId
        , getEnvProtocol
        , setEnvProtocol
        , getProtoParamMode
        , setProtoParamMode
        , getEnvSocketPath
        , setEnvSocketPath
        , getEnvThreads
        , setEnvThreads
        , getEnvWallets
        , setEnvWallets
        , getEnvSummary
        , setEnvSummary
) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict (RWST)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import           "contra-tracer" Control.Tracer (traceWith)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Prelude

import           Cardano.Api (File (..), SocketPath)

import           Cardano.Benchmarking.GeneratorTx
import qualified Cardano.Benchmarking.LogTypes as Tracer
import           Cardano.Benchmarking.OuroborosImports (NetworkId, PaymentKey, ShelleyGenesis,
                   SigningKey)
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Wallet
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.TxGenerator.PlutusContext (PlutusBudgetSummary)
import           Cardano.TxGenerator.Types (TxGenError (..))


-- | The 'Env' type represents the state maintained while executing
-- a series of actions. The 'Maybe' types are largely to represent
-- as-of-yet unset values.
data Env = Env { -- | 'Cardano.Api.ProtocolParameters' is ultimately
                 -- wrapped by 'ProtocolParameterMode' which itself is
                 -- a sort of custom 'Maybe'.
                 protoParams :: Maybe ProtocolParameterMode
               , benchTracers :: Maybe Tracer.BenchTracers
               , envGenesis :: Maybe (ShelleyGenesis StandardCrypto)
               , envProtocol :: Maybe SomeConsensusProtocol
               , envNetworkId :: Maybe NetworkId
               , envSocketPath :: Maybe FilePath
               , envKeys :: Map String (SigningKey PaymentKey)
               , envThreads :: Map String AsyncBenchmarkControl
               , envWallets :: Map String WalletRef
               , envSummary :: Maybe PlutusBudgetSummary
               }

emptyEnv :: Env
emptyEnv = Env { protoParams = Nothing
               , benchTracers = Nothing
               , envGenesis = Nothing
               , envKeys = Map.empty
               , envProtocol = Nothing
               , envNetworkId = Nothing
               , envSocketPath = Nothing
               , envThreads = Map.empty
               , envWallets = Map.empty
               , envSummary = Nothing
               }

type ActionM a = ExceptT Error (RWST IOManager () Env IO) a

runActionM :: ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionM = runActionMEnv emptyEnv

runActionMEnv :: Env -> ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionMEnv env action iom = RWS.runRWST (runExceptT action) iom env

data Error where
  TxGenError  :: !TxGenError -> Error
  UserError   :: !String     -> Error
  WalletError :: !String     -> Error

deriving instance Show Error

withTxGenError :: Monad m => ExceptT TxGenError m a -> ExceptT Error m a
withTxGenError = withExceptT Cardano.Benchmarking.Script.Env.TxGenError

liftToAction :: IO (Either TxGenError a) -> ActionM a
liftToAction = withTxGenError . ExceptT . liftIO

liftTxGenError :: TxGenError -> ActionM a
liftTxGenError = throwE . Cardano.Benchmarking.Script.Env.TxGenError

liftIOSafe :: IO (Either TxGenError a) -> ActionM a
liftIOSafe a = liftIO a >>= either liftTxGenError pure

askIOManager :: ActionM IOManager
askIOManager = lift RWS.ask

modifyEnv :: (Env -> Env) -> ActionM ()
modifyEnv = lift . RWS.modify

setProtoParamMode :: ProtocolParameterMode -> ActionM ()
setProtoParamMode val = modifyEnv (\e -> e { protoParams = pure val })

setBenchTracers :: Tracer.BenchTracers -> ActionM ()
setBenchTracers val = modifyEnv (\e -> e { benchTracers = pure val })

setEnvGenesis :: ShelleyGenesis StandardCrypto -> ActionM ()
setEnvGenesis val = modifyEnv (\e -> e { envGenesis = pure val })

setEnvKeys :: String -> SigningKey PaymentKey -> ActionM ()
setEnvKeys key val = modifyEnv (\e -> e { envKeys = Map.insert key val (envKeys e) })

setEnvProtocol :: SomeConsensusProtocol -> ActionM ()
setEnvProtocol val = modifyEnv (\e -> e { envProtocol = pure val })

setEnvNetworkId :: NetworkId -> ActionM ()
setEnvNetworkId val = modifyEnv (\e -> e { envNetworkId = pure val })

setEnvSocketPath :: FilePath -> ActionM ()
setEnvSocketPath val = modifyEnv (\e -> e { envSocketPath = pure val })

setEnvThreads :: String -> AsyncBenchmarkControl -> ActionM ()
setEnvThreads key val = modifyEnv (\e -> e { envThreads = Map.insert key val (envThreads e) })

setEnvWallets :: String -> WalletRef -> ActionM ()
setEnvWallets key val = modifyEnv (\e -> e { envWallets = Map.insert key val (envWallets e) })

setEnvSummary :: PlutusBudgetSummary -> ActionM ()
setEnvSummary val = modifyEnv (\e -> e { envSummary = pure val })

getEnvVal :: (Env -> Maybe t) -> String -> ActionM t
getEnvVal acc s = do
  lift (RWS.gets acc) >>= \case
    Just x -> return x
    Nothing -> throwE . UserError $ "Unset " ++ s

getEnvMap :: (Env -> Map String t) -> String -> ActionM t
getEnvMap acc key = do
  m <- lift $ RWS.gets acc
  case Map.lookup key m of
    Just x -> return x
    Nothing -> throwE . UserError $ "Lookup of " ++ key ++ " failed"

getProtoParamMode :: ActionM ProtocolParameterMode
getProtoParamMode = getEnvVal protoParams "ProtocolParameterMode"

getBenchTracers :: ActionM Tracer.BenchTracers
getBenchTracers = getEnvVal benchTracers "BenchTracers"

getEnvGenesis :: ActionM (ShelleyGenesis StandardCrypto)
getEnvGenesis = getEnvVal envGenesis "Genesis"

getEnvKeys :: String -> ActionM (SigningKey PaymentKey)
getEnvKeys = getEnvMap envKeys

getEnvNetworkId :: ActionM NetworkId
getEnvNetworkId = getEnvVal envNetworkId "Genesis"

getEnvProtocol :: ActionM SomeConsensusProtocol
getEnvProtocol = getEnvVal envProtocol "Protocol"

getEnvSocketPath :: ActionM SocketPath
getEnvSocketPath = File <$> getEnvVal envSocketPath "SocketPath"

getEnvThreads :: String -> ActionM AsyncBenchmarkControl
getEnvThreads = getEnvMap envThreads

getEnvWallets :: String -> ActionM WalletRef
getEnvWallets = getEnvMap envWallets

getEnvSummary :: ActionM (Maybe PlutusBudgetSummary)
getEnvSummary = lift (RWS.gets envSummary)

traceBenchTxSubmit :: (forall txId. x -> Tracer.TraceBenchTxSubmit txId) -> x -> ActionM ()
traceBenchTxSubmit tag msg = do
  tracers  <- getBenchTracers
  liftIO $ traceWith (Tracer.btTxSubmit_ tracers) $ tag msg

traceError :: String -> ActionM ()
traceError = traceBenchTxSubmit (Tracer.TraceBenchTxSubError . Text.pack)

traceDebug :: String -> ActionM ()
traceDebug = traceBenchTxSubmit Tracer.TraceBenchTxSubDebug
