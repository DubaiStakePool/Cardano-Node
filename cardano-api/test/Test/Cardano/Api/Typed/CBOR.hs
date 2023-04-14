{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Api.Typed.CBOR
  ( tests
  ) where

import           Cardano.Api

import           Cardano.Api.Shelley (AsType(..))
import           Data.Proxy (Proxy (..))
import           Data.String (IsString (..))
import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Gen.Cardano.Api.Typed
import           Test.Hedgehog.Roundtrip.CBOR (roundtrip_CBOR)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

{- HLINT ignore "Use camelCase" -}

-- TODO: Need to add PaymentExtendedKey roundtrip tests however
-- we can't derive an Eq instance for Crypto.HD.XPrv

prop_roundtrip_txbody_CBOR :: Property
prop_roundtrip_txbody_CBOR = H.property $ do
  AnyCardanoEra era <- H.forAll $ Gen.element [minBound..AnyCardanoEra BabbageEra]
  x <- H.forAll $ makeSignedTransaction [] <$> genTxBody era
  H.tripping x serialiseTxLedgerCddl (deserialiseTxLedgerCddl era)

test_roundtrip_tx_CBOR :: [TestTree]
test_roundtrip_tx_CBOR =
  [ testPropertyNamed (show era) (fromString (show era)) $ roundtrip_CBOR (proxyToAsType Proxy) (genTx era)
  | AnyCardanoEra era <- [minBound..(AnyCardanoEra BabbageEra)]
  ]

prop_roundtrip_witness_byron_CBOR :: Property
prop_roundtrip_witness_byron_CBOR =
  roundtrip_CBOR (AsKeyWitness AsByronEra) genByronKeyWitness

prop_roundtrip_witness_shelley_CBOR :: Property
prop_roundtrip_witness_shelley_CBOR =
  roundtrip_CBOR (AsKeyWitness AsShelleyEra) (genShelleyWitness ShelleyEra)

prop_roundtrip_witness_allegra_CBOR :: Property
prop_roundtrip_witness_allegra_CBOR =
  roundtrip_CBOR (AsKeyWitness AsAllegraEra) (genShelleyWitness AllegraEra)

prop_roundtrip_witness_mary_CBOR :: Property
prop_roundtrip_witness_mary_CBOR =
  roundtrip_CBOR (AsKeyWitness AsMaryEra) (genShelleyWitness MaryEra)

prop_roundtrip_witness_alonzo_CBOR :: Property
prop_roundtrip_witness_alonzo_CBOR =
  roundtrip_CBOR (AsKeyWitness AsAlonzoEra) (genShelleyWitness AlonzoEra)

prop_roundtrip_operational_certificate_CBOR :: Property
prop_roundtrip_operational_certificate_CBOR =
  roundtrip_CBOR AsOperationalCertificate genOperationalCertificate

prop_roundtrip_operational_certificate_issue_counter_CBOR :: Property
prop_roundtrip_operational_certificate_issue_counter_CBOR =
  roundtrip_CBOR AsOperationalCertificateIssueCounter genOperationalCertificateIssueCounter

prop_roundtrip_verification_key_byron_CBOR :: Property
prop_roundtrip_verification_key_byron_CBOR =
  roundtrip_CBOR (AsVerificationKey AsByronKey) (genVerificationKey AsByronKey)

prop_roundtrip_signing_key_byron_CBOR :: Property
prop_roundtrip_signing_key_byron_CBOR =
  roundtrip_CBOR (AsSigningKey AsByronKey) (genSigningKey AsByronKey)

prop_roundtrip_verification_key_payment_CBOR :: Property
prop_roundtrip_verification_key_payment_CBOR =
  roundtrip_CBOR (AsVerificationKey AsPaymentKey) (genVerificationKey AsPaymentKey)

prop_roundtrip_signing_key_payment_CBOR :: Property
prop_roundtrip_signing_key_payment_CBOR =
  roundtrip_CBOR (AsSigningKey AsPaymentKey) (genSigningKey AsPaymentKey)

prop_roundtrip_verification_key_stake_CBOR :: Property
prop_roundtrip_verification_key_stake_CBOR =
  roundtrip_CBOR (AsVerificationKey AsStakeKey) (genVerificationKey AsStakeKey)

prop_roundtrip_signing_key_stake_CBOR :: Property
prop_roundtrip_signing_key_stake_CBOR =
  roundtrip_CBOR (AsSigningKey AsStakeKey) (genSigningKey AsStakeKey)

prop_roundtrip_verification_key_genesis_CBOR :: Property
prop_roundtrip_verification_key_genesis_CBOR =
  roundtrip_CBOR (AsVerificationKey AsGenesisKey) (genVerificationKey AsGenesisKey)

prop_roundtrip_signing_key_genesis_CBOR :: Property
prop_roundtrip_signing_key_genesis_CBOR =
  roundtrip_CBOR (AsSigningKey AsGenesisKey) (genSigningKey AsGenesisKey)

prop_roundtrip_verification_key_genesis_delegate_CBOR :: Property
prop_roundtrip_verification_key_genesis_delegate_CBOR =
  roundtrip_CBOR (AsVerificationKey AsGenesisDelegateKey) (genVerificationKey AsGenesisDelegateKey)

prop_roundtrip_signing_key_genesis_delegate_CBOR :: Property
prop_roundtrip_signing_key_genesis_delegate_CBOR =
  roundtrip_CBOR (AsSigningKey AsGenesisDelegateKey) (genSigningKey AsGenesisDelegateKey)

prop_roundtrip_verification_key_stake_pool_CBOR :: Property
prop_roundtrip_verification_key_stake_pool_CBOR =
  roundtrip_CBOR (AsVerificationKey AsStakePoolKey) (genVerificationKey AsStakePoolKey)

prop_roundtrip_signing_key_stake_pool_CBOR :: Property
prop_roundtrip_signing_key_stake_pool_CBOR =
  roundtrip_CBOR (AsSigningKey AsStakePoolKey) (genSigningKey AsStakePoolKey)

prop_roundtrip_verification_key_vrf_CBOR :: Property
prop_roundtrip_verification_key_vrf_CBOR =
  roundtrip_CBOR (AsVerificationKey AsVrfKey) (genVerificationKey AsVrfKey)

prop_roundtrip_signing_key_vrf_CBOR :: Property
prop_roundtrip_signing_key_vrf_CBOR =
  roundtrip_CBOR (AsSigningKey AsVrfKey) (genSigningKey AsVrfKey)

prop_roundtrip_verification_key_kes_CBOR :: Property
prop_roundtrip_verification_key_kes_CBOR =
  roundtrip_CBOR (AsVerificationKey AsKesKey) (genVerificationKey AsKesKey)

prop_roundtrip_signing_key_kes_CBOR :: Property
prop_roundtrip_signing_key_kes_CBOR =
  roundtrip_CBOR (AsSigningKey AsKesKey) (genSigningKey AsKesKey)

prop_roundtrip_script_SimpleScriptV1_CBOR :: Property
prop_roundtrip_script_SimpleScriptV1_CBOR =
  roundtrip_CBOR (AsScript AsSimpleScript)
                 (genScript SimpleScriptLanguage)

prop_roundtrip_script_SimpleScriptV2_CBOR :: Property
prop_roundtrip_script_SimpleScriptV2_CBOR =
  roundtrip_CBOR (AsScript AsSimpleScript)
                 (genScript SimpleScriptLanguage)

prop_roundtrip_script_PlutusScriptV1_CBOR :: Property
prop_roundtrip_script_PlutusScriptV1_CBOR =
  roundtrip_CBOR (AsScript AsPlutusScriptV1)
                 (genScript (PlutusScriptLanguage PlutusScriptV1))

prop_roundtrip_script_PlutusScriptV2_CBOR :: Property
prop_roundtrip_script_PlutusScriptV2_CBOR =
  roundtrip_CBOR (AsScript AsPlutusScriptV2)
                 (genScript (PlutusScriptLanguage PlutusScriptV2))

prop_roundtrip_ScriptData_CBOR :: Property
prop_roundtrip_ScriptData_CBOR =
  roundtrip_CBOR AsHashableScriptData genHashableScriptData

prop_roundtrip_UpdateProposal_CBOR :: Property
prop_roundtrip_UpdateProposal_CBOR =
  roundtrip_CBOR AsUpdateProposal genUpdateProposal

prop_roundtrip_Tx_Cddl :: Property
prop_roundtrip_Tx_Cddl = H.property $ do
  AnyCardanoEra era <- H.forAll $ Gen.element [minBound..maxBound]
  x <- forAll $ genTx era
  H.tripping x serialiseTxLedgerCddl (deserialiseTxLedgerCddl era)

prop_roundtrip_TxWitness_Cddl :: Property
prop_roundtrip_TxWitness_Cddl = H.property $ do
  AnyShelleyBasedEra sbe <- H.forAll $ Gen.element [minBound..maxBound]
  x <- forAll $ genShelleyKeyWitness $ shelleyBasedToCardanoEra sbe
  tripping x (serialiseWitnessLedgerCddl sbe) (deserialiseWitnessLedgerCddl sbe)

prop_roundtrip_GovernancePoll_CBOR :: Property
prop_roundtrip_GovernancePoll_CBOR =
  roundtrip_CBOR AsGovernancePoll genGovernancePoll

prop_roundtrip_GovernancePollAnswer_CBOR :: Property
prop_roundtrip_GovernancePollAnswer_CBOR =
  roundtrip_CBOR AsGovernancePollAnswer genGovernancePollAnswer

prop_roundtrip_GovernancePollWitness_CBOR :: Property
prop_roundtrip_GovernancePollWitness_CBOR =
  roundtrip_CBOR AsGovernancePollWitness genGovernancePollWitness

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.CBOR"
  [ testPropertyNamed "roundtrip witness byron CBOR"                         "roundtrip witness byron CBOR"                         prop_roundtrip_witness_byron_CBOR
  , testPropertyNamed "roundtrip witness shelley CBOR"                       "roundtrip witness shelley CBOR"                       prop_roundtrip_witness_shelley_CBOR
  , testPropertyNamed "roundtrip witness allegra CBOR"                       "roundtrip witness allegra CBOR"                       prop_roundtrip_witness_allegra_CBOR
  , testPropertyNamed "roundtrip witness mary CBOR"                          "roundtrip witness mary CBOR"                          prop_roundtrip_witness_mary_CBOR
  , testPropertyNamed "roundtrip witness alonzo CBOR"                        "roundtrip witness alonzo CBOR"                        prop_roundtrip_witness_alonzo_CBOR
  , testPropertyNamed "roundtrip operational certificate CBOR"               "roundtrip operational certificate CBOR"               prop_roundtrip_operational_certificate_CBOR
  , testPropertyNamed "roundtrip operational certificate issue counter CBOR" "roundtrip operational certificate issue counter CBOR" prop_roundtrip_operational_certificate_issue_counter_CBOR
  , testPropertyNamed "roundtrip verification key byron CBOR"                "roundtrip verification key byron CBOR"                prop_roundtrip_verification_key_byron_CBOR
  , testPropertyNamed "roundtrip signing key byron CBOR"                     "roundtrip signing key byron CBOR"                     prop_roundtrip_signing_key_byron_CBOR
  , testPropertyNamed "roundtrip verification key payment CBOR"              "roundtrip verification key payment CBOR"              prop_roundtrip_verification_key_payment_CBOR
  , testPropertyNamed "roundtrip signing key payment CBOR"                   "roundtrip signing key payment CBOR"                   prop_roundtrip_signing_key_payment_CBOR
  , testPropertyNamed "roundtrip verification key stake CBOR"                "roundtrip verification key stake CBOR"                prop_roundtrip_verification_key_stake_CBOR
  , testPropertyNamed "roundtrip signing key stake CBOR"                     "roundtrip signing key stake CBOR"                     prop_roundtrip_signing_key_stake_CBOR
  , testPropertyNamed "roundtrip verification key genesis CBOR"              "roundtrip verification key genesis CBOR"              prop_roundtrip_verification_key_genesis_CBOR
  , testPropertyNamed "roundtrip signing key genesis CBOR"                   "roundtrip signing key genesis CBOR"                   prop_roundtrip_signing_key_genesis_CBOR
  , testPropertyNamed "roundtrip verification key genesis delegate CBOR"     "roundtrip verification key genesis delegate CBOR"     prop_roundtrip_verification_key_genesis_delegate_CBOR
  , testPropertyNamed "roundtrip signing key genesis delegate CBOR"          "roundtrip signing key genesis delegate CBOR"          prop_roundtrip_signing_key_genesis_delegate_CBOR
  , testPropertyNamed "roundtrip verification key stake pool CBOR"           "roundtrip verification key stake pool CBOR"           prop_roundtrip_verification_key_stake_pool_CBOR
  , testPropertyNamed "roundtrip signing key stake pool CBOR"                "roundtrip signing key stake pool CBOR"                prop_roundtrip_signing_key_stake_pool_CBOR
  , testPropertyNamed "roundtrip verification key vrf CBOR"                  "roundtrip verification key vrf CBOR"                  prop_roundtrip_verification_key_vrf_CBOR
  , testPropertyNamed "roundtrip signing key vrf CBOR"                       "roundtrip signing key vrf CBOR"                       prop_roundtrip_signing_key_vrf_CBOR
  , testPropertyNamed "roundtrip verification key kes CBOR"                  "roundtrip verification key kes CBOR"                  prop_roundtrip_verification_key_kes_CBOR
  , testPropertyNamed "roundtrip signing key kes CBOR"                       "roundtrip signing key kes CBOR"                       prop_roundtrip_signing_key_kes_CBOR
  , testPropertyNamed "roundtrip script SimpleScriptV1 CBOR"                 "roundtrip script SimpleScriptV1 CBOR"                 prop_roundtrip_script_SimpleScriptV1_CBOR
  , testPropertyNamed "roundtrip script SimpleScriptV2 CBOR"                 "roundtrip script SimpleScriptV2 CBOR"                 prop_roundtrip_script_SimpleScriptV2_CBOR
  , testPropertyNamed "roundtrip script PlutusScriptV1 CBOR"                 "roundtrip script PlutusScriptV1 CBOR"                 prop_roundtrip_script_PlutusScriptV1_CBOR
  , testPropertyNamed "roundtrip script PlutusScriptV2 CBOR"                 "roundtrip script PlutusScriptV2 CBOR"                 prop_roundtrip_script_PlutusScriptV2_CBOR
  , testPropertyNamed "roundtrip UpdateProposal CBOR"                        "roundtrip UpdateProposal CBOR"                        prop_roundtrip_UpdateProposal_CBOR
  , testPropertyNamed "roundtrip ScriptData CBOR"                            "roundtrip ScriptData CBOR"                            prop_roundtrip_ScriptData_CBOR
  , testPropertyNamed "roundtrip txbody CBOR"                                "roundtrip txbody CBOR"                                prop_roundtrip_txbody_CBOR
  , testPropertyNamed "roundtrip Tx Cddl"                                    "roundtrip Tx Cddl"                                    prop_roundtrip_Tx_Cddl
  , testPropertyNamed "roundtrip TxWitness Cddl"                             "roundtrip TxWitness Cddl"                             prop_roundtrip_TxWitness_Cddl
  , testPropertyNamed "roundtrip GovernancePoll CBOR"                        "roundtrip GovernancePoll CBOR"                        prop_roundtrip_GovernancePoll_CBOR
  , testPropertyNamed "roundtrip GovernancePollAnswer CBOR"                  "roundtrip GovernancePollAnswer CBOR"                  prop_roundtrip_GovernancePollAnswer_CBOR
  , testPropertyNamed "roundtrip GovernancePollWitness CBOR"                 "roundtrip GovernancePollWitness CBOR"                 prop_roundtrip_GovernancePollWitness_CBOR
  , testGroup "roundtrip tx CBOR"         test_roundtrip_tx_CBOR
  ]
