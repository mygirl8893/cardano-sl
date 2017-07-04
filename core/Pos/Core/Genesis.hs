module Pos.Core.Genesis
       (
       -- * /genesis-core.bin/
         StakeDistribution(..)
       , GenesisCoreData(..)
       , compileGenCoreData
       -- ** Derived data
       , genesisAddresses
       , genesisStakeDistribution
       , genesisStakesProdMod

       -- * Constants
       , genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys
       , genesisDevHdwSecretKeys

       -- * Utils
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey
       , getTotalStake
       ) where

import           Universum

import           Data.Default            (def)
import qualified Data.Text               as T
import           Formatting              (int, sformat, (%))

import           Pos.Binary.Crypto       ()
import           Pos.Core.Address        (makePubKeyAddress)
import           Pos.Core.Constants      (genesisKeysN, isDevelopment)
import           Pos.Core.Genesis.Parser (compileGenCoreData)
import           Pos.Core.Genesis.Types  (GenesisCoreData (..), StakeDistribution (..),
                                          getTotalStake)
import           Pos.Core.Types          (Address, StakesMap)
import           Pos.Crypto.SafeSigning  (EncryptedSecretKey, emptyPassphrase,
                                          safeDeterministicKeyGen)
import           Pos.Crypto.Signing      (PublicKey, SecretKey, deterministicKeyGen)

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | List of pairs from 'SecretKey' with corresponding 'PublicKey'.
genesisDevKeyPairs :: [(PublicKey, SecretKey)]
genesisDevKeyPairs = map generateGenesisKeyPair [0 .. genesisKeysN - 1]

-- | List of 'PublicKey's in genesis.
genesisDevPublicKeys :: [PublicKey]
genesisDevPublicKeys = map fst genesisDevKeyPairs

-- | List of 'SecretKey's in genesis.
genesisDevSecretKeys :: [SecretKey]
genesisDevSecretKeys = map snd genesisDevKeyPairs

-- | List of 'SecretKey's in genesis for HD wallets.
genesisDevHdwSecretKeys :: [EncryptedSecretKey]
genesisDevHdwSecretKeys =
    map generateHdwGenesisSecretKey [0 .. genesisKeysN - 1]

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

generateGenesisKeyPair :: Int -> (PublicKey, SecretKey)
generateGenesisKeyPair =
    deterministicKeyGen .
    encodeUtf8 .
    T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

generateHdwGenesisSecretKey :: Int -> EncryptedSecretKey
generateHdwGenesisSecretKey =
    snd .
    flip safeDeterministicKeyGen emptyPassphrase .
    encodeUtf8 .
    T.take 32 . sformat ("My 32-byte hdw seed #" %int % "                  ")

----------------------------------------------------------------------------
-- GenesisCore-derived data
----------------------------------------------------------------------------

-- | List of addresses in genesis. See 'genesisPublicKeys'.
genesisAddresses :: [Address]
genesisAddresses
    | isDevelopment = map makePubKeyAddress genesisDevPublicKeys
    | otherwise     = gcdAddresses compileGenCoreData

genesisStakeDistribution :: StakeDistribution
genesisStakeDistribution
    | isDevelopment = def
    | otherwise     = gcdDistribution compileGenCoreData

-- | Genesis stakes which should be used only in production mode.
genesisStakesProdMod :: StakesMap
genesisStakesProdMod = gcdBootstrapBalances compileGenCoreData
