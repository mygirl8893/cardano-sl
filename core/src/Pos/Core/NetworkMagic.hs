module Pos.Core.NetworkMagic
       ( NetworkMagic (..)
       , makeNetworkMagic
       ) where

import           Universum

import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import           Data.Serialize (getWord8, putWord8)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Crypto.Configuration (ProtocolMagic (..), RequiresNetworkMagic (..),
                                           getProtocolMagic)
import           Pos.Util.Util (cerealError)


--------------------------------------------------------------------------------
-- NetworkMagic
--------------------------------------------------------------------------------

-- mhueschen: although it makes no sense for an identifier to have a sign, we're
-- opting to maintain consistency with `ProtocolMagicId`, rather than risk subtle
-- conversion bugs.
data NetworkMagic
    = NetworkMainStage
    | NetworkTestnet !Int32
    deriving (Show, Eq, Ord, Generic)

instance NFData NetworkMagic

instance Buildable NetworkMagic where
    build NetworkMainStage  = "NetworkMainStage"
    build (NetworkTestnet n) = bprint ("NetworkTestnet ("%build%")") n

instance SafeCopy NetworkMagic where
    getCopy = contain $ getWord8 >>= \case
        0 -> pure NetworkMainStage
        1 -> NetworkTestnet <$> safeGet
        t -> cerealError $ "getCopy@NetworkMagic: couldn't read tag: " <> show t
    putCopy NetworkMainStage  = contain $ putWord8 0
    putCopy (NetworkTestnet x) = contain $ putWord8 1 >> safePut x

makeNetworkMagic :: ProtocolMagic -> NetworkMagic
makeNetworkMagic pm = case getRequiresNetworkMagic pm of
    RequiresNoMagic -> NetworkMainStage
    RequiresMagic    -> NetworkTestnet (getProtocolMagic pm)
