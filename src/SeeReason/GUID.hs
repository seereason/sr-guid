{-# language StandaloneDeriving #-}
module SeeReason.GUID ( GUID, GUIDConfig, GUIDGenerator, GUIDNode
                     , newGUIDGenerator
                     , defaultGUIDConfig
                     , nextGUID
                     )
 where

-- A wrapper for whatever GUID we end up using.
-- There are so many to choose from and it was hard to decide without more info.
-- Snowflake is nice because small, tested (by twitter), runs in client.

import Control.Monad.Trans
import Data.Data
import Data.Typeable
import Data.SafeCopy (base, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import Data.Serialize (Serialize(get, put))
import Data.Snowflake

-- use newtypes and make the instances clearer?

type GUID = Snowflake
type GUIDConfig = SnowflakeConfig
type GUIDGenerator = SnowflakeGen
type GUIDNode = Integer

newGUIDGenerator :: MonadIO m => GUIDConfig -> GUIDNode -> m GUIDGenerator
newGUIDGenerator c i = liftIO $ newSnowflakeGen c i

defaultGUIDConfig :: GUIDConfig
defaultGUIDConfig = defaultConfig

nextGUID :: MonadIO m => GUIDGenerator -> m GUID
nextGUID = liftIO . nextSnowflake

instance SafeCopy Snowflake where version = 1; kind=base
instance Serialize Snowflake where get = safeGet; put = safePut
instance SafeCopy SnowflakeConfig where version = 1; kind=base
instance Serialize SnowflakeConfig where get = safeGet; put = safePut

deriving instance Ord SnowflakeConfig

-- Data.UUID doesn't work well in the client, and isn't time sortable.
-- V4 doesn't work in the client.
--import Data.UUID (UUID)
--import Data.UUID.V1 (nextUUID)
-- import Data.UUID.V5 (generateNamed)

