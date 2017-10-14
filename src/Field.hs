module Field where

import Attribute

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Get

data Field = Field {
        f_access_flags :: Word16,
        name_index :: Word16,
        descriptor_index :: Word16,
        attributes :: [Attribute]
    } deriving Show

getField = do
    access_flags <- getWord16be
    name_index <- getWord16be
    descriptor_index <- getWord16be
    attributes_count <- getWord16be
    attributes <- getAttributes (fromIntegral attributes_count)
    return (Field access_flags name_index descriptor_index attributes)

getFields 0 = return []
getFields n = do
    field <- getField
    rest <- getFields (n - 1)
    return (field:rest)
