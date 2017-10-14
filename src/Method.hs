module Method where

import Attribute

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Get

data Method = Method {
        m_access_flags :: Word16,
        m_name_index :: Word16,
        m_descriptor_index :: Word16,
        m_attributes :: [Attribute]
    } deriving Show

getMethod = do
    access_flags <- getWord16be
    name_index <- getWord16be
    descriptor_index <- getWord16be
    attribute_count <- getWord16be
    attributes <- getAttributes (fromIntegral attribute_count)
    return (Method access_flags name_index descriptor_index attributes)

getMethods 0 = return []
getMethods n = do
    method <- getMethod
    rest <- getMethods (n - 1)
    return (method:rest)
