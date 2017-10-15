module Attribute where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Get

data Attribute =
    Attribute {
        attribute_name_index :: Word16,
        info :: B.ByteString
    } |
    ConstantValue {
    } |
    Code {
    } |
    StackMapTable {
    } |
    Exceptions {
    } |
    BootstrapMethods {
    }
    deriving Show

getAttribute = do
    attribute_name_index <- getWord16be
    attribute_length <- getWord32be
    info <- getByteString (fromIntegral attribute_length)
    return (Attribute attribute_name_index info)

getAttributes 0 = return []
getAttributes n = do
    attr <- getAttribute
    rest <- getAttributes (n - 1)
    return (attr:rest)
