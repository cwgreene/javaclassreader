module ConstantPool where

import Types
import Attribute

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.UTF8 as UTF8

import Debug.Trace

data Constant = ConstClass NameIx |
    ConstFieldRef ClassIx NameAndTypeIx |
    ConstMethodRef ClassIx NameAndTypeIx |
    ConstInterfaceMethodRef ClassIx NameAndTypeIx |
    ConstString StringIx | ConstInt IntWord | ConstFloat FloatWord |
    ConstLong HighIntWord LoIntWord | ConstDouble HighFloatWord LoFloatWord |
    ConstUtf8 String |
    ConstNameAndType NameIx DescriptorIx |
    ConstMethodHandle ReferenceKind ReferenceIx |
    ConstMethodType DescriptorIx |
    ConstInvokeDynamic BootstrapMethodAttrIx NameAndTypeIx |
    ConstUnused deriving Show

cCONSTANT_Class = 7
cCONSTANT_Fieldref = 9
cCONSTANT_Methodref = 10
cCONSTANT_InterfaceMethodref = 11
cCONSTANT_String = 8
cCONSTANT_Integer = 3
cCONSTANT_Float = 4
cCONSTANT_Long = 5
cCONSTANT_Double = 6
cCONSTANT_NameAndType = 12
cCONSTANT_Utf8 = 1
cCONSTANT_MethodHandle = 15
cCONSTANT_MethodType = 16
cCONSTANT_InvokeDynamic = 18

getRef :: (Word16 -> Word16 -> Constant) -> Get Constant
getRef constructor = do
    class_index <- getWord16be
    name_and_type_index <- getWord16be
    return $ constructor class_index name_and_type_index

getShortNum constructor = do
    bytes <- getWord32be
    return (constructor bytes)

getLargeNum :: (Word32 -> Word32 -> Constant) -> Get Constant
getLargeNum constructor = do
    high_bytes <- getWord32be
    low_bytes <- getWord32be
    return (constructor high_bytes low_bytes)

getConstantPoolType :: Word16 -> Get Constant
getConstantPoolType 7 = do
    name_index <- getWord16be
    return (ConstClass name_index)
getConstantPoolType 9 = getRef ConstFieldRef
getConstantPoolType 10 = getRef $ ConstMethodRef
getConstantPoolType 11 =
    getRef ConstInterfaceMethodRef
getConstantPoolType 8 = do
    string_index <- getWord16be
    return (ConstString string_index)
getConstantPoolType 3 = getShortNum ConstInt
getConstantPoolType 4 = getShortNum ConstFloat
getConstantPoolType 5 = getLargeNum ConstLong
getConstantPoolType 6 = getLargeNum ConstDouble
getConstantPoolType 12 = do
    name_index <- getWord16be
    descriptor_index <- getWord16be
    return (ConstNameAndType name_index descriptor_index)
getConstantPoolType 1 = do
    length <- getWord16be
    bytes <- getByteString (fromIntegral length)
    return (ConstUtf8 $ UTF8.toString bytes)
getConstantPoolType 15 = do
    reference_kind <- getWord8
    reference_index <- getWord16be
    return $ ConstMethodHandle reference_kind reference_index
getConstantPoolType 16 = do
    descriptor_index <- getWord16be
    return $ ConstMethodType descriptor_index
getConstantPoolType 18 = do
    bootstrap_method_attr_index <- getWord16be
    name_and_type_index <- getWord16be
    return $ ConstInvokeDynamic bootstrap_method_attr_index name_and_type_index

remainingPool 5 x = x - 2
remainingPool 6 x = x - 2
remainingPool _ x = x - 1

getConstantPool :: Int -> Get [Constant]
getConstantPool 1 = return []
getConstantPool x = do
    tag <- getWord8
    info <- getConstantPoolType (fromIntegral tag)
    next <- getConstantPool (remainingPool tag x)
    if (tag == cCONSTANT_Double || tag == cCONSTANT_Long) then
        return $ info:(ConstUnused:next)
    else
        return $ info:next

getInterfaces 0 = return []
getInterfaces x = do
    interfaceIndex <- getWord16be
    rest <- getInterfaces (x - 1)
    return $ interfaceIndex:rest
