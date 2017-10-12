import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.UTF8 as UTF8

data Constant = ConstClass Int Int |
    ConstFieldRef Int Int | ConstMethodRef Int Int | ConstInterfaceMethodRef Int Int |
    ConstString Int | ConstInt Int | ConstFloat Int | ConstLong Int Int | ConstDouble Int Int |
    ConstUtf8 String | ConstNameAndType Int Int | ConstMethodHandle Int Int | ConstInvokeDynamic Int Int

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

getConstantPoolType :: Int -> Get Constant
getConstantPoolType cCONSTANT_Class = do
    name_index <- getWord16be
    return (ConstClass name_index)

getRef constructor = do
    class_index <- getWord8
    name_and_type_index <- getWord8
    return (constructor class_index name_and_type_index)

getConstantPoolType cCONSTANT_Fieldref = getRef ConstFieldRef
getConstantPoolType cCONSTANT_Methodref = getRef ConstMethodRef
getConstantPoolType cCONSTANT_InterfaceMethodref =
    getRef ConstInterfaceMethodRef

getConstantPoolType cCONSTANT_String = do
    string_index <- getWord16be
    return (ConstString string_index)

getShortNum constructor = do
    bytes <- getWord32be
    return (constructor bytes)

getConstantPoolType cCONSTANT_Integer = getShortNum ConstInt
getConstantPoolType cCONSTANT_Float = getShortNum ConstFloat

getLargeNum :: Int -> Int -> Constant -> Get Constant
getLargeNum constructor = do
    high_bytes <- getWord32be
    low_bytes <- getWord32be
    return (constructor high_bytes low_bytes)

getConstantPoolType cCONSTANT_Long = getLargeNum ConstLong
getConstantPoolType cCONSTANT_Double = getLargeNum ConstDouble

getConstantPoolType cCONSTANT_NameAndType = do
    name_index <- getWord16be
    descriptor_index <- getWord16be
    return (ConstNameAndType name_index descriptor_index)

getConstantPoolType cCONSTANT_Utf8 = do
    length <- getWord16be
    bytes <- getByteString length
    return (ConstUtf8 $ UTF8.toString bytes)

getConstantPoolType cCONSTANT_MethodHandle = do
    reference_kind <- getWord8
    reference_index <- getWord16be
    return $ ConstMethodHandle $ reference_kind reference_index

getConstantPoolType cCONSTANT_InvokeDynamic = do
    bootstrap_method_attr_index <- getWord16be
    name_and_type_index <- getWord16be
    return $ ConstInvokeDynamic $ bootstrap_method_attr_index name_and_type_index


getConstantPool 1 = []
getConstantPool x = do
    tag <- getWord8
    info <- getConstantPoolType tag
    return (info:(getConstantPool (x-1)))

readJCF = do
    magic <- getWord32be
    minor <- getWord16be
    major <- getWord16be
    constant_pool_count <- getWord16be
    constant_pool <- getConstantPool constant_pool_count
    return constant_pool
{-    access_flags <- getWord16be
    this_class <- getWord16be
    super_class <- getWord16be
    interfaces_count <- getWord16be
    interfaces <- getInterfacesCount interfaces_count
    fields_count <- getWord16be
    fields <- getFields fields_count
    methods_count <- getWord16be
    methods <- getMethods methods_count
    attributes_count <- getWord16be
    attributes <- attributes attributes_count -}

main = do
    input <- BL.getContents
    print $ runGet readJCF input

