module Main where

import ConstantPool
import Method
import Attribute
import Field

--import Text.Pretty.Simple
import Text.Show.Pretty

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.UTF8 as UTF8

import Debug.Trace

import ConstantPool

data ClassFile = ClassFile {
        magic :: Word32, -- CAFE BABE

        minor :: Word16,
        major :: Word16,

        constantPool :: [Constant],

        c_access_flags :: Word16,
        this_class :: Word16,
        super_class :: Word16,

        interfaces :: [Word16],

        fields :: [Field],

        methods :: [Method],
        
        cl_attributes :: [Attribute] 
    } deriving Show

readJCF = do
    magic <- getWord32be
    minor <- getWord16be
    major <- getWord16be
    constant_pool_count <- getWord16be
    constant_pool <- getConstantPool (fromIntegral constant_pool_count)
    access_flags <- getWord16be
    this_class <- getWord16be
    super_class <- getWord16be
    interfaces_count <- getWord16be
    interfaces <- getInterfaces (fromIntegral interfaces_count)
    fields_count <- getWord16be
    fields <- getFields (fromIntegral fields_count)
    methods_count <- getWord16be
    methods <- getMethods (fromIntegral methods_count)
    attributes_count <- getWord16be
    attributes <- getAttributes (fromIntegral attributes_count)
    return (ClassFile magic minor major
        constant_pool
        access_flags
        this_class
        super_class
        interfaces
        fields
        methods
        attributes)

main = do
    input <- BL.getContents
    pPrint $ runGet readJCF input

