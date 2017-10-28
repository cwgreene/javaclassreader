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

import Numeric
import System.Environment

import Debug.Trace

import ConstantPool

data Hex32 = Hex32 Word32

instance Show Hex32 where
    show (Hex32 x) = "0x" ++ (showHex x "")

data ClassFile = ClassFile {
        magic :: Hex32, -- CAFE BABE

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
    constant_pool <- getConstantPool (fromIntegral constant_pool_count) (fromIntegral constant_pool_count)
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
    return (ClassFile (Hex32 magic) minor major
        constant_pool
        access_flags
        this_class
        super_class
        interfaces
        fields
        methods
        attributes)

readFiles [] = return ()
readFiles (f:files) = do
    input <- BL.readFile f
    pPrint $ runGet readJCF input
    readFiles files

main = do
    filePaths <- getArgs
    readFiles filePaths

