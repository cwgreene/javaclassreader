module Types where

import Data.Word

-- Tags
type Tag = Word8
type ReferenceKind = Word8

-- Constant Pool References
type NameIx = Word16
type ClassIx = Word16
type NameAndTypeIx = Word16
type StringIx = Word16
type DescriptorIx = Word16
type ReferenceIx = Word16
type BootstrapMethodAttrIx = Word16

-- Word 32s
type FloatWord = Word32
type IntWord = Word32
type HighIntWord = Word32
type LoIntWord = Word32
type HighFloatWord = Word32
type LoFloatWord = Word32
