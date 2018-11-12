{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, DerivingStrategies, FlexibleContexts, GeneralizedNewtypeDeriving, KindSignatures, StrictData, TypeApplications, TypeFamilies, ScopedTypeVariables, OverloadedStrings, UndecidableInstances #-}

module Main where

import Prelude hiding (Word)

import qualified Data.Vector.Sized as Vector
import Data.Vector.Sized (Vector)
import Data.Word (Word8)
import Data.Semilattice.Lower
import GHC.TypeLits
import Data.Finite
import Data.Text.Prettyprint.Doc
import Data.Proxy
import Data.IndexedListLiterals

-- Signs are positive or negative.

data Sign = Pos | Neg deriving Eq

instance Pretty Sign where
  pretty Pos = "+"
  pretty Neg = "-"

-- Concrete values (bits and bytes) can be promoted
-- to a Haskell integer.

class Concrete a where
  concrete :: a -> Integer

-- A binary digit is 0 or 1

data Bin = O | I deriving (Eq, Ord, Enum, Bounded, Lower)

instance Pretty Bin where
  pretty O = "0"
  pretty I = "1"

instance Concrete Bin where
  concrete O = 0
  concrete I = 1

-- A decimal digit is 0..99

newtype Dec = Dec (Finite 100)
  deriving newtype (Eq, Ord, Show, Enum, Bounded)

instance Concrete Dec where
  concrete (Dec i) = getFinite i

instance Lower Dec where
  lowerBound = Dec (finite 0)

-- Bin and Dec have associated bases and MIX widths.

type family Base a :: Nat where
  Base Bin = 2
  Base Dec = 10

type family Width a :: Nat where
  Width Bin = 6
  Width Dec = 2

-- There's no way to specify a custom KnownNat instance for Width,
-- even though these are known statically, so let's use this constraint
-- synonym to hide that particular wart.

type Zeroed a = (KnownNat (Width a), Lower a)

-- Bytes are a vector of 2 decimals or 6 binary digits.
newtype Byte a = Byte { unByte :: Vector (Width a) a }

instance Zeroed a => Lower (Byte a) where
  lowerBound = Byte (Vector.replicate lowerBound)

-- Helper for making a Byte out of a tuple of Dec or Bin values.
-- LSB comes first. Note that this will fail if you provide
-- anything but 2 Dec values or 6 Byte values.
makeByte :: forall input a. (IndexedListLiterals input (Width a) a, KnownNat (Width a)) => input -> Byte a
makeByte = Byte . Vector.fromTuple

-- We can extract a Haskell Integer from a Byte if we know what
-- base the contents of the byte are in.
instance (KnownNat (Base a), Concrete a) => Concrete (Byte a) where
  concrete (Byte bs) = Vector.ifoldr' go 0 bs where
    go idx val acc = acc + (concrete val * (natVal (Proxy @(Base a)) ^ getFinite idx))

-- Words have 5 bytes and a sign
data Word a = Word
  { wbytes :: Vector 5 (Byte a)
  , wsign  :: Sign
  }

-- Index registers have 2 bytes and a sign.
data Index a = Index
  { ibytes :: Vector 2 (Byte a)
  , isign  :: Sign
  }

-- A jump register has 2 bytes and is treated as having a positive sign.
newtype Jump a = Jump
  { jbytes :: Vector 2 (Byte a)
  }

instance Zeroed a => Lower (Index a) where
  lowerBound = Index (Vector.replicate lowerBound) Pos

instance Zeroed a => Lower (Word a) where
  lowerBound = Word (Vector.replicate lowerBound) Pos

instance Zeroed a => Lower (Jump a) where
  lowerBound = Jump (Vector.replicate lowerBound)

-- Helper to give us a general accessor to the bytes of a word/index/jump.
type family Count (f :: * -> *) :: Nat where
  Count Word   = 5
  Count Index  = 2
  Count Jump   = 2

class Bytes (f :: * -> *) where
  bytes :: f a -> Vector (Count f) (Byte a)

instance Bytes Word  where bytes = wbytes
instance Bytes Index where bytes = ibytes
instance Bytes Jump  where bytes = jbytes

-- Same, but for signs.
class Signed a where sign :: a -> Sign

instance Signed (Word a) where sign  = wsign
instance Signed (Index a) where sign = isign
instance Signed (Jump a) where sign  = const Pos -- predefined

-- 2 general-purpose registers, six index registers, and a jump
-- register for the program counter.
data Registers a = Registers
  { rA  :: Word a
  , rX  :: Word a
  , rl1 :: Index a
  , rl2 :: Index a
  , rl3 :: Index a
  , rl4 :: Index a
  , rl5 :: Index a
  , rl6 :: Index a
  , rJ  :: Jump a
  }

instance Zeroed a => Lower (Registers a) where
  lowerBound = Registers lowerBound
                         lowerBound
                         lowerBound
                         lowerBound
                         lowerBound
                         lowerBound
                         lowerBound
                         lowerBound
                         lowerBound

-- 4000 bytes in memory.
newtype Memory a = Memory
  { contents :: Vector 4000 (Byte a)
  }

instance Zeroed a => Lower (Memory a) where
  lowerBound = Memory (Vector.replicate lowerBound)

-- The MIX CPU has memory, registers, and flag bits for overflow/comparison result
data MIX a = MIX
  { memory     :: Memory a
  , registers  :: Registers a
  , overflow   :: Bool
  , comparison :: Ordering
  }

instance Zeroed a => Lower (MIX a) where
  lowerBound = MIX lowerBound lowerBound False EQ

nth :: forall n a. Finite n -> Vector n a -> a
nth = flip Vector.index

main :: IO ()
main = do
  let zero = lowerBound :: MIX Bin

  let decB = makeByte (Dec 2, Dec 20) -- 202
  print . concrete $ decB

  let binB = makeByte (I, O, I, O, O, O) -- 5
  print . concrete $ binB

  print . concrete . nth 2 . bytes . rA . registers $ zero
  putStrLn "hello world"
