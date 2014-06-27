{-# LANGUAGE Rank2Types, StandaloneDeriving,
    ForeignFunctionInterface, RecordWildCards #-}

import Data.Word
import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import qualified Data.Serialize.Put as S

foreign import ccall "Execute.h buildAndExecute" c_buildAndExecute
  :: Ptr a -> Int -> Ptr b -> IO (Ptr c)

executeVector :: VS.Vector Word8 -> Ptr a -> IO (Ptr b)
executeVector vs arg = VS.unsafeWith vs $ \ vsBuf ->
  c_buildAndExecute vsBuf (VS.length vs) arg

ptrToInt :: Ptr a -> Int
ptrToInt = (`minusPtr` nullPtr)

main = do
  let vs = VS.fromList $ concat testInstrs
  print vs
  print . ptrToInt =<< executeVector vs nullPtr

type Instr = [Word8]

data Reg
  = Reg {
    regNo :: Int,
    regWidth :: Width,
    regIsExt :: Bool
  }
  deriving (Eq, Ord)

instance Show Reg where
  show (Reg i W64 False) = words "rax rcx rdx rbx rsp rbp rsi rdi" !! i
  show (Reg i W64 True) = words "r8 r9 r10 r11 r12 r13 r14 r15" !! i
  show (Reg i W32 False) = words "eax ecx edx ebx esp ebp esi edi" !! i
  show (Reg i W16 False) = words "ax cx dx bx sp bp si di" !! i
  show (Reg i W8 False) = words "al cl dl bl ah ch dh bh" !! i

[rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi]
  = map (\ i -> Reg i W64 False) [0..7]

[r8,  r9,  r10, r11, r12, r13, r14, r15]
  = map (\ i -> Reg i W64 True) [0..7]

[eax, ecx, edx, ebx, esp, ebp, esi, edi]
  = map (\ i -> Reg i W32 False) [0..7]

[ax,   cx,  dx,  bx,  sp,  bp,  si,  di]
  = map (\ i -> Reg i W16 False) [0..7]

[al,   cl,  dl,  bl,  ah,  ch,  dh,  bh]
  = map (\ i -> Reg i W8 False) [0..7]

-- XXX
encodeReg = regNo

data Mem
  = Mem {
    memBase :: Maybe Reg,
    memIndex :: Reg,
    memScale :: Maybe Width,
    memDisp :: Int
  }
  deriving (Show, Eq, Ord)

memToSIB (Mem {..}) = SIB memScale memIndex memBase

data Width
  = W8
  | W16
  | W32
  | W64
  deriving (Show, Eq, Ord, Enum)

encodeWidth = fromEnum

data REX
  = REX {
    rexW :: Bool,
    rexR :: Bool,
    rexX :: Bool,
    rexB :: Bool
  }
  deriving (Show, Eq)

defREX = REX False False False False

data ModRM
  = ModRM {
    modrmMod :: Mod,
    modrmReg :: Reg,
    modrmRM :: Reg
  }
  deriving (Show, Eq, Ord)

defModRM = ModRM DirectAddr rax rax

encodeModRM :: ModRM -> Word8
encodeModRM (ModRM {..}) = fromIntegral
  (encodeMod modrmMod `shiftL` 6 +
   encodeReg modrmReg `shiftL` 3 +
   encodeReg modrmRM)

data Mod
  = IndirectAddr
  | IndirectAddr8
  | IndirectAddr32
  | DirectAddr
  deriving (Show, Eq, Ord, Enum)

data SIB
  = SIB {
    sibScale :: Maybe Width,
    sibIndex :: Reg,
    sibBase :: Maybe Reg
  }
  deriving (Show, Eq, Ord)

encodeSIB :: SIB -> Word8
encodeSIB (SIB {..}) = fromIntegral
  (maybe 0 encodeWidth sibScale `shiftL` 6 +
   encodeReg sibIndex `shiftL` 3 +
   maybe 0 encodeReg sibBase)

encodeMod :: Mod -> Int
encodeMod = fromEnum

encodeREX :: REX -> Word8
encodeREX (REX w r x b) = encodeBits [True, False, False, w, r, x, b]

encodeBits = sum . zipWith combine [0..] . reverse
 where
  combine ix True = setBit 0 ix
  combine _ _ = 0

encodeImm32 :: Int -> Instr
encodeImm32 = B.unpack . S.runPut . S.putWord32le . fromIntegral

-- Instr
-- XXX: Consider a final tagless style? Will it work?

testInstrs =
  [ movi32r 100 rdi
  , movi32r 10 rsi
  , lea (Mem (Just rdi) rsi (Just W16) 1000) rax
  , ret
  ]

-- r/m/16/32/64 imm/15/32
instrI32R64 :: Word8 -> Int -> Reg -> Instr
instrI32R64 opcode imm dst = 
  [ encodeREX (defREX { rexW = True })
  , opcode
  , encodeModRM (defModRM { modrmMod = DirectAddr, modrmRM = dst })
  ] ++ encodeImm32 imm

instrR64R64 :: Word8 -> Reg -> Reg -> Instr
instrR64R64 opcode src dst =
  [ encodeREX (defREX { rexW = True })
  , opcode -- MOV r/m, r; 0x8B is MOV r, r/m. The r/m is always the dst.
  , encodeModRM (defModRM { modrmMod = DirectAddr
                          , modrmReg = src
                          , modrmRM = dst
                          })
  ]

-- 0x89 is MOV r/m, r; 0x8B is MOV r, r/m. The r/m is always the dst.
movrr :: Reg -> Reg -> Instr
movrr = instrR64R64 0x89

-- i.e., a flipped version
movrr2 src dst = instrR64R64 0x8B dst src

addrr = instrR64R64 0x01
-- This works as well
addrr2 src dst = instrR64R64 0x03 dst src

movi32r = instrI32R64 0xC7

addi32r = instrI32R64 0x81

ret :: Instr
ret = [0xC3]

-- r16/32/64 m
-- lea %dst, disp(%maybeBase, %index, maybeScale)
-- dst = disp + maybeBase + index * maybeScale
lea :: Mem -> Reg -> Instr
lea mem dst =
  [ encodeREX (defREX { rexW = True })
  , 0x8D
  , encodeModRM (defModRM { modrmMod = IndirectAddr32
                          , modrmReg = dst
                          , modrmRM = rsp -- Enable SIB
                          })
  , encodeSIB (memToSIB mem)
  ] ++ encodeImm32 (memDisp mem)

