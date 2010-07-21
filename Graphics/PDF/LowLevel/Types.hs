{-# LANGUAGE GeneralizedNewtypeDeriving #-}

---------------------------------------------------------
-- |
-- Copyright   : (c) alpha 2006
-- License     : BSD-style
--
-- Maintainer  : Leon P Smith <leon@melding-monads.com>
-- Stability   : experimental
-- Portability : portable
--
-- Low level stuff
---------------------------------------------------------
-- #hide
module Graphics.PDF.LowLevel.Types where

import Graphics.PDF.Coordinates
import Graphics.PDF.LowLevel.Serializer

import qualified Data.Map as M
import Data.List(intersperse)
import Data.Int
import Control.Monad.State
import Control.Monad.Writer
import Data.Word
import Data.Binary.Builder(Builder,fromByteString)
import qualified Data.ByteString as S
#if __GLASGOW_HASKELL__ >= 608
import qualified Data.ByteString.Lazy.Internal as L(ByteString(..))
#else
import qualified Data.ByteString.Base as L(LazyByteString(..))
#endif

-- | PDF Objects
class PdfObject a where
  toPDF :: a -> Builder

-- | Anonymous PDF object
data AnyPdfObject = forall a . PdfObject a => AnyPdfObject a

instance PdfObject AnyPdfObject where
 toPDF (AnyPdfObject a) = toPDF a

-- | A length in a PDF document
newtype PDFLength = PDFLength Int64 deriving(Eq,Show,Ord,Num)

-- | A real number in a PDF document
type PDFFloat = Double

instance PdfObject Int where
    toPDF a = serialize a

instance PdfObject PDFLength where
    toPDF (PDFLength a) = serialize (show a)

instance PdfObject PDFFloat where
  toPDF a = serialize a

instance PdfObject (Complex PDFFloat) where
  toPDF (x :+ y) = mconcat [ serialize x
                           , serialize ' '
                           , serialize y
                           ]

instance PdfObject Bool where
  toPDF (True) = serialize "true"
  toPDF (False) = serialize "false"

-- | A PDFString containing a strict bytestring
newtype PDFString = PDFString S.ByteString deriving(Eq,Ord,Show)

-- | Create a PDF string from an Haskell one
toPDFString :: String -> PDFString
toPDFString = PDFString . S.pack . map encodeISO88591 -- . escapeString

encodeISO88591 :: Char -> Word8
encodeISO88591 a =
    let c = fromEnum a in
    if c < 32 || c >= 256 then 32 else fromIntegral c

#if __GLASGOW_HASKELL__ >= 608
instance SerializeValue L.ByteString PDFString where
  serialize (PDFString t) = L.Chunk t L.Empty
#else
instance SerializeValue L.LazyByteString PDFString where
  serialize (PDFString t) = L.LPS [t]
#endif

instance SerializeValue Builder PDFString where
  serialize (PDFString t) = fromByteString t

-- | Escape PDF characters which have a special meaning
escapeString :: PDFString -> PDFString
escapeString (PDFString t) = PDFString . S.pack . escapeOnWords8 . S.unpack $ t

pc2w :: Char -> Word8
pc2w = fromIntegral . fromEnum

escapeOnWords8 :: [Word8] -> [Word8]
escapeOnWords8 [] = []
escapeOnWords8 (a:l) | a == pc2w '(' = (pc2w '\\'):(pc2w '('):escapeOnWords8 l
                     | a == pc2w ')' = (pc2w '\\'):(pc2w ')'):escapeOnWords8 l
                     | a == pc2w '\\' = (pc2w '\\'):(pc2w '\\'):escapeOnWords8 l
                     | otherwise = a:escapeOnWords8 l

-- Misc strings useful to build bytestrings

lparen :: SerializeValue s Char => s
lparen = serialize '('

rparen :: SerializeValue s Char => s
rparen = serialize  ')'

lbracket :: SerializeValue s Char => s
lbracket = serialize  '['

rbracket :: SerializeValue s Char => s
rbracket = serialize  ']'

bspace :: SerializeValue s Char => s
bspace = serialize  ' '

blt :: SerializeValue s Char => s
blt = serialize  '<'

bgt :: SerializeValue s Char => s
bgt = serialize  '>'

newline :: SerializeValue s Char => s
newline = serialize  '\n'

noPdfObject :: Monoid s => s
noPdfObject = mempty

instance PdfObject PDFString where
  toPDF a = mconcat [ lparen
                    , serialize . escapeString $ a
                    , rparen
                    ]

-- | A PDFName object
newtype PDFName = PDFName String deriving(Eq,Ord)

instance PdfObject PDFName where
 toPDF (PDFName a) = serialize ("/" ++ a)

-- | A PDFArray
type PDFArray = [AnyPdfObject]

instance PdfObject a => PdfObject [a] where
    toPDF l = mconcat $ (lbracket:intersperse bspace (map toPDF l)) ++ [bspace] ++ [rbracket]

-- | A PDFDictionary

newtype PDFDictionary = PDFDictionary (M.Map PDFName AnyPdfObject)

instance PdfObject PDFDictionary where
  toPDF (PDFDictionary a) = mconcat $ [blt,blt,newline]
                                       ++ [convertLevel a]
                                       ++ [bgt,bgt]
   where
     convertLevel _ = M.foldWithKey convertItem mempty a
        where convertItem key value current
                  = mconcat $ [ toPDF key
                              , bspace
                              , toPDF value
                              , newline
                              , current ]


-- | Am empty dictionary
emptyDictionary :: PDFDictionary
emptyDictionary = PDFDictionary M.empty

isEmptyDictionary :: PDFDictionary -> Bool
isEmptyDictionary (PDFDictionary d) = M.null d

insertInPdfDict :: PDFName -> AnyPdfObject -> PDFDictionary -> PDFDictionary
insertInPdfDict key obj (PDFDictionary d) = PDFDictionary $ M.insert key obj d

pdfDictUnion :: PDFDictionary -> PDFDictionary -> PDFDictionary
pdfDictUnion (PDFDictionary a) (PDFDictionary b) = PDFDictionary $ M.union a b


-- | A PDF rectangle
data Rect = Rect !Point !Point deriving(Eq)

roundInt :: RealFrac a => a -> Int
roundInt = round

instance PdfObject Rect where
  toPDF (Rect (a :+ b) (c :+ d))
    = toPDF . map (AnyPdfObject . roundInt) $ [a,b,c,d]


-- | A Referenced objects
data PDFReferencedObject a = PDFReferencedObject !Int !a

instance PdfObject a => PdfObject (PDFReferencedObject a) where
  toPDF (PDFReferencedObject referenceId obj) =
    mconcat  $ [ serialize . show $ referenceId
               , serialize " 0 obj"
               , newline
               , toPDF obj
               , newline
               , serialize "endobj"
               , newline , newline
               ]

-- | A reference to a PDF object
data PDFReference s = PDFReference !Int deriving(Eq,Ord,Show)

-- | Get the reference value
referenceValue :: PDFReference s -> Int
referenceValue (PDFReference i) = i

instance PdfObject s => Num (PDFReference s) where
  (+) (PDFReference a) (PDFReference b) = PDFReference (a+b)
  (*) (PDFReference a) (PDFReference b) = PDFReference (a*b)
  negate (PDFReference a) = PDFReference (negate a)
  abs (PDFReference a) = PDFReference (abs a)
  signum (PDFReference a) = PDFReference (signum a)
  fromInteger a = PDFReference (fromInteger a)

instance PdfObject s => PdfObject (PDFReference s) where
  toPDF (PDFReference i) = mconcat $ [ serialize . show $ i
                                     , serialize " 0 R"]



instance (PdfObject a,PdfObject b) => PdfObject (Either a b) where
  toPDF (Left a) = toPDF a
  toPDF (Right a) = toPDF a

modifyStrict :: (MonadState s m) => (s -> s) -> m ()
modifyStrict f = do
  	s <- get
  	put $! (f s)

-- | A monad where paths can be created
class MonadWriter Builder m => MonadPath m