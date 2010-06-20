---------------------------------------------------------
-- |
-- Copyright   : (c) alpha 2007
-- License     : BSD-style
--
-- Maintainer  : Leon P Smith <leon@melding-monads.com>
-- Stability   : experimental
-- Portability : portable
--
-- PDF Patterns
---------------------------------------------------------

module Graphics.PDF.Pattern
     ( -- * Pattern
       TilingType(..)
     , PDFColoredPattern
     , PDFUncoloredPattern
     , createColoredTiling
     , createUncoloredTiling
     , setColoredFillPattern
     , setColoredStrokePattern
     , setUncoloredFillPattern
     , setUncoloredStrokePattern
     ) where

import Graphics.PDF.Coordinates
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Resources
import qualified Data.Map as M
import Graphics.PDF.Pages(recordBound,createContent)
import Control.Monad.State
import Control.Monad.Writer
import Graphics.PDF.LowLevel.Serializer
-- import Data.Monoid

data PaintType = ColoredTiling
               | UncoloredTiling
               deriving(Eq,Enum)

-- | Tiling type
data TilingType = ConstantSpacing
                | NoDistortion
                | ConstantSpacingAndFaster
                deriving(Eq,Enum)

-- | Create a colored tiling pattern
createColoredTiling :: Point    -- ^ Left, Bottom
                    -> Point    -- ^ Right, Top
                    -> Point    -- ^ Horizontal step, Vertical step
                    -> TilingType
                    -> Draw a -- ^ Drawing commands
                    -> PDF (PDFReference PDFColoredPattern)
createColoredTiling  x y step tt d =  createTilingPattern x y step ColoredTiling tt d  >>= return . PDFReference

-- | Create an uncolored tiling pattern
createUncoloredTiling :: Point    -- ^ Left, Bottom
                      -> Point    -- ^ Right, Top
                      -> Point    -- ^ Horizontal step, Vertical step
                      -> TilingType
                      -> Draw a -- ^ Drawing commands
                      -> PDF (PDFReference PDFUncoloredPattern)
createUncoloredTiling x y step tt d =  createTilingPattern x y step UncoloredTiling tt d >>= return . PDFReference

-- | Create a PDF tiling pattern
createTilingPattern :: Point    -- ^ Left, Bottom
                    -> Point    -- ^ Right, Top
                    -> Point    -- ^ Horizontal step, Vertical step
                    -> PaintType
                    -> TilingType
                    -> Draw a -- ^ Drawing commands
                    -> PDF Int
createTilingPattern (xa :+ ya) (xb :+ yb) (hstep :+ vstep) pt tt d =
    let a' = do modifyStrict $ \s ->
                  s {otherRsrcs = PDFDictionary. M.fromList $
                       [ (PDFName "Type",AnyPdfObject . PDFName $ "Pattern")
                       , (PDFName "PatternType",AnyPdfObject . PDFInteger $ 1)
                       , (PDFName "PaintType",AnyPdfObject . PDFInteger $ (fromEnum pt) + 1)
                       , (PDFName "TilingType",AnyPdfObject . PDFInteger $ (fromEnum tt) + 1)
                       , (PDFName "Matrix",AnyPdfObject . (map (AnyPdfObject . PDFInteger)) $ [1,0,0,1,0,0])
                       , (PDFName "BBox",AnyPdfObject . map AnyPdfObject  $ [xa,ya,xb,yb])
                       , (PDFName "XStep",AnyPdfObject hstep)
                       , (PDFName "YStep",AnyPdfObject vstep)
                       ]
                    }
                d
   in do
       PDFReference s <- createContent a' Nothing
       recordBound s (xb-xa) (yb-ya)
       return s


-- | Set the fill pattern
setColoredFillPattern :: PDFReference PDFColoredPattern -> Draw ()
setColoredFillPattern (PDFReference a) = do
     patternMap <- gets patterns
     (newName,newMap) <- setResource "Pattern" (PDFReference a) patternMap
     modifyStrict $ \s -> s { patterns = newMap }
     tell . serialize $ ("\n/Pattern cs")
     tell . mconcat $ [ serialize "\n/"
                      , serialize newName
                      , serialize " scn"
                      ]

-- | Set the stroke pattern
setColoredStrokePattern :: PDFReference PDFColoredPattern -> Draw ()
setColoredStrokePattern (PDFReference a) = do
  patternMap <- gets patterns
  (newName,newMap) <- setResource "Pattern" (PDFReference a) patternMap
  modifyStrict $ \s -> s { patterns = newMap }
  tell . serialize $ ("\n/Pattern CS")
  tell . mconcat $ [ serialize "\n/"
                   , serialize newName
                   , serialize " SCN"
                   ]

-- | Set the fill pattern
setUncoloredFillPattern :: PDFReference PDFUncoloredPattern -> Color -> Draw ()
setUncoloredFillPattern (PDFReference a) col = do
       let (r,g,b) = getRgbColor col
       colorMap <- gets colorSpaces
       (newColorName,_) <- setResource "ColorSpace" PatternRGB colorMap
       patternMap <- gets patterns
       (newName,newMap) <- setResource "Pattern" (PDFReference a) patternMap
       modifyStrict $ \s -> s { patterns = newMap }
       tell . mconcat $ [ serialize "\n/"
                        , serialize newColorName
                        , serialize " cs"
                        ]
       tell . mconcat $ [ serialize '\n'
                        , toPDF r
                        , serialize ' '
                        , toPDF g
                        , serialize ' '
                        , toPDF b
                        , serialize ' '
                        , serialize " /"
                        , serialize newName
                        , serialize " scn"
                        ]

-- | Set the stroke pattern
setUncoloredStrokePattern :: PDFReference PDFUncoloredPattern -> Color -> Draw ()
setUncoloredStrokePattern (PDFReference a) col = do
    let (r,g,b) = getRgbColor col
    colorMap <- gets colorSpaces
    (newColorName,_) <- setResource "ColorSpace" PatternRGB colorMap
    patternMap <- gets patterns
    (newName,newMap) <- setResource "Pattern" (PDFReference a) patternMap
    modifyStrict $ \s -> s { patterns = newMap }
    tell . mconcat $ [ serialize "\n/"
                     , serialize newColorName
                     , serialize " CS"
                     ]
    tell . mconcat $ [ serialize '\n'
                     , toPDF r
                     , serialize ' '
                     , toPDF g
                     , serialize ' '
                     , toPDF b
                     , serialize ' '
                     , serialize " /"
                     , serialize newName
                     , serialize " SCN"
                     ]