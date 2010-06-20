---------------------------------------------------------
-- |
-- Copyright   : (c) alpha 2007
-- License     : BSD-style
--
-- Maintainer  : Leon P Smith <leon@melding-monads.com>
-- Stability   : experimental
-- Portability : portable
--
-- PDF shading
---------------------------------------------------------
module Graphics.PDF.Shading(
  -- * Shading
  -- ** Type
    PDFShading(..)
  , paintWithShading
  , applyShading
 ) where

import Graphics.PDF.Draw
import Graphics.PDF.LowLevel.Types
import Control.Monad.State(gets)
import Graphics.PDF.Shapes(setAsClipPath)
import Control.Monad.Writer
import Graphics.PDF.LowLevel.Serializer
-- import Data.Monoid

-- | Fill clipping region with a shading
applyShading :: PDFShading -> Draw ()
applyShading shade = do
    shadingMap <- gets shadings
    (newName,newMap) <- setResource "Shading" shade shadingMap
    modifyStrict $ \s -> s { shadings = newMap }
    tell . mconcat $[ serialize "\n/"
                    , serialize newName
                    , serialize " sh"
                    ]

paintWithShading :: PDFShading -- ^ Shading
                 -> Draw a -- ^ Shape to paint
                 -> Draw ()
paintWithShading shade d = do
    withNewContext $ do
      _ <- d
      setAsClipPath
      applyShading shade