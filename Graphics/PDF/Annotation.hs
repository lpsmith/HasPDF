---------------------------------------------------------
-- |
-- Copyright   : (c) alpha 2007
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Annotations
---------------------------------------------------------

module Graphics.PDF.Annotation(
   -- * Annotations
   -- ** Types
     TextAnnotation(..)
   , URLLink(..)
   , PDFLink(..)
   , TextIcon(..)
   -- ** Functions
   , newAnnotation
 ) where

import Data.List (foldl')
import Graphics.PDF.Coordinates
import Graphics.PDF.Shapes
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import qualified Data.Map as M
import Graphics.PDF.Action
import Graphics.PDF.Pages
import Control.Monad.State(gets)
--import Debug.Trace

data TextIcon
   = Note
   | Paragraph
   | NewParagraph
   | Key
   | Comment
   | Help
   | Insert
     deriving(Eq,Show)


data TextAnnotation = TextAnnotation
   !PDFString -- ^ Content
   !Rectangle -- ^ Rect
   !TextIcon
data URLLink = URLLink
  !PDFString -- ^ Content
  !Rectangle -- ^ Rect
  !String -- ^ URL
  !Bool -- ^ Border
data PDFLink = PDFLink
  !PDFString -- ^ Content
  !Rectangle -- ^ Rect
  !(PDFReference PDFPage) -- ^ Page
  !Point
  !Bool -- ^ Border
--data Screen = Screen (PDFReference Rendition) PDFString [PDFFloat] (PDFReference PDFPage) (Maybe (PDFReference ControlMedia)) (Maybe (PDFReference ControlMedia))

applyMatrixToRectangle :: Matrix -> Rectangle -> Rectangle
applyMatrixToRectangle m (Rectangle (xa :+ ya) (xb :+ yb)) =
    let (xa'  :+ ya' ) = m `transform` (xa :+ ya)
        (xa'' :+ yb' ) = m `transform` (xa :+ yb)
        (xb'  :+ ya'') = m `transform` (xb :+ ya)
        (xb'' :+ yb'') = m `transform` (xb :+ yb)
        x1 = minimum [xa',xa'',xb',xb'']
        x2 = maximum [xa',xa'',xb',xb'']
        y1 = minimum [ya',ya'',yb',yb'']
        y2 = maximum [ya',ya'',yb',yb'']
    in  Rectangle (x1 :+ y1) (x2 :+ y2)

-- | Get the border shqpe depending on the style
getBorder :: Bool -> [PDFInteger]
getBorder False = [0,0,0]
getBorder True = [0,0,1]

standardAnnotationDict :: AnnotationObject a => a -> [(PDFName,AnyPdfObject)]
standardAnnotationDict a
  = [ (PDFName "Type",AnyPdfObject . PDFName $ "Annot")
    , (PDFName "Subtype",AnyPdfObject $ annotationType a)
    , (PDFName "Rect",AnyPdfObject . map AnyPdfObject $ annotationRect a)
    , (PDFName "Contents",AnyPdfObject $ annotationContent a)
    ]

--instance PdfObject Screen where
--   toPDF a@(Screen _ _ _ p play stop) = toPDF . PDFDictionary . M.fromList $
--        standardAnnotationDict a ++ [(PDFName "P",AnyPdfObject p)]
--                                    ++ (maybe [] (\x -> [(PDFName "A",AnyPdfObject x)]) play)
--                                    ++ (maybe [] (\x -> [(PDFName "AA",AnyPdfObject $ otherActions x)]) stop)
--         where
--             otherActions x = PDFDictionary . M.fromList $ [(PDFName "D",AnyPdfObject x)]
--
--instance AnnotationObject Screen where
--  addAnnotation (Screen video s rect p _ _) = do
--      r <- supply
--      playAction <- addObject $ ControlMedia Play r video
--      stopAction <- addObject $ ControlMedia Stop r video
--      updateObject (PDFReference r) $ Screen video s rect p (Just playAction) (Just playAction)
--      return $ PDFReference r
--  annotationType _ = PDFName "Screen"
--  annotationContent (Screen _ s _ _ _ _) = s
--  annotationRect (Screen _ _ r _ _ _) = r

instance PdfObject TextAnnotation where
      toPDF a@(TextAnnotation _ _ i) = toPDF . PDFDictionary . M.fromList $
           standardAnnotationDict a ++ [(PDFName "Name",AnyPdfObject . PDFName $ show i)]

instance AnnotationObject TextAnnotation where
    addAnnotation = addObject
    annotationType _ = PDFName "Text"
    annotationContent (TextAnnotation s _ _) = s
    annotationRect (TextAnnotation _ (Rectangle (x0 :+ y0) (x1 :+ y1)) _) = [x0,y0,x1,y1]
    annotationToGlobalCoordinates (TextAnnotation a r b) = do
        gr <- transformAnnotRect r
        return $ TextAnnotation a gr b

instance PdfObject URLLink where
    toPDF a@(URLLink _ _ url border) = toPDF . PDFDictionary . M.fromList $
           standardAnnotationDict a ++
            [ (PDFName "A",AnyPdfObject (GoToURL url))
            , (PDFName "Border",AnyPdfObject . map AnyPdfObject $ (getBorder border))
            ]

instance AnnotationObject URLLink where
    addAnnotation = addObject
    annotationType _ = PDFName "Link"
    annotationContent (URLLink s _ _ _) = s
    annotationRect (URLLink _ (Rectangle (x0 :+ y0) (x1 :+ y1)) _ _) = [x0,y0,x1,y1]
    annotationToGlobalCoordinates (URLLink a r b c) = do
        gr <- transformAnnotRect r
        return $ URLLink a gr b c

instance PdfObject PDFLink where
    toPDF a@(PDFLink _ _ page xy border) 
      = toPDF . PDFDictionary . M.fromList $
          standardAnnotationDict a ++
            [ (PDFName "Dest"  , AnyPdfObject dest)
            , (PDFName "Border", AnyPdfObject . map AnyPdfObject $ getBorder border) ]
     where
         dest = [ AnyPdfObject page
                , AnyPdfObject (PDFName "XYZ")
                , AnyPdfObject xy
                , AnyPdfObject (PDFInteger 0)]


instance AnnotationObject PDFLink where
    addAnnotation = addObject
    annotationType _ = PDFName "Link"
    annotationContent (PDFLink s _ _ _ _) = s
    annotationRect (PDFLink _ (Rectangle (x0 :+ y0) (x1 :+ y1)) _ _ _) = [x0,y0,x1,y1]
    annotationToGlobalCoordinates (PDFLink a r b c d) = do
        gr <- transformAnnotRect r
        return $ PDFLink a gr b c d

transformAnnotRect :: Rectangle -> Draw Rectangle
transformAnnotRect r = do
    ms <- gets matrix
    let m = foldl' (*) 1 ms
    return $ m `applyMatrixToRectangle` r

-- | Create a new annotation object
newAnnotation :: (PdfObject a, AnnotationObject a) => a -> Draw ()
newAnnotation annot = do
    annot' <- annotationToGlobalCoordinates annot
    modifyStrict $ \s -> s {annots = (AnyAnnotation annot'):(annots s)}
    return ()
