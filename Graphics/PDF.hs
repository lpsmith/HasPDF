---------------------------------------------------------
-- |
-- Copyright   : (c) alpheccar 2007
-- License     : BSD-style
--
-- Maintainer  : Leon P Smith <leon@melding-monads.com>
-- Stability   : experimental
-- Portability : portable
--
-- Generation of PDF documents
---------------------------------------------------------
module Graphics.PDF
  (
  -- * HPDF
  -- ** PDF Monad
    PDF
  , runPdf
  -- ** PDF Common Types
  , PDFRect(..)
  , PDFFloat
  , PDFReference
  , PDFString
  , PDFPage
  , Pages
  -- ** Document management
  , module Graphics.PDF.Document
  -- ** Drawing
  , module Graphics.PDF.Shapes
  -- ** Colors
  , module Graphics.PDF.Colors
  -- ** Geometry
  , module Graphics.PDF.Coordinates
  , applyMatrix
  -- ** Text
  , module Graphics.PDF.Text
  -- ** Navigation
  , module Graphics.PDF.Navigation
  -- ** Annotations
  , module Graphics.PDF.Annotation
  -- ** Actions
  , module Graphics.PDF.Action
  -- ** Images
  , module Graphics.PDF.Image
  -- ** Patterns
  , module Graphics.PDF.Pattern
  -- ** Shading
  , module Graphics.PDF.Shading
  -- ** Typesetting
  , module Graphics.PDF.Typesetting
  , module Graphics.PDF.Hyphenate
  ) where

import Graphics.PDF.Hyphenate
import Graphics.PDF.Typesetting
import Graphics.PDF.Shading
import Graphics.PDF.Pattern
import Graphics.PDF.Navigation
import Graphics.PDF.Text
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Int
-- import System.IO
import Text.Printf(printf)
import Control.Monad.State
import Graphics.PDF.Annotation
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Colors
import Graphics.PDF.Shapes
import Graphics.PDF.Coordinates
import Graphics.PDF.Pages
import Graphics.PDF.Document
import Codec.Compression.Zlib
import Graphics.PDF.Action
import Graphics.PDF.Image
import Graphics.PDF.Resources(emptyResource)
import Data.Binary.Builder(Builder,fromLazyByteString, toLazyByteString)
import Graphics.PDF.LowLevel.Serializer
import Data.Monoid

-- | Create a new PDF document and return a first page
-- The page is using the document size by default
createPDF :: PDF ()
createPDF  = do
  -- Create the Proc structure
  --proc <- addObject PDFProc
  -- Create an empty resource
  --addObject $ PDFResource proc
  return ()

-- Create the PDF stream objects from the draw monads
createStreams :: PDF ()
createStreams = do
    ls <- gets streams >>= return . IM.toList
    modifyStrict $ \s -> s {streams = IM.empty}
    mapM_ addStream ls
 where
    addStream (k,(p,(state',w'))) = do
     -- New reference for the stream
     r <- supply
     -- Run the drawing and get the new state (resource, annotation)
     --myBounds <- gets xobjectBound
     --cp <- gets currentPage
     --let (_,state',w') = runDrawing d (emptyEnvironment {streamId = r, xobjectb = myBounds, currentp = maybe Nothing (\(PDFReference x) -> Just x) cp })
     let ref = PDFReference r :: PDFReference PDFLength

     -- Pattern NEEDS a resource entry even if empty otherwise don't work with acrobat reader
     -- Image DON'T want a resource entry if empty otherwise don't work with apple reader
     resources <- if (emptyResource (rsrc state')) && (not (pdfDictMember (PDFName "PatternType") (otherRsrcs state')))
       then do
         case p of
            -- Not linked to a page
            -- otherResource are entries specific to a special stream (like an XObject) so we return empty for a page
            Nothing -> return (otherRsrcs state')
            -- Linked to a page
            Just pageRef -> do
                 setPageAnnotations (annots state') pageRef
                 return emptyDictionary
       -- Some resource are needed by the stream
       else do
         rsrcRef <- addObject (rsrc state')
         case p of
             -- Not linked to a page
             Nothing -> do
                  return $ (otherRsrcs state') `pdfDictUnion` (PDFDictionary . M.fromList  $ [(PDFName "Resources",AnyPdfObject rsrcRef)])
             -- Linked to a page
             Just pageRef -> do
                  setPageAnnotations (annots state') pageRef
                  setPageResource rsrcRef pageRef
                  return emptyDictionary

     infos <- gets docInfo
     -- Resources to add to the stream
     -- We compress only if the stream is not using its own filter
     if (compressed infos) && (not (pdfDictMember (PDFName "Filter") resources))
       then do
         let w''' = compress . toLazyByteString $ w'
             w'' = fromLazyByteString w'''
         updateObject (PDFReference k :: PDFReference PDFStream) (PDFStream w'' True ref resources)
         updateObject ref (PDFLength (B.length w'''))
       else do
         updateObject (PDFReference k :: PDFReference PDFStream) (PDFStream w' False ref resources)
         updateObject ref (PDFLength (B.length . toLazyByteString $ w'))

-- | Save all the pages and streams in the main object dictionary
saveObjects :: PDF (PDFReference PDFCatalog)
saveObjects  = do
  -- Save streams to the object dictionary so that they are saved in the PDF document
  createStreams
  infos <- gets docInfo
  -- Save pages to the object dictionary so that they are saved in the PDF document
  pRef <- addPages
  -- Create outlines object
  o <- gets outline
  oref <- addOutlines o
  -- Create the catalog
  cat <- addObject $ PDFCatalog oref pRef (pageMode infos) (pageLayout infos) (viewerPreferences infos)
  modifyStrict $ \s -> s {catalog = cat}
  gets catalog

-- | The PDFTrailer
#ifndef __HADDOCK__
data PDFTrailer = PDFTrailer
  !Int -- Number of PDF objects in the document
  !(PDFReference PDFCatalog) -- Reference to the PDf catalog
  !(PDFDocumentInfo)
#else
data PDFTrailer
#endif

instance PdfObject PDFTrailer where
   toPDF (PDFTrailer size root infos) = toPDF $ PDFDictionary. M.fromList $
     [ (PDFName "Size",AnyPdfObject . PDFInteger $ size)
     , (PDFName "Root",AnyPdfObject root)
     , (PDFName "Info",AnyPdfObject . PDFDictionary . M.fromList $ allInfos)
     ]
     where
      allInfos = [ (PDFName "Author",AnyPdfObject . author $ infos)
                 , (PDFName "Subject",AnyPdfObject . subject $ infos)
                 , (PDFName "Producer",AnyPdfObject $ toPDFString "HPDF - The Haskell PDF Library" )
                 ]

-- | Write PDF objects in the TOC
writeObjectsAndCreateToc :: [Builder] -- ^ List of objects each object being already converted to a bytestring
                          -> (Int,Int64,[Builder])
writeObjectsAndCreateToc l =
   let lengths =  tail . scanl (\len obj -> len + (B.length . toLazyByteString $ obj)) 0 $ l
       createEntry x = serialize $ (printf "%010d 00000 n \n" ((fromIntegral x)::Integer) :: String)
       entries = map createEntry (init lengths)
   in
   (length l,last lengths,entries)
-- foldr writeObject (0,0::Int64,[]) l where
-- writeObject obj (nb,len,toc) = (nb+1,len + (B.length . toLazyByteString $ obj),(serialize $ (printf "%010d 00000 n \n" ((fromIntegral len)::Integer))) : toc)

defaultPdfSettings :: PdfState
defaultPdfSettings =
  PdfState {
             supplySrc = 1
           , objects = IM.empty
           , pages = noPages
           , streams = IM.empty
           , catalog = PDFReference 0
           , defaultRect = PDFRect 0 0 600 400
           , docInfo = standardDocInfo { author=toPDFString "Unknown", compressed = True}
           , outline = Nothing
           , currentPage = Nothing
           , xobjectBound = IM.empty
           , firstOutline = [True]
           }

createObjectByteStrings :: PdfState -> PDF a -> Builder
createObjectByteStrings pdfState m =
      let header = serialize "%PDF-1.5\n"
          objectEncoding (x,a) = toPDF . PDFReferencedObject (fromIntegral x) $ a
          (root,s) = flip runState pdfState  . unPDF $ createPDF >> m >> saveObjects
          objs = objects s
          objectContents = header : (map objectEncoding . IM.toAscList $ objs)
          (nb,len,toc) = writeObjectsAndCreateToc objectContents
      in
        mconcat$ objectContents ++
                [ serialize "xref\n"
                , serialize $ "0 " ++ show nb ++ "\n"
                , serialize "0000000000 65535 f \n"
                ]
                ++
                toc
                ++
                [ serialize "\ntrailer\n"
                , toPDF $ PDFTrailer nb root (docInfo pdfState)
                , serialize "\nstartxref\n"
                , serialize (show len)
                , serialize "\n%%EOF"
                ]

-- | Generates a PDF document
runPdf :: String -- ^ Name of the PDF document
       -> PDFDocumentInfo
       -> PDFRect -- ^ Default size for a page
       -> PDF a  -- ^ PDF action
       -> IO ()
runPdf filename infos rect m = do
  let content = createObjectByteStrings (defaultPdfSettings {defaultRect = rect, docInfo = infos} ) m
  B.writeFile filename (toLazyByteString content)


