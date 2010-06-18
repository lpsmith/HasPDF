---------------------------------------------------------
-- |
-- Copyright   : (c) alpha 2007
-- License     : BSD-style
--
-- Maintainer  : Leon P Smith <leon@melding-monads.com>
-- Stability   : experimental
-- Portability : portable
--
-- PDF Images
---------------------------------------------------------

module Graphics.PDF.Image(
   -- * Images
   -- ** Types
     PDFJpeg
   , JpegFile
   -- ** Functions
   , createPDFJpeg
   , readJpegFile
   , jpegBounds
 ) where
     
import Graphics.PDF.LowLevel.Types
import qualified Data.Map as M
import Graphics.PDF.Draw
import Graphics.PDF.Resources
import Graphics.PDF.Pages
import Graphics.PDF.Document
import qualified Data.ByteString.Lazy as B
import Control.Monad.Writer
#if __GLASGOW_HASKELL__ >= 608
import System.IO hiding(withFile)
#else
import System.IO
#endif
import Data.Char(ord)
import Data.Bits
import Control.Monad.Error  
import Graphics.PDF.Coordinates
import Data.Binary.Builder(Builder,fromLazyByteString)
import Control.Exception as E

#if __GLASGOW_HASKELL__ >= 610
import Control.OldException(ioErrors)
#endif

m_sof0 :: Int
m_sof0 = 0xc0 
m_sof1 :: Int 
m_sof1 = 0xc1 
--m_sof2 :: Int 
--m_sof2 = 0xc2  
m_sof3 :: Int 
m_sof3 = 0xc3  
m_sof5 :: Int 
m_sof5 = 0xc5 
m_sof6 :: Int  
m_sof6 = 0xc6 
m_sof7 :: Int  
m_sof7 = 0xc7  
--m_jpg :: Int 
--m_jpg = 0xc8  
m_sof9 :: Int  
m_sof9 = 0xc9  
m_sof10 :: Int 
m_sof10 = 0xca
m_sof11 :: Int 
m_sof11 = 0xcb
m_sof13 :: Int 
m_sof13 = 0xcd 
m_sof14 :: Int 
m_sof14 = 0xce 
m_sof15 :: Int 
m_sof15 = 0xcf 
--m_dht :: Int 
--m_dht = 0xc4   
--m_dac :: Int 
--m_dac = 0xcc   
m_rst0 :: Int              
m_rst0 = 0xd0  
m_rst1 :: Int 
m_rst1 = 0xd1 
m_rst2 :: Int  
m_rst2 = 0xd2  
m_rst3 :: Int 
m_rst3 = 0xd3
m_rst4 :: Int   
m_rst4 = 0xd4 
m_rst5 :: Int  
m_rst5 = 0xd5
m_rst6 :: Int   
m_rst6 = 0xd6 
m_rst7 :: Int  
m_rst7 = 0xd7  
m_soi :: Int 
m_soi = 0xd8 
m_eoi :: Int   
m_eoi = 0xd9 
--m_sos :: Int   
--m_sos = 0xda
--m_dqt :: Int    
--m_dqt = 0xdb 
--m_dnl :: Int   
--m_dnl = 0xdc
--m_dri :: Int    
--m_dri = 0xdd
--m_dhp :: Int    
--m_dhp = 0xde
--m_exp :: Int    
--m_exp = 0xdf
--m_app0 :: Int    
--m_app0 = 0xe0  
--m_app1 :: Int 
--m_app1 = 0xe1  
--m_app2 :: Int 
--m_app2 = 0xe2  
--m_app3 :: Int 
--m_app3 = 0xe3  
--m_app4 :: Int 
--m_app4 = 0xe4 
--m_app5 :: Int  
--m_app5 = 0xe5  
--m_app6 :: Int 
--m_app6 = 0xe6  
--m_app7 :: Int 
--m_app7 = 0xe7 
--m_app8 :: Int  
--m_app8 = 0xe8  
--m_app9 :: Int 
--m_app9 = 0xe9  
--m_app10 :: Int 
--m_app10 = 0xea
--m_app11 :: Int 
--m_app11 = 0xeb
--m_app12 :: Int 
--m_app12 = 0xec
--m_app13 :: Int 
--m_app13 = 0xed
--m_app14 :: Int 
--m_app14 = 0xee
--m_app15 :: Int 
--m_app15 = 0xef
--m_jpg0 :: Int 
--m_jpg0 = 0xf0 
--m_jpg13 :: Int 
--m_jpg13 = 0xfd
--m_com :: Int 
--m_com = 0xfe
m_tem :: Int 
m_tem = 0x01 
--m_error :: Int 
--m_error = 0x100 
   
io :: IO a -> FA a 
io = FA . liftIO

-- | File analyzer monad
newtype FA a = FA {unFA :: ErrorT String IO a} 
#ifndef __HADDOCK__
  deriving(Monad,MonadError String,Functor)
#else
instance Monad FA
instance MonadError String FA
instance MonadIO FA
instance Functor FA
#endif
    
runFA :: FA a -> IO (Either String a)
runFA = runErrorT . unFA

readWord16 :: Handle -> FA Int
readWord16 h = io $ do
    hi <- hGetChar h
    lo <- hGetChar h
    return $ ((fromEnum hi) `shiftL` 8) .|. (fromEnum . ord $ lo)

readWord8 :: Handle -> FA Int
readWord8 h = io $ do
    lo <- hGetChar h
    return $ fromEnum . ord $ lo
            
--optional :: FA (Maybe a) -> FA (Maybe a)
--optional a = a --`catchError` (\e -> return Nothing)

--jfif :: Handle -> FA (Maybe (Double,Double))
--jfif h = do
--     header <- readWord16 h
--     when (header /= 0x0FFE0) $ throwError (strMsg "No JFIF magic number")
--     readWord16 h
--     mapM_ check "JFIF"
--     readWord16 h
--     unit <- readWord8 h
--     width <- fromIntegral `fmap` readWord16 h
--     height <- fromIntegral `fmap` readWord16 h
--     case unit of
--         1 -> return $ Just (width,height)
--         2 -> return $ Just (width*2.54,height*2.54)
--         _ -> return $ Just (0,0)
--    where
--     check c' = do
--         c <- io $ hGetChar h
--         when (c /= c') $ throwError (strMsg "No JFIF header")
           
parseJpegContent :: Handle -> FA (Int,Int,Int,Int)
parseJpegContent h = do
    r <- readWord8 h
    when (r /=  0x0FF) $ throwError (strMsg "No marker found")
    sof <- readWord8 h
    case sof of
        a | a `elem` [m_sof5,m_sof6,m_sof7,m_sof9,m_sof10,m_sof11,m_sof13,m_sof14,m_sof15] -> throwError (strMsg "Unuspported compression mode")
          | a `elem` [m_sof0,m_sof1,m_sof3] -> do
              readWord16 h
              bits_per_component <- readWord8 h
              height <- readWord16 h
              width <- readWord16 h
              color_space  <- readWord8 h
              return (bits_per_component,height,width,color_space)                  
          | a `elem` [m_soi,m_eoi,m_tem,m_rst0,m_rst1,m_rst2,m_rst3,m_rst4,m_rst5,m_rst6,m_rst7] -> parseJpegContent h
          | otherwise -> do
               l <- readWord16 h
               io $ hSeek h RelativeSeek (fromIntegral (l-2))
               parseJpegContent h

analyzeJpeg :: Handle -> FA (Int,PDFFloat,PDFFloat,Int)
analyzeJpeg h = do
    -- Get Length
    io $ hSeek h SeekFromEnd 0
    --fileLength <- io $ hTell h
    io $ hSeek h AbsoluteSeek 0
    -- Check jpeg
    header <- readWord16 h
    when (header /= 0x0FFD8) $ throwError (strMsg "Not a JPEG File")
   
    -- Extract resolution from jfif
    --res <- optional $ jfif h
    
    io $ hSeek h AbsoluteSeek 0
    
    (bits_per_component,height,width,color_space) <- parseJpegContent h
    --io $ print fileLength
    --io $ print res
    --io $ print bits_per_component
    --io $ print height
    --io $ print width
    --io $ print color_space
    --io $ hClose h
    unless (color_space `elem` [1,3,4]) $ throwError (strMsg "Color space not supported")
    return (bits_per_component,(fromIntegral height),(fromIntegral width),color_space)
    
--test = analyzePng "Test/logo.png"
    
withFile :: String -> (Handle -> IO c) -> IO c    
withFile name = bracket (openBinaryFile name ReadMode) hClose

-- | Read a JPEG file and return an abstract description of its content or an error
-- The read is not lazy. The whole image will be loaded into memory
readJpegFile :: FilePath
             -> IO (Either String JpegFile)
readJpegFile f = catchJust ioErrors (do
     r <- liftIO $ withFile f $ \h -> do
             runFA (analyzeJpeg h)
     case r of
         Right (bits_per_component,height,width,color_space) -> do
                 img <- liftIO $ withFile f $ \h' -> do
                     nb <- hFileSize h'
                     B.hGet h' (fromIntegral nb)
                 return (Right $ JpegFile bits_per_component width height color_space (fromLazyByteString img))
         Left s -> return $ Left s) (\err -> return $ Left (show err)) 

-- | Get the JPEG bounds
jpegBounds :: JpegFile -> (PDFFloat,PDFFloat)
jpegBounds (JpegFile _ w h _ _) = (w,h)

-- | Use an abstract description of a Jpeg to return a PDFReference that can be used to manipulate the Jpeg in the context
-- of the PDF document
createPDFJpeg :: JpegFile  
              -> PDF (PDFReference PDFJpeg)
createPDFJpeg (JpegFile bits_per_component width height color_space img) = do
        PDFReference s <- createContent a' Nothing  
        recordBound s width height
        return (PDFReference s) 
    where
       color c = case c of
           1 -> [(PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceGray")]
           3 -> [(PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceRGB")]
           4 -> [(PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceCMYK")
                ,(PDFName "Decode",AnyPdfObject . map (AnyPdfObject . PDFInteger) $ [1,0,1,0,1,0,1,0])
                ]
           _ -> error "Jpeg color space not supported"
       a' = 
                 do modifyStrict $ \s -> s  {otherRsrcs = PDFDictionary. M.fromList $ 
                                                   [ (PDFName "Type",AnyPdfObject . PDFName $ "XObject")
                                                   , (PDFName "Subtype",AnyPdfObject . PDFName $ "Image")
                                                   , (PDFName "Width",AnyPdfObject . PDFInteger $ round width)
                                                   , (PDFName "Height",AnyPdfObject . PDFInteger $ round height)
                                                   , (PDFName "BitsPerComponent",AnyPdfObject . PDFInteger $ bits_per_component)
                                                   , (PDFName "Interpolate", AnyPdfObject True)
                                                   , (PDFName "Filter",AnyPdfObject . PDFName $ "DCTDecode")
                                                   ] ++ color color_space
                                             }
                    tell img        
              
-- | A Jpeg file   
data JpegFile = JpegFile !Int !PDFFloat !PDFFloat !Int !Builder
     
-- | A Jpeg PDF object
data PDFJpeg
instance PDFXObject PDFJpeg where
    drawXObject a = withNewContext $ do
            (width,height) <- bounds a
            applyMatrix (scale (width :+ height))
            privateDrawXObject a
        
instance PdfObject PDFJpeg where
  toPDF _ = noPdfObject
instance PdfResourceObject (PDFReference PDFJpeg) where
  toRsrc = AnyPdfObject