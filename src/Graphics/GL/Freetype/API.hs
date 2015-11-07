{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.GL.Freetype.API where
import Foreign
import Foreign.C
import Control.Monad.Trans

newtype TextureAtlas = TextureAtlas (Ptr TextureAtlas)
newtype FontPtr      = FontPtr      (Ptr FontPtr)
newtype Glyph        = Glyph        (Ptr Glyph)

data BitDepth = BitDepth1 -- Regular 'alpha-channel-only' atlas
              | BitDepth3 -- Subpixel RGB atlas

rawBitDepth :: Num a => BitDepth -> a
rawBitDepth BitDepth1 = 1
rawBitDepth BitDepth3 = 3

foreign import ccall "texture_atlas_new"
    texture_atlas_new :: CInt -> CInt -> CInt -> IO TextureAtlas

newTextureAtlas :: Int -> Int -> BitDepth -> IO TextureAtlas
newTextureAtlas width height bitDepth = 
    texture_atlas_new 
        (fromIntegral width)
        (fromIntegral height)
        (rawBitDepth bitDepth)

foreign import ccall "get_atlas_texture_id"
    get_atlas_texture_id :: TextureAtlas -> CUInt

atlasTextureID :: Num a => TextureAtlas -> a
atlasTextureID = fromIntegral . get_atlas_texture_id

foreign import ccall "texture_font_new_from_file"
    texture_font_new_from_file :: TextureAtlas -> CFloat -> Ptr CChar -> IO FontPtr

newFontFromFile :: TextureAtlas -> Float -> String -> IO FontPtr
newFontFromFile textureAtlas pointSize fileName = 
    withCString fileName $ \fileNamePtr ->
        texture_font_new_from_file 
            textureAtlas 
            (realToFrac pointSize) 
            fileNamePtr

foreign import ccall "texture_font_load_glyphs"
    texture_font_load_glyphs :: FontPtr -> Ptr CWchar -> IO CSize

loadFontGlyphs :: FontPtr -> String -> IO CSize
loadFontGlyphs font glyphs =
    withCWString glyphs $ \glyphsPtr ->
        texture_font_load_glyphs font glyphsPtr

foreign import ccall "texture_font_get_glyph"
    texture_font_get_glyph :: FontPtr -> CWchar -> IO Glyph

getGlyph :: MonadIO m => FontPtr -> Char -> m Glyph
getGlyph font char = liftIO $
    withCWString [char] $ \charPtr -> do
        cwchar <- peek charPtr
        texture_font_get_glyph font cwchar

foreign import ccall "texture_glyph_get_kerning"
    texture_glyph_get_kerning :: Glyph -> CWchar -> IO CFloat

-- | Gets the kerning between two glyphs — e.g if rendering "Hi", 
-- pass the glyph for i along with 'H',
-- and add the returned offset to i's position
getGlyphKerning :: MonadIO m => Glyph -> Char -> m Float
getGlyphKerning glyph char = liftIO $ 
    withCWString [char] $ \charPtr -> do
        cwchar <- peek charPtr
        realToFrac <$> texture_glyph_get_kerning glyph cwchar

foreign import ccall "get_glyph_metrics"
    get_glyph_metrics :: Glyph -> IO (Ptr CFloat)

data GlyphMetrics = GlyphMetrics
    { gmOffsetX  :: Float
    , gmOffsetY  :: Float
    , gmWidth    :: Float
    , gmHeight   :: Float
    , gmS0       :: Float
    , gmT0       :: Float
    , gmS1       :: Float
    , gmT1       :: Float
    , gmAdvanceX :: Float
    } deriving Show

getGlyphMetrics :: Glyph -> IO GlyphMetrics
getGlyphMetrics glyph = do
    glyphMetricsPtr <- get_glyph_metrics glyph
    [offsetX, offsetY, width, height, s0, t0, s1, t1, advanceX] <- fmap realToFrac <$> peekArray 9 glyphMetricsPtr
    free glyphMetricsPtr
    return (GlyphMetrics offsetX offsetY width height s0 t0 s1 t1 advanceX)
