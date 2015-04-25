{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.GL.Freetype where
import Foreign
import Foreign.C

newtype TextureAtlas = TextureAtlas (Ptr TextureAtlas)
newtype Font = Font (Ptr Font)

foreign import ccall "texture_atlas_new"
    texture_atlas_new :: CInt -> CInt -> CInt -> IO TextureAtlas

newTextureAtlas :: (Integral a, Integral b, Integral c) => a -> b -> c -> IO TextureAtlas
newTextureAtlas width height bitDepth = 
    texture_atlas_new 
        (fromIntegral width)
        (fromIntegral height)
        (fromIntegral bitDepth)

foreign import ccall "texture_font_new_from_file"
    texture_font_new_from_file :: TextureAtlas -> CFloat -> Ptr CChar -> IO Font

newFontFromFile :: Real a => TextureAtlas -> a -> String -> IO Font
newFontFromFile textureAtlas pointSize fileName = 
    withCString fileName $ \fileNamePtr ->
        texture_font_new_from_file 
            textureAtlas 
            (realToFrac pointSize) 
            fileNamePtr

foreign import ccall "texture_font_new_from_file"
    texture_font_load_glyphs :: Font -> Ptr CChar -> IO ()

loadFontGlyphs :: Font -> String -> IO ()
loadFontGlyphs font glyphs =
    withCString glyphs $ \glyphsPtr ->
        texture_font_load_glyphs font glyphsPtr