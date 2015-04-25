{-# LANGUAGE RecordWildCards #-}
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.GL.Freetype

import Data.Bits
import Control.Monad
import Linear

import SetupGLFW
import ShaderLoader
-- import Cube
import GlyphQuad

-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

main :: IO a
main = do

    let (resX, resY) = (1920, 1080)

    win <- setupGLFW "Freetype-GL" resX resY

    -- Test Freetype
    atlas <- newTextureAtlas 512 512 1
    -- font  <- newFontFromFile atlas 100 "freetype-gl/fonts/SourceSansPro-Regular.ttf"
    font  <- newFontFromFile atlas 100 "freetype-gl/fonts/Vera.ttf"

    missed <- loadFontGlyphs font "A Quick Brown Fox Jumps Over The Lazy Dog 0123456789"
    putStrLn $ "Missed: " ++ show missed

    let textureID = TextureID (atlasTextureID atlas)

    a <- getGlyph font 'Q'
    gms@GlyphMetrics{..} <- getGlyphMetrics a
    -- print gms

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    quad <- makeGlyphQuad glyphQuadProg textureID (gmS0, gmT0, gmS1, gmT1)

    -- Scene rendering setup
    -- cubeProg <- createShaderProgram "test/cube.vert" "test/cube.frag"
    
    -- cube <- makeCube cubeProg

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    forever $ 
        mainLoop win quad


mainLoop :: GLFW.Window -> GlyphQuad -> IO ()
mainLoop win glyphQuad = do
    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Clear the framebuffer
    glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    -- Render our scene
    let projection = perspective 45 (1920/1080) 0.01 1000
        model      = mkTransformation 1 (V3 0 0 (-4))
        view       = lookAt (V3 0 2 0) (V3 0 0 (-4)) (V3 0 1 0)
        mvp        = projection !*! view !*! model
        (x,y,w,h)  = (0,0,1920,1080)

    glViewport x y w h

    -- renderCube cube mvp
    renderGlyphQuad glyphQuad mvp
    
    GLFW.swapBuffers win



