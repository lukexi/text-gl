{-# LANGUAGE RecordWildCards #-}
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.GL.Pal
import Graphics.GL.Freetype

import Data.Bits
import Control.Monad
import System.Random
import Linear

import SetupGLFW

-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

resX, resY :: Num a => a
resX = 1920
resY = 1080



main :: IO a
main = do

    win <- setupGLFW "Freetype-GL" resX resY

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    
    fontGlyphs <- makeGlyphs "freetype-gl/fonts/Vera.ttf" 50 glyphQuadProg asciiChars

    -- Scene rendering setup
    -- cubeProg <- createShaderProgram "test/cube.vert" "test/cube.frag"
    
    -- cube <- makeCube cubeProg

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    forever $ 
        mainLoop win fontGlyphs 


mainLoop :: GLFW.Window -> FontGlyphs -> IO ()
mainLoop win fontGlyphs = do
    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Clear the framebuffer
    glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let xOffset = 0
        -- (-xOffset/2)

    -- Render our scene
    let projection = perspective 45 (resX/resY) 0.01 1000
        model      = mkTransformation 1 (V3 (realToFrac xOffset) 0 (-4))
        view       = lookAt (V3 0 2 500) (V3 0 0 (-4)) (V3 0 1 0)
        mvp        = projection !*! view !*! model
        (x,y,w,h)  = (0,0,1920,1080)

    glViewport x y w h

    -- renderCube cube mvp
    frameChars <- replicateM 10 $ randomRIO (' ','~')
    renderText fontGlyphs frameChars mvp
    
    GLFW.swapBuffers win



