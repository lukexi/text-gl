{-# LANGUAGE RecordWildCards #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL.Freetype

import Control.Monad
import System.Random

-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

main :: IO a
main = do

    (win, events) <- createWindow "Freetype-GL" 1024 768

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    font          <- makeGlyphs "freetype-gl/fonts/Vera.ttf" 50 glyphQuadProg

    glClearColor 0 0.1 0.1 1
    -- glEnable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    forever $ 
        mainLoop win events font 


mainLoop :: Window -> Events -> Font -> IO ()
mainLoop win events font = do
    -- Get mouse/keyboard/OS events from GLFW
    processEvents events $ closeOnEscape win

    -- Update the viewport and projection
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projection44 <- getWindowProjection win 45 0.01 1000

    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Create our model view projection matrix
    let model44      = mkTransformation 1 (V3 (-1) 0 (-4))
                        !*! scaleMatrix 0.005
        view44       = lookAt (V3 0 2 0) (V3 0 0 (-4)) (V3 0 1 0)
        mvp          = projection44 !*! view44 !*! model44

    -- Render random characters
    frameChars <- replicateM 10 $ randomRIO (' ','~')
    renderText font frameChars (-1,3) mvp
    
    swapBuffers win

