{-# LANGUAGE RecordWildCards #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL.Freetype

import Control.Monad
-- import System.Random
import Halive.Utils

-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

main :: IO a
main = do

    (win, events) <- reacquire 0 $ createWindow "Freetype-GL" 1024 768

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    font          <- createFont "freetype-gl/fonts/SourceCodePro-Regular.ttf" 50 glyphQuadProg

    glClearColor 0 0.1 0.1 1
    -- glEnable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    glGetErrors
    forever $ 
        mainLoop win events font 


mainLoop :: Window -> Events -> Font -> IO ()
mainLoop win events font = do
    glGetErrors
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
        view44       = lookAt (V3 0 2 0) (V3 0 0 (-4)) (V3 0 1 0)
        mvp          = projection44 !*! view44 !*! model44

    -- Render random characters
    -- frameChars <- replicateM 10 $ randomRIO (' ','~')
    let frameChars = asciiChars ++ ['\n'] ++ asciiChars
    renderText font frameChars (-1,3) mvp
    
    swapBuffers win

