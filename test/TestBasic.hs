
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Control.Monad.State
import Control.Lens.Extra
import System.Random
import Halive.Utils

-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Freetype-GL" 1024 768

    glyphProg <- createShaderProgram "test/glyph.vert" "test/glyph.frag"
    font      <- createFont "test/SourceCodePro-Regular.ttf" 50 glyphProg

    glClearColor 0 0.1 0.1 1
    -- glEnable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    glGetErrors

    let frameChars = asciiChars ++ ['\n'] ++ asciiChars
    textRenderer <- createTextRenderer font (textBufferFromString frameChars)

    void . flip runStateT textRenderer . whileWindow win $ 
        mainLoop win events 


mainLoop :: (MonadIO m, MonadState TextRenderer m) => Window -> Events -> m ()
mainLoop win events = do
    --glGetErrors
    -- Get mouse/keyboard/OS events from GLFW
    es <- gatherEvents events
    forM_ es $ closeOnEscape win

    -- Update the viewport and projection
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projection44 <- getWindowProjection win 45 0.01 1000

    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Create our model view projection matrix
    let textPos      = V3 0 0 (-1)
        model44      = mkTransformation 1 textPos
        view44       = lookAt (V3 0 0 0) textPos (V3 0 1 0)
        projView44   = projection44 !*! view44 

    -- Render random characters
    n <- liftIO $ randomRIO (1,50)
    frameChars <- liftIO $ fmap concat $ replicateM n $ do
        line <- replicateM 50 $ randomRIO (' ','~')
        return $ line ++ ['\n']

    txrTextBuffer .= textBufferFromString frameChars
    put =<< updateMetrics =<< get
    
    textRenderer <- use id
    renderText textRenderer projView44 model44
    
    swapBuffers win

