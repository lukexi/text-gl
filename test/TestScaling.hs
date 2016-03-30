
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
-- A test of scaling text to fit a container
-------------------------------------------------------------

frameChars = "Hello My Dolly" 

main :: IO ()
main = do

    print "HELLOTHERE"
    (win, events) <- reacquire 0 $ createWindow "Freetype-GL" 1024 768

    glyphProg <- createShaderProgram "test/glyph.vert" "test/glyph.frag"
    font      <- createFont "freetype-gl/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    glClearColor 0 0.1 0.1 1
    -- glEnable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    glGetErrors

    let frameChars = asciiChars ++ ['\n'] ++ asciiChars
    textRenderer <- createTextRenderer font (textBufferFromString frameChars)

    void . flip runStateT textRenderer . whileWindow win $ 
        mainLoop win events 

correctionMatrixForFont' :: Fractional a => Font -> M44 a
correctionMatrixForFont' Font{..} = correctedMVP
  where
    -- Ensures the characters are always the same 
    -- size no matter what point size was specified
    resolutionCompensationScale = realToFrac (1 / fntPointSize / charWidth)
    -- Also scale by the width of a wide character
    charWidth = gmAdvanceX (glyMetrics (fntGlyphForChar '_'))
    correctedMVP = translateMatrix (V3 (-0.5) (0.5) 0) 
               !*! scaleMatrix resolutionCompensationScale


renderText' :: (MonadIO m) 
           => TextRenderer -> M44 GLfloat -> V3 GLfloat -> m ()
renderText' textRenderer mvp color = do
    let font@Font{..}     = textRenderer ^. txrFont
        TextMetrics{..}   = textRenderer ^. txrTextMetrics
        GlyphUniforms{..} = fntUniforms
        rendererVAO       = textRenderer ^. txrVAO
    useProgram fntShader
    glBindTexture GL_TEXTURE_2D (unTextureID fntTextureID)

    let correctedMVP      = mvp !*! correctionMatrixForFont' font

    uniformM44 uMVP     correctedMVP
    uniformI   uTexture 0
    uniformV3  uColor   color

    let numVertices  = 4
        numInstances = fromIntegral txmNumChars
    withVAO rendererVAO $ 
        glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()


mainLoop :: (MonadIO m, MonadState TextRenderer m) => Window -> Events -> m ()
mainLoop win events = do
    --glGetErrors
    -- Get mouse/keyboard/OS events from GLFW
    processEvents events $ closeOnEscape win

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
        mvp          = projection44 !*! view44 !*! model44

    
    -- Leaving this here so we can test updating chars later
    txrTextBuffer .= textBufferFromString frameChars
    put =<< updateMetrics =<< get
    
    textRenderer <- use id
    renderText textRenderer mvp (V3 1 1 1)
    
    swapBuffers win

