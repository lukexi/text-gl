
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Control.Monad.State
import Control.Monad.Reader
import Control.Lens.Extra
import System.Random
import Halive.Utils


data Uniforms = Uniforms 
  { uMVP   :: UniformLocation (M44 GLfloat) 
  , uColor :: UniformLocation (V4 GLfloat) 
  } deriving Data

-------------------------------------------------------------
-- A test of scaling text to fit a container
-------------------------------------------------------------

frameChars = unlines
    [ "X"
    , "$MALL"
    , "Hello" 
    , "What's" 
    , "Up" 
    , "Doc" 
    --, "Who's~~~~~~~~~~~" 
    --, "The" 
    --, "Best" 
    --, "Around" 
    ]

main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Freetype-GL" 1024 768

    glyphProg <- createShaderProgram "test/glyph.vert" "test/glyph.frag"
    font      <- createFont "freetype-gl/fonts/SourceCodePro-Regular.ttf" 100 glyphProg

    shader     <- createShaderProgram "test/geo.vert" "test/geo.frag"
    planeGeo   <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 5
    planeShape <- makeShape planeGeo shader

    glClearColor 0 0.1 0.1 1
    -- glEnable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    --glGetErrors

    textRenderer <- createTextRenderer font (textBufferFromString frameChars)

    void . flip runStateT textRenderer . whileWindow win $ 
        mainLoop win events planeShape

correctionMatrixForFont' :: Font -> (Int, Int) -> Float -> M44 Float
correctionMatrixForFont' Font{..} (dimX, dimY) finalScale = correctedMVP
  where
    finalScale = 0.1
    (numCharsX, numCharsY) = (realToFrac dimX, realToFrac dimY)
    -- Also scale by the width of a wide character
    charWidthFull = gmAdvanceX (glyMetrics (fntGlyphForChar '_')) * numCharsX
    charHeightFull = fntPointSize
    charHeight = 1 / charHeightFull
    charWidth  = 1 / charWidthFull
    lineSpacingOffset = fntPointSize * 0.15
    centeringOffset = V3 (-charWidthFull/2) (charHeightFull * numCharsY/2 + lineSpacingOffset) 0
    -- Ensures the characters are always the same 
    -- size no matter what point size was specified
    resolutionCompensationScale = realToFrac charHeight
    correctedMVP = scaleMatrix (resolutionCompensationScale * realToFrac finalScale) 
               !*! translateMatrix (centeringOffset)


renderText' :: (MonadIO m) 
           => TextRenderer -> M44 GLfloat -> V3 GLfloat -> m ()
renderText' textRenderer mvp color = do
    let font@Font{..}     = textRenderer ^. txrFont
        TextMetrics{..}   = textRenderer ^. txrTextMetrics
        GlyphUniforms{..} = fntUniforms
        rendererVAO       = textRenderer ^. txrVAO
    useProgram fntShader
    glBindTexture GL_TEXTURE_2D (unTextureID fntTextureID)

    let dims = textSeqDimensions . bufText $ textRenderer ^. txrTextBuffer
    let correctedMVP      = mvp !*! correctionMatrixForFont' font dims 1
                                        

    uniformM44 uMVP     correctedMVP
    uniformI   uTexture 0
    uniformV3  uColor   color

    let numVertices  = 4
        numInstances = fromIntegral txmNumChars
    withVAO rendererVAO $ 
        glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()


mainLoop :: (MonadIO m, MonadState TextRenderer m) => Window -> Events -> Shape Uniforms -> m ()
mainLoop win events planeShape = do
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
        mvpX         = projection44 !*! view44 !*! model44 !*! scaleMatrix (V3 1 0.002 1)
        mvpY         = projection44 !*! view44 !*! model44 !*! scaleMatrix (V3 0.002 1 1)

    
    withShape planeShape $ do
        Uniforms{..} <- asks sUniforms
        uniformV4 uColor (V4 0.2 0.5 0.2 1)
        uniformM44 uMVP mvp
        drawShape
        uniformV4 uColor (V4 0.2 0.0 0.2 1)
        uniformM44 uMVP mvpX
        drawShape
        uniformM44 uMVP mvpY
        drawShape

    -- Leaving this here so we can test updating chars later
    --txrTextBuffer .= textBufferFromString frameChars
    --put =<< updateMetrics =<< get
    
    textRenderer <- use id
    renderText' textRenderer mvp (V3 1 1 1)
    
    swapBuffers win

