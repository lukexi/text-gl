{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Halive.Utils

data Uniforms = Uniforms 
  { uMVP   :: UniformLocation (M44 GLfloat) 
  , uColor :: UniformLocation (V4 GLfloat) 
  } deriving Data


fontFile :: FilePath
fontFile = "freetype-gl/fonts/SourceCodePro-Regular.ttf"


main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Tiny Rick" 1024 768

    glyphProg     <- createShaderProgram "test/glyph.vert" "test/glyph.frag"
    font          <- createFont fontFile 50 glyphProg

    let fontCharWidthPoints = gmAdvanceX (glyMetrics (fntGlyphForChar font $ '_')) / (fntPointSize font)

    shader     <- createShaderProgram "test/geo.vert" "test/geo.frag"
    planeGeo   <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 5
    planeShape <- makeShape planeGeo shader
    
    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    -- let fileName = "test/TestBuffer.hs"
    let fileName = "TODO.txt"
    initialState <- textRendererFromFile font fileName WatchFile

    
    void . flip runStateT initialState $ do
        editTextRendererBuffer id $ moveTo (Cursor 0 0)
        whileWindow win $ 
            mainLoop win events planeShape fontCharWidthPoints

handleEvents win events projM44 modelM44 = do
    -- Update the text renderer if needed from file changes
    refreshTextRendererFromFile id

    -- Get mouse/keyboard/OS events from GLFW
    es <- gatherEvents events
    forM_ es $ \e -> do
        closeOnEscape win e

        _ <- handleTextBufferEvent win e id
        _ <- handleTextBufferMouseEvent win e id projM44 modelM44 newPose
        return ()


--mainLoop :: (MonadState TextRenderer m, MonadIO m) => Window -> Events -> m ()
mainLoop win events planeShape fontCharWidthPoints = do
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projM44 <- getWindowProjection win 45 0.01 1000
    -- glGetErrors


    let modelM44 = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 0 0 (-1))
        viewM44  = viewMatrixFromPose newPose
        projViewM44 = projM44 !*! viewM44

    textRenderer <- get
    
    let textBuffer = textRenderer ^. txrTextBuffer
        numLinesVisible = 50
        (cols, lines) = bufDims textBuffer 
        (_, Cursor cursLine cursCol) = getSelection textBuffer 
        numLines = numLinesVisible
        scrollLines = fromIntegral cursLine
        --scrollColumns = -fromIntegral cursCol + 0
        scrollColumns = fromIntegral cols/5
        scrollM44 = translateMatrix (V3 (scrollColumns * fontCharWidthPoints) scrollLines 0.01)
        
        textModelM44 = modelM44 !*! scaleMatrix (recip (fromIntegral numLines)) !*! scrollM44

    handleEvents win events projM44 textModelM44
    

    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Render our scene
    --glDisable GL_DEPTH_TEST
    glEnable GL_STENCIL_TEST
    glClear GL_STENCIL_BUFFER_BIT           -- Clear stencil buffer  (0 by default)
    glStencilOp GL_KEEP GL_KEEP GL_REPLACE  -- sfail dpfail dppfail

    -- Draw background
    glStencilFunc GL_ALWAYS 1 0xFF          -- Set any stencil to 1
    glStencilMask 0xFF                      -- Write to stencil buffer
    
    withShape planeShape $ do
        Uniforms{..} <- asks sUniforms
        uniformV4 uColor (V4 0.01 0.02 0.05 1)
        uniformM44 uMVP (projViewM44 !*! modelM44)
        drawShape

    -- Draw clipped thing
    glStencilFunc GL_EQUAL 1 0xFF -- Pass test if stencil value is 1
    glStencilMask 0x00            -- Don't write anything to stencil buffer

    get >>= \textRenderer -> 
        renderText textRenderer projViewM44 textModelM44
    
    glDisable GL_STENCIL_TEST
    --glEnable GL_DEPTH_TEST
    swapBuffers win



