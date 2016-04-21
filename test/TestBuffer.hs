{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Control.Monad
import Control.Monad.State
import Halive.Utils

fontFile :: FilePath
-- fontFile = "freetype-gl/fonts/Vera.ttf"
-- fontFile = "freetype-gl/fonts/Lobster-Regular.ttf"
-- fontFile = "freetype-gl/fonts/LuckiestGuy.ttf"
fontFile = "freetype-gl/fonts/SourceCodePro-Regular.ttf"


main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Tiny Rick" 1024 768

    glyphProg     <- createShaderProgram "test/glyph.vert" "test/glyph.frag"
    font          <- createFont fontFile 50 glyphProg
    
    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    -- let fileName = "test/TestBuffer.hs"
    let fileName = "TODO.txt"
    initialState <- textRendererFromFile font fileName WatchFile

    
    void . flip runStateT initialState $ do
        editTextRendererBuffer id $ moveTo (Cursor 0 0)
        whileWindow win $ mainLoop win events


mainLoop :: (MonadState TextRenderer m, MonadIO m) => Window -> Events -> m ()
mainLoop win events = do
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projM44 <- getWindowProjection win 45 0.01 1000
    -- glGetErrors

    let modelM44 = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 0 0 (-1))
                        !*! scaleMatrix (1/50) -- ~50 characters per GL unit
        viewM44  = viewMatrixFromPose newPose
        projViewM44 = projM44 !*! viewM44

    -- Update the text renderer if needed from file changes
    refreshTextRendererFromFile id

    -- Get mouse/keyboard/OS events from GLFW
    es <- gatherEvents events
    forM_ es $ \e -> do
        closeOnEscape win e

        _ <- handleTextBufferEvent win e id
        _ <- handleTextBufferMouseEvent win e id projM44 modelM44 newPose
        return ()

    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    -- Render our scene
    textRenderer <- get
    renderText textRenderer projViewM44 modelM44
    
    swapBuffers win



