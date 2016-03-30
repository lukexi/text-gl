{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer

import Control.Lens.Extra
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

    glyphProg <- createShaderProgram "test/glyph.vert" "test/glyph.frag"
    font      <- createFont fontFile 50 glyphProg

    glClearColor 0.1 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    -- let fileName = "test/TestBuffer.hs"
    let fileName = "test/TODO.txt"
    initialState <- textRendererFromFile font fileName WatchFile

    
    void . flip runStateT initialState . whileWindow win $ 
        mainLoop win events


mainLoop :: (MonadState TextRenderer m, MonadIO m) => Window -> Events -> m ()
mainLoop win events = do
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projection44 <- getWindowProjection win 45 0.01 1000
    -- glGetErrors

    let model44 = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 (-1) (1) (-3))
        view44  = viewMatrixFromPose newPose
        mvp     = projection44 !*! view44 !*! model44

    -- Update the text renderer if needed from file changes
    refreshTextRendererFromFile id

    -- Get mouse/keyboard/OS events from GLFW
    processEvents events $ \e -> do
        closeOnEscape win e

        _ <- handleTextBufferEvent win e id
        onMouseDown e $ \_ -> do
            textRenderer <- get
            ray <- cursorPosToWorldRay win projection44 newPose
            case rayToTextRendererCursor ray textRenderer model44 of
                Just cursor -> put =<< beginDrag cursor textRenderer
                Nothing -> return ()
        onCursor e $ \_ _ -> do
            textRenderer <- get
            ray <- cursorPosToWorldRay win projection44 newPose
            case rayToTextRendererCursor ray textRenderer model44 of
                Just cursor -> put =<< continueDrag cursor textRenderer
                Nothing -> return ()

    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        -- Render our scene
        
            

        textRenderer <- get
        renderText textRenderer mvp (V3 1 1 1)
        
        swapBuffers win



