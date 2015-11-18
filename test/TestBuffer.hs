{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import Halive.Utils

-- data AppState = AppState 
--     { _appTextBuffer :: Buffer
--     }
-- makeLenses ''AppState
-- newAppState :: AppState
-- newAppState = AppState { _appLines = mempty }

fontFile :: FilePath
fontFile = "freetype-gl/fonts/Vera.ttf"
-- fontFile = "freetype-gl/fonts/Lobster-Regular.ttf"
-- fontFile = "freetype-gl/fonts/LuckiestGuy.ttf"
-- fontFile = "freetype-gl/fonts/SourceCodePro-Regular.ttf"


main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Tiny Rick" 1024 768

    glyphProg <- createShaderProgram "test/glyph.vert" "test/glyph.frag"
    font      <- createFont fontFile 30 glyphProg

    glClearColor 1 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    text <- readFile "test/TestBuffer.hs"
    -- let initialState = newAppState { _appTextBuffer = bufferFromString text }
    let initialState = textBufferFromString "test/TestBuffer.hs" text
    void . flip runStateT initialState . whileWindow win $ 
        mainLoop win events font 


mainLoop :: (MonadState TextBuffer m, MonadIO m) => Window -> Events -> Font -> m ()
mainLoop win events font = do
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    projection44 <- getWindowProjection win 45 0.01 1000
    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    processEvents events $ \e -> do
        closeOnEscape win e

        superIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftSuper
        shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
        if 
            | superIsDown ->
                onKeyDown e Key'S $ do
                    liftIO $ putStrLn "Saving..."
                    bufferString <- gets stringFromTextBuffer
                    liftIO $ writeFile "test/TestBuffer.hs" bufferString
            | shiftIsDown -> do
                onKeyDown e Key'Left  $ id %= selectLeft
                onKeyDown e Key'Right $ id %= selectRight
            | otherwise -> do
                onChar e         $ \char -> id %= insertChar char
                onKeyDown e Key'Backspace $ id %= backspace
                onKeyDown e Key'Enter     $ id %= insertChar '\n'
                onKeyDown e Key'Left      $ id %= moveLeft
                onKeyDown e Key'Right     $ id %= moveRight
                onKeyDown e Key'Down      $ id %= moveDown
                onKeyDown e Key'Up        $ id %= moveUp
        
        updateIndicesAndOffsets =<< use id

    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        -- Render our scene
        let view44       = viewMatrixFromPose newPose
            model44      = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 (-2) (2) (-4))
            mvp          = projection44 !*! view44 !*! model44

        buffer <- get
        renderText font (bufText buffer) (bufSelection buffer) mvp
        
        swapBuffers win



