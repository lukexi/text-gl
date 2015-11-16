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

import TextBuffer

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

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    font          <- createFont fontFile 30 glyphQuadProg

    glClearColor 1 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    glEnable    GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    text <- readFile "test/TestBuffer.hs"
    -- let initialState = newAppState { _appTextBuffer = bufferFromString text }
    let initialState = bufferFromString text
    void . flip runStateT initialState . whileWindow win $ 
        mainLoop win events font 


mainLoop :: (MonadState Buffer m, MonadIO m) => Window -> Events -> Font -> m ()
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
                    bufferString <- gets stringFromBuffer
                    liftIO $ writeFile "test/TestBuffer.hs" bufferString
            | shiftIsDown -> do
                onKeyDown e Key'Left  $ selectLeft
                onKeyDown e Key'Right $ selectRight
            | otherwise -> do
                case e of
                    Character char -> insertChar char
                    _ -> return ()
                onKeyDown e Key'Backspace $ backspace
                onKeyDown e Key'Enter     $ insertChar '\n'
                onKeyDown e Key'Left      $ moveLeft
                onKeyDown e Key'Right     $ moveRight
                onKeyDown e Key'Down      $ moveDown
                onKeyDown e Key'Up        $ moveUp

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



