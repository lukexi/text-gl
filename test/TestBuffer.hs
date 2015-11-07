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

resX, resY :: Num a => a
resX = 1024
resY = 768

-- data AppState = AppState 
--     { _appTextBuffer :: Buffer
--     }
-- makeLenses ''AppState
-- newAppState :: AppState
-- newAppState = AppState { _appLines = mempty }

fontFile :: FilePath
-- fontFile = "freetype-gl/fonts/Vera.ttf"
-- fontFile = "freetype-gl/fonts/Lobster-Regular.ttf"
-- fontFile = "freetype-gl/fonts/LuckiestGuy.ttf"
fontFile = "freetype-gl/fonts/SourceCodePro-Regular.ttf"


main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Tiny Rick" resX resY

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    font          <- makeGlyphs fontFile 30 glyphQuadProg

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

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
        if superIsDown
            then 
                onKeyDown Key'S e $ do
                    liftIO $ putStrLn "Saving..."
                    bufferString <- gets stringFromBuffer
                    liftIO $ writeFile "test/TestBuffer.hs" bufferString
            else do
                case e of
                    Character char -> insertChar char
                    _ -> return ()
                onKeyDown Key'Backspace e $ 
                    backspace
                onKeyDown Key'Enter e $ do
                    insertChar '\n'

    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        glEnable GL_BLEND
        glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

        -- Render our scene
        let view44       = viewMatrixFromPose newPose
            model44      = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 (-2) (2) (-4))
                                !*! scaleMatrix 0.003
            mvp = projection44 !*! view44 !*! model44

        string <- gets stringFromBuffer
        renderText font string mvp
        
        swapBuffers win



