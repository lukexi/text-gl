{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Freetype

import Control.Lens
import Control.Monad
import Control.Monad.State
import Halive.Utils
-- import System.Random
-- import Linear



-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

resX, resY :: Num a => a
resX = 1024
resY = 768

-- TODO use some zippers here!
data AppState = AppState 
    { _appLines :: [String]
    , _appCurrLine :: String
    }
makeLenses ''AppState

newAppState :: AppState
newAppState = AppState { _appLines = mempty, _appCurrLine = mempty }

main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Freetype-GL" resX resY

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    font          <- makeGlyphs "freetype-gl/fonts/Vera.ttf" 50 glyphQuadProg

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    void . flip runStateT newAppState . whileWindow win $ 
        mainLoop win events font 


mainLoop :: (MonadState AppState m, MonadIO m) => Window -> Events -> Font -> m ()
mainLoop win events font = do
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h
    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    processEvents events $ \e -> do
        closeOnEscape win e

        case e of
            Character char -> 
                appCurrLine <>= [char]
            _ -> return ()
        onKeyDown Key'Backspace e $ appCurrLine %= \cs -> if null cs then "" else init cs
        onKeyDown Key'Enter e $ do
            currLine <- use appCurrLine
            appLines <>= [currLine]
            appCurrLine .= ""

    -- Clear the framebuffer
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let xOffset = (-250)
        -- (-xOffset/2)

    -- Render our scene
    let projection44 = perspective 45 (fromIntegral w/fromIntegral h) 0.01 1000
        view44       = lookAt (V3 0 0 500) (V3 0 0 (-4)) (V3 0 1 0)
        model44      = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 xOffset 0 (-4))

    -- renderCube cube mvp
    textLines <- use appLines
    currLine  <- use appCurrLine
    let allLines = textLines ++ [currLine]
    _ <- liftIO $ renderText font allLines (projection44 !*! view44 !*! model44)
    
    swapBuffers win



