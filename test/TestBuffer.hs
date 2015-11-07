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
import qualified Data.Sequence as Seq

resX, resY :: Num a => a
resX = 1024
resY = 768

data AppState = AppState 
    { _appLines :: [String]
    , _appCurrLine :: String
    }
makeLenses ''AppState

-- type Buffer = Sequence (Sequence Char)

-- fontFile = "freetype-gl/fonts/Vera.ttf"
-- fontFile = "freetype-gl/fonts/Lobster-Regular.ttf"
-- fontFile = "freetype-gl/fonts/LuckiestGuy.ttf"
fontFile = "freetype-gl/fonts/SourceCodePro-Regular.ttf"

newAppState :: AppState
newAppState = AppState { _appLines = mempty, _appCurrLine = mempty }

main :: IO ()
main = do

    (win, events) <- reacquire 0 $ createWindow "Tiny Rick" resX resY

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    font          <- makeGlyphs fontFile 30 glyphQuadProg

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST
    glDisable GL_DEPTH_TEST

    text <- readFile "test/TestBuffer.hs"
    let initialState = newAppState { _appLines = lines text }
    void . flip runStateT initialState . whileWindow win $ 
        mainLoop win events font 


mainLoop :: (MonadState AppState m, MonadIO m) => Window -> Events -> Font -> m ()
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
                onKeyDown Key'S e $
                    liftIO $ putStrLn "Saving..."
            else do
                case e of
                    Character char -> 
                        appCurrLine <>= [char]
                    _ -> return ()
                onKeyDown Key'Backspace e $ 
                    appCurrLine %= \cs -> if null cs then "" else init cs
                onKeyDown Key'Enter e $ do
                    currLine <- use appCurrLine
                    appLines <>= [currLine]
                    appCurrLine .= ""

    immutably $ do
        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        glEnable GL_BLEND
        glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

        let xOffset = (-0)
            -- (-xOffset/2)

        -- Render our scene
        let view44       = viewMatrixFromPose newPose
            model44      = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 (-2) (2) (-4))
                                !*! scaleMatrix 0.003

        -- renderCube cube mvp
        textLines <- use appLines
        currLine  <- use appCurrLine
        let allLines = textLines ++ [currLine]
        _ <- liftIO $ renderText font allLines (projection44 !*! view44 !*! model44)
        
        swapBuffers win



