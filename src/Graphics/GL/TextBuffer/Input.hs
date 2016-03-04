{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.GL.TextBuffer.Input where

import Graphics.UI.GLFW.Pal

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State

import Graphics.GL.Freetype
import Graphics.GL.TextBuffer.Types
import Graphics.GL.TextBuffer.TextBuffer
import Graphics.GL.TextBuffer.Render
import Data.Char

-- | Recognize certain control characters and react to them appropriately
isBackspaceChar :: Char -> Bool
isBackspaceChar = (== 8) . ord

textRendererFromFile :: MonadIO m => Font -> FilePath -> m TextRenderer
textRendererFromFile font filePath = liftIO $ do
    text <- readFile filePath
    createTextRenderer font (textBufferWithPath filePath text)

saveTextBuffer :: MonadIO m => TextBuffer -> m ()
saveTextBuffer buffer = liftIO $ case bufPath buffer of
    Nothing -> putStrLn "Tried to save text buffer with no path"
    Just bufferPath -> do
        putStrLn $ "Saving " ++ bufferPath ++ "..."
        writeFile bufferPath (stringFromTextBuffer buffer)

handleTextBufferEvent :: forall s m. (MonadState s m, MonadIO m) 
                      => Window -> Event -> Traversal' s TextRenderer -> m ()
handleTextBufferEvent win e rendererLens = do
    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer
    -- let bufferLens = rendererLens . txrTextBuffer :: (Traversal' s TextBuffer)
    superIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftSuper
    -- shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
    if  | superIsDown -> do
            onKeyDown e Key'S      $ maybe (return ()) saveTextBuffer =<< preuse textBufferLens
            onKeyDown e Key'C      $ do
                mTextBuffer <- preuse textBufferLens
                forM_ mTextBuffer $ \buffer -> 
                    setClipboardString win (selectionFromTextBuffer buffer)
            onKeyDown e Key'X      $ do
                mTextBuffer <- preuse textBufferLens
                forM_ mTextBuffer $ \buffer -> 
                    setClipboardString win (selectionFromTextBuffer buffer)
                textBufferLens %= backspace
            onKeyDown e Key'V      $ do
                mString <- getClipboardString win
                forM_ mString $ \string -> 
                    textBufferLens %= insertString string
            onKeyDown e Key'Z      $ 
                textBufferLens %= undo
        | otherwise -> do

            onChar e $ \case 
                (isBackspaceChar -> True) -> textBufferLens %= backspace
                char                      -> textBufferLens %= insertChar char
            onKey  e Key'Enter     $ textBufferLens %= insertChar '\n'
            onKey  e Key'Backspace $ textBufferLens %= backspace

            onKey  e Key'Left      $ textBufferLens %= moveLeft
            onKey  e Key'Right     $ textBufferLens %= moveRight
            onKey  e Key'Down      $ textBufferLens %= moveDown
            onKey  e Key'Up        $ textBufferLens %= moveUp

            onKeyWithMods e [ModKeyAlt]   Key'Left  $ textBufferLens %= moveWordLeft
            onKeyWithMods e [ModKeyAlt]   Key'Right $ textBufferLens %= moveWordRight

            onKeyWithMods e [ModKeyAlt, ModKeyShift]   Key'Right $ 
                textBufferLens %= selectWordRight
            onKeyWithMods e [ModKeyAlt, ModKeyShift]   Key'Left $ 
                textBufferLens %= selectWordLeft

            onKeyWithMods e [ModKeyShift] Key'Left  $ textBufferLens %= selectLeft
            onKeyWithMods e [ModKeyShift] Key'Right $ textBufferLens %= selectRight
            onKeyWithMods e [ModKeyShift] Key'Up    $ textBufferLens %= selectUp
            onKeyWithMods e [ModKeyShift] Key'Down  $ textBufferLens %= selectDown

    -- Continuously save the file
    let updateBuffer save = do
            maybeRenderer <- preuse rendererLens
            forM_ maybeRenderer $ \renderer -> do
                newRenderer <- updateMetrics renderer
                rendererLens .= newRenderer
                when save $ saveTextBuffer (newRenderer ^. txrTextBuffer)
            -- buf <- preuse textBufferLens
            -- liftIO . print $ join $ bufSelection <$> buf

    onChar e                          $ \_ -> updateBuffer True
    onKey  e Key'Enter                      $ updateBuffer True
    onKey  e Key'Backspace                  $ updateBuffer True
    onKey  e Key'Up                         $ updateBuffer False
    onKey  e Key'Down                       $ updateBuffer False
    onKey  e Key'Left                       $ updateBuffer False
    onKey  e Key'Right                      $ updateBuffer False
    
    onKeyWithMods e [ModKeyAlt]   Key'Left  $ updateBuffer False
    onKeyWithMods e [ModKeyAlt]   Key'Right $ updateBuffer False
    onKeyWithMods e [ModKeyAlt, ModKeyShift]   Key'Left  $ updateBuffer False
    onKeyWithMods e [ModKeyAlt, ModKeyShift]   Key'Right $ updateBuffer False

    onKeyWithMods e [ModKeyShift] Key'Left  $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Right $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Up    $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Down  $ updateBuffer False
    onKeyWithMods e [ModKeySuper] Key'Z     $ updateBuffer True
    onKeyWithMods e [ModKeySuper] Key'V     $ updateBuffer True
    onKeyWithMods e [ModKeySuper] Key'X     $ updateBuffer True

    
