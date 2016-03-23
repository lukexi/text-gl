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

import Halive.FileListener

data FileWatchMode = WatchFile | NoWatchFile deriving (Eq, Show)

-- | Recognize certain control characters and react to them appropriately
isBackspaceChar :: Char -> Bool
isBackspaceChar = (== 8) . ord

-- | Must call refreshTextRendererFromFile
textRendererFromFile :: MonadIO m => Font -> FilePath -> FileWatchMode -> m TextRenderer
textRendererFromFile font filePath watchMode = liftIO $ do
    text <- readFile filePath
    textRenderer <- createTextRenderer font (textBufferWithPath filePath text)

    case watchMode of
        NoWatchFile -> return textRenderer
        WatchFile -> do
            fileWatcher <- eventListenerForFile filePath
            return $ textRenderer & txrFileWatcher ?~ fileWatcher

-- | Must pass WatchFile to textRendererFromFile to use this
refreshTextRendererFromFile :: forall s m. (MonadState s m, MonadIO m) 
                            => Traversal' s TextRenderer -> m ()
refreshTextRendererFromFile rendererLens = do
    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer
    mTextRenderer <- preuse rendererLens
    forM_ mTextRenderer $ \textRenderer -> 
        forM_ (textRenderer ^. txrFileWatcher) $ \fileWatcher -> 
            forM_ (bufPath (textRenderer ^. txrTextBuffer)) $ \filePath -> 
                onFileEvent fileWatcher $ do
                    text <- liftIO $ readFile filePath
                    textBufferLens %= setTextFromString text

                    updateTextBufferMetricsWithLens rendererLens

updateTextBufferMetricsWithLens :: forall s m. (MonadState s m, MonadIO m) 
                                 => Traversal' s TextRenderer -> m ()
updateTextBufferMetricsWithLens rendererLens = do
    mTextRenderer <- preuse rendererLens
    forM_ mTextRenderer $ \textRenderer -> do
        newRenderer <- updateMetrics textRenderer
        rendererLens .= newRenderer

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
        --(commandKey, commandModKey, optionModKey) = (Key'LeftSuper, ModKeySuper, ModKeyAlt) -- Mac
        (commandKey, commandModKey, optionModKey) = (Key'LeftControl, ModKeyControl, ModKeyAlt) -- Windows

    commandIsDown <- (== KeyState'Pressed) <$> getKey win commandKey
    -- shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
    if  | commandIsDown -> do
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
            onKey  e Key'Enter     $ textBufferLens %= carriageReturn
            onKey  e Key'Backspace $ textBufferLens %= backspace

            onKey  e Key'Left      $ textBufferLens %= moveLeft
            onKey  e Key'Right     $ textBufferLens %= moveRight
            onKey  e Key'Down      $ textBufferLens %= moveDown
            onKey  e Key'Up        $ textBufferLens %= moveUp

            onKeyWithMods e [optionModKey]   Key'Left  $ textBufferLens %= moveWordLeft
            onKeyWithMods e [optionModKey]   Key'Right $ textBufferLens %= moveWordRight

            onKeyWithMods e [optionModKey, ModKeyShift]   Key'Right $ 
                textBufferLens %= selectWordRight
            onKeyWithMods e [optionModKey, ModKeyShift]   Key'Left $ 
                textBufferLens %= selectWordLeft

            onKeyWithMods e [ModKeyShift] Key'Left  $ textBufferLens %= selectLeft
            onKeyWithMods e [ModKeyShift] Key'Right $ textBufferLens %= selectRight
            onKeyWithMods e [ModKeyShift] Key'Up    $ textBufferLens %= selectUp
            onKeyWithMods e [ModKeyShift] Key'Down  $ textBufferLens %= selectDown

    -- Continuously save the file
    let updateBuffer save = do
            updateTextBufferMetricsWithLens rendererLens
            when save $ do
                maybeRenderer <- preuse rendererLens
                forM_ maybeRenderer $ \renderer -> do
                    saveTextBuffer (renderer ^. txrTextBuffer)

    onChar e                          $ \_ -> updateBuffer True
    onKey  e Key'Enter                      $ updateBuffer True
    onKey  e Key'Backspace                  $ updateBuffer True
    onKey  e Key'Up                         $ updateBuffer False
    onKey  e Key'Down                       $ updateBuffer False
    onKey  e Key'Left                       $ updateBuffer False
    onKey  e Key'Right                      $ updateBuffer False
    
    onKeyWithMods e [optionModKey]   Key'Left  $ updateBuffer False
    onKeyWithMods e [optionModKey]   Key'Right $ updateBuffer False
    onKeyWithMods e [optionModKey, ModKeyShift]   Key'Left  $ updateBuffer False
    onKeyWithMods e [optionModKey, ModKeyShift]   Key'Right $ updateBuffer False

    onKeyWithMods e [ModKeyShift] Key'Left  $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Right $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Up    $ updateBuffer False
    onKeyWithMods e [ModKeyShift] Key'Down  $ updateBuffer False
    onKeyWithMods e [commandModKey] Key'Z     $ updateBuffer True
    onKeyWithMods e [commandModKey] Key'V     $ updateBuffer True
    onKeyWithMods e [commandModKey] Key'X     $ updateBuffer True

    
